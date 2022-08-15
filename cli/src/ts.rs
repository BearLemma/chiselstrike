use crate::chisel::{
    type_msg::TypeEnum, AddTypeRequest, ContainerType, FieldDefinition, StructField, StructType,
    TypeMsg,
};
use anyhow::{anyhow, bail, ensure, Context, Result};
use chisel_server::auth::is_auth_entity_name;
use std::fmt;
use std::path::Path;
use swc_common::sync::Lrc;
use swc_common::{
    errors::{emitter, Handler},
    SourceMap, Spanned,
};
use swc_ecma_ast::PropName;
use swc_ecma_ast::{
    ClassMember, ClassProp, Decl, Decorator, Expr, Ident, Lit, ModuleDecl, ModuleItem,
    TsEntityName, TsKeywordTypeKind, TsType, TsTypeAnn,
};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};
use swc_ecmascript::ast::{
    self as swc_ecma_ast, ClassDecl, TsKeywordType, TsTypeAliasDecl, TsTypeElement,
};
use swc_ecmascript::parser as swc_ecma_parser;

impl FieldDefinition {
    pub(crate) fn field_type(&self) -> Result<&TypeEnum> {
        self.field_type
            .as_ref()
            .with_context(|| format!("field_type of field '{}' is None", self.name))?
            .type_enum
            .as_ref()
            .with_context(|| format!("type_enum of field '{}' is None", self.name))
    }
}

impl ContainerType {
    fn value_type(&self) -> Result<&TypeEnum> {
        self.value_type
            .as_ref()
            .context("value_type of ContainerType is None")?
            .type_enum
            .as_ref()
            .context("type_enum of value_type of ContainerType is None")
    }
}

impl TypeEnum {
    fn array(inner: TypeEnum) -> Self {
        let inner = TypeMsg {
            type_enum: Some(inner),
        };
        TypeEnum::Array(Box::new(ContainerType {
            value_type: Some(Box::new(inner)),
        }))
    }
}

impl fmt::Display for TypeEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeEnum::String(_) => f.write_str("string"),
            TypeEnum::Number(_) => f.write_str("number"),
            TypeEnum::Bool(_) => f.write_str("bool"),
            TypeEnum::Entity(name) => name.fmt(f),
            TypeEnum::Array(inner) => {
                let inner = inner.value_type().unwrap();
                write!(f, "Array<{inner}>")
            }
            TypeEnum::Struct(user_struct) => {
                write!(f, "{} {{ .. }}", user_struct.name)
            }
        }
    }
}

impl From<TypeEnum> for TypeMsg {
    fn from(type_enum: TypeEnum) -> Self {
        TypeMsg {
            type_enum: Some(type_enum),
        }
    }
}

fn swc_err<S: Spanned>(handler: &Handler, s: S, msg: &str) -> anyhow::Error {
    handler.span_err(s.span(), msg);
    anyhow!("{}", msg)
}

fn get_ident_string(handler: &Handler, x: &Expr) -> Result<String> {
    match x {
        Expr::Ident(id) => Ok(ident_to_string(id)),
        z => Err(swc_err(handler, z, "expected an identifier")),
    }
}

fn ident_to_string(id: &Ident) -> String {
    id.sym.to_string()
}

fn get_field_info(handler: &Handler, x: &PropName) -> Result<(String, bool)> {
    match x {
        PropName::Ident(id) => Ok((ident_to_string(id), id.optional)),
        z => Err(swc_err(handler, z, "expected an identifier")),
    }
}

fn map_keyword_type(handler: &Handler, kw: &TsKeywordType) -> Result<DeclType> {
    match kw.kind {
        TsKeywordTypeKind::TsStringKeyword => Ok(DeclType::String),
        TsKeywordTypeKind::TsNumberKeyword => Ok(DeclType::Number),
        TsKeywordTypeKind::TsBooleanKeyword => Ok(DeclType::Bool),
        _ => Err(swc_err(handler, kw, "type keyword not supported")),
    }
}

fn map_array_type(handler: &Handler, x: &TsType) -> Result<DeclType> {
    match x {
        TsType::TsArrayType(array_type) => match &*array_type.elem_type {
            TsType::TsKeywordType(kw) => map_keyword_type(handler, &kw),
            TsType::TsArrayType(_) => map_array_type(handler, &*array_type.elem_type),
            _ => Err(swc_err(
                handler,
                x,
                "only arrays of primitive types are supported",
            )),
        }
        .map(|e| DeclType::Array(Box::new(e))),
        _ => panic!("trying to map as array a type which is not an array"),
    }
}

fn map_type(handler: &Handler, x: &TsType) -> Result<DeclType> {
    match x {
        TsType::TsKeywordType(kw) => map_keyword_type(handler, &kw),
        TsType::TsTypeRef(tr) => match &tr.type_name {
            TsEntityName::Ident(id) => {
                let ident_name = ident_to_string(id);
                Ok(DeclType::TypeRef(ident_name))
            }
            TsEntityName::TsQualifiedName(_) => Err(anyhow!("qualified names are not supported")),
        },
        TsType::TsArrayType(_) => map_array_type(handler, x),
        t => Err(swc_err(handler, t, "type is not supported")),
    }
}

fn get_field_type(handler: &Handler, x: &Option<TsTypeAnn>) -> Result<DeclType> {
    let t = x
        .clone()
        .context("type annotation is temporarily mandatory")?;
    map_type(handler, &t.type_ann)
}

fn parse_literal(handler: &Handler, x: &Lit) -> Result<(String, DeclType)> {
    let r = match x {
        Lit::Str(x) => (x.value.to_string(), DeclType::String),
        Lit::Bool(x) => (x.value.to_string(), DeclType::Bool),
        Lit::Num(x) => (x.value.to_string(), DeclType::Number),
        x => anyhow::bail!(swc_err(handler, x, "literal not supported")),
    };
    Ok(r)
}

fn get_field_value(handler: &Handler, x: &Option<Box<Expr>>) -> Result<Option<(String, DeclType)>> {
    match x {
        None => Ok(None),
        Some(k) => match &**k {
            Expr::Lit(k) => {
                let (val, val_type) = parse_literal(handler, k)?;
                Ok(Some((val, val_type)))
            }
            Expr::Unary(k) => {
                let op = k.op;
                let value = get_field_value(handler, &Some(k.arg.clone()))?
                    .ok_or_else(|| swc_err(handler, k, "unexpected empty expression"))?;
                Ok(Some((format!("{}{}", op, value.0), value.1)))
            }
            // If the code is invalid, then parsing will reject this anyway. If it is valid
            // but not a literal or unary, so we just behave as if there is no default as far
            // as the type system is concerned. That means we cannot add this field to an existing
            // schema (unless as optional), but in a new schema is fine. The runtime will execute
            // this expression and end up with the correct default.
            _ => Ok(None),
        },
    }
}

fn get_type_decorators(handler: &Handler, x: &[Decorator]) -> Result<(Vec<String>, bool)> {
    let mut output = vec![];
    let mut is_unique = false;
    for dec in x.iter() {
        match &*dec.expr {
            Expr::Call(call) => {
                let callee = call.callee.clone().expr().ok_or_else(|| {
                    anyhow!("expected expression, got {:?} instead", call.callee.clone())
                })?;
                let name = get_ident_string(handler, &callee)?;
                ensure!(
                    name == "labels",
                    format!("decorator '{}' is not supported by ChiselStrike", name)
                );
                for arg in &call.args {
                    if let Some((label, ty)) = get_field_value(handler, &Some(arg.expr.clone()))? {
                        ensure!(ty == DeclType::String, "Only strings accepted as labels");
                        output.push(label);
                    }
                }
            }
            Expr::Ident(x) => {
                let name = ident_to_string(x);
                ensure!(name != "labels", "expected a call-like decorator");

                ensure!(
                    name == "unique",
                    format!("decorator '{}' is not supported by ChiselStrike", name)
                );
                is_unique = true;
            }
            z => {
                return Err(swc_err(handler, z, "expected a call-like decorator"));
            }
        };
    }
    Ok((output, is_unique))
}

fn parse_class_prop(x: &ClassProp, class_name: &str, handler: &Handler) -> Result<EntityDeclField> {
    let (field_name, is_optional) = get_field_info(handler, &x.key)?;
    anyhow::ensure!(field_name != "id", "Creating a field with the name `id` is not supported. 😟\nBut don't worry! ChiselStrike creates an id field automatically, and you can access it in your endpoints as {}.id 🤩", class_name);

    let (default_value, field_type) = match get_field_value(handler, &x.value)? {
        None => (None, get_field_type(handler, &x.type_ann)?),
        Some((val, val_ty)) => (Some(val), val_ty),
    };
    let (labels, is_unique) = get_type_decorators(handler, &x.decorators)?;

    let missing_initializer = match &field_type {
        DeclType::TypeRef(name) if !is_optional => match &x.value {
            None => true,
            Some(k) => match &**k {
                Expr::New(_) => false,
                x => anyhow::bail!(swc_err(
                    handler,
                    x,
                    &format!(
                        "field `{field_name}` of type name `{name}` has unexpected initializer"
                    ),
                )),
            },
        },
        _ => false,
    };

    Ok(EntityDeclField {
        name: field_name,
        is_optional,
        is_unique,
        default_value,
        field_type,
        labels,
        missing_initializer,
    })
}

fn parse_class_decl<P: AsRef<Path>>(
    handler: &Handler,
    filename: &P,
    decl: &ClassDecl,
) -> Result<EntityDecl> {
    let mut fields = Vec::default();
    let name = ident_to_string(&decl.ident);

    for member in &decl.class.body {
        match member {
            ClassMember::ClassProp(x) => match parse_class_prop(x, &name, handler) {
                Err(err) => {
                    handler.span_err(x.span(), &format!("While parsing class {}", name));
                    bail!("{}", err);
                }
                Ok(fd) => {
                    fields.push(fd);
                }
            },
            ClassMember::Constructor(_x) => {
                handler.span_err(member.span(), "Constructors not allowed in ChiselStrike model definitions. Consider adding default values so one is not needed, or call ChiselEntity's create method");
                bail!("invalid type file {}", filename.as_ref().display());
            }
            _ => {}
        }
    }
    Ok(EntityDecl { name, fields })
}

fn parse_type_decl(handler: &Handler, decl: &TsTypeAliasDecl) -> Result<StructDecl> {
    let type_name = ident_to_string(&decl.id);
    let mut fields = vec![];
    match &*decl.type_ann {
        TsType::TsTypeLit(type_lit) => {
            for element in &type_lit.members {
                let field = parse_type_element(element, &type_name, handler)?;
                fields.push(field);
            }
        }
        _ => {}
    }
    Ok(StructDecl {
        name: type_name.clone(),
        fields,
    })
}

fn parse_type_element(
    element: &TsTypeElement,
    type_name: &str,
    handler: &Handler,
) -> Result<StructDeclField> {
    match element {
        TsTypeElement::TsPropertySignature(property) => {
            let (field_name, is_optional) = match &*property.key {
                Expr::Ident(id) => (ident_to_string(id), id.optional),
                z => anyhow::bail!(swc_err(handler, z, "expected an identifier")),
            };

            let field_type = if let Some(type_ann) = &property.type_ann {
                match &*type_ann.type_ann {
                    TsType::TsKeywordType(kw) => map_keyword_type(handler, kw)?,
                    _ => anyhow::bail!(swc_err(
                        handler,
                        type_ann,
                        &format!(
                            "type `{type_name}` has a property `{field_name}` of unsupported type"
                        ),
                    )),
                }
            } else {
                anyhow::bail!(swc_err(
                    handler,
                    property,
                    &format!(
                        "type `{type_name}` has a property `{field_name}` without type annotation"
                    ),
                ))
            };
            Ok(StructDeclField {
                name: field_name,
                field_type,
                is_optional,
            })
        }
        _ => anyhow::bail!(swc_err(
            handler,
            element,
            &format!("type `{type_name}` has a member which isn't a property"),
        )),
    }
}

fn parse_one_file<P: AsRef<Path>>(filename: &P, user_types: &mut UserTypes) -> Result<()> {
    let cm: Lrc<SourceMap> = Default::default();

    let emitter = Box::new(emitter::EmitterWriter::new(
        Box::new(std::io::stderr()),
        Some(cm.clone()),
        false,
        true,
    ));

    let handler = Handler::with_emitter(true, false, emitter);

    let fm = cm.load_file(filename.as_ref())?;

    let mut config = TsConfig {
        decorators: true,
        ..Default::default()
    };
    config.decorators = true;

    let lexer = Lexer::new(
        // We want to parse typescript with decorators support
        Syntax::Typescript(config),
        Default::default(),
        StringInput::from(&*fm),
        None,
    );

    let mut parser = Parser::new_from(lexer);
    let mut errors = false;
    for e in parser.take_errors() {
        errors = true;
        e.into_diagnostic(&handler).emit();
    }
    if errors {
        bail!("Exiting on parsing errors");
    }

    let x = parser.parse_typescript_module().map_err(|e| {
        e.into_diagnostic(&handler).emit();
        anyhow!("Exiting on script parsing errors")
    })?;

    for decl in &x.body {
        match decl {
            ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(exp)) => match &exp.decl {
                Decl::Class(class_decl) => {
                    let entity = parse_class_decl(&handler, filename, &class_decl)?;
                    user_types.add_entity(entity)?;
                }
                Decl::TsTypeAlias(type_decl) => {
                    let user_struct = parse_type_decl(&handler, &type_decl)?;
                    user_types.add_structure(user_struct)?;
                }
                z => {
                    handler.span_err(z.span(), "Only class definitions allowed in the types file");
                    bail!("invalid type file {}", filename.as_ref().display());
                }
            },
            ModuleItem::ModuleDecl(ModuleDecl::Import(_)) => {
                // Right now just accept imports, but don't try to parse them.
                // The compiler will error out if the imports are invalid.
            }
            ModuleItem::Stmt(_) => {}
            z => {
                handler.span_err(
                    z.span(),
                    "ChiselStrike expects either import statements or exported classes (but not default exported)",
                );
                bail!("invalid type file {}", filename.as_ref().display());
            }
        }
    }
    Ok(())
}

struct EntityDecl {
    name: String,
    fields: Vec<EntityDeclField>,
}

struct EntityDeclField {
    name: String,
    field_type: DeclType,
    labels: Vec<String>,
    default_value: Option<String>,
    is_optional: bool,
    is_unique: bool,
    /// Set to true if the field is of TypeRef type and it's missing default initializer which
    /// will result in a warning if the field type turns out to be Entity.
    missing_initializer: bool,
}

struct StructDecl {
    name: String,
    fields: Vec<StructDeclField>,
}

struct StructDeclField {
    name: String,
    field_type: DeclType,
    is_optional: bool,
}

#[derive(PartialEq)]
enum DeclType {
    String,
    Number,
    Bool,
    /// Reference to an Entity or Structure
    TypeRef(String),
    Array(Box<DeclType>),
}

struct UserTypes {
    entities: Vec<EntityDecl>,
    structs: Vec<StructDecl>,
}

impl UserTypes {
    fn new() -> Self {
        Self {
            entities: vec![],
            structs: vec![],
        }
    }

    fn check_existence(&self, name: &str) -> Result<()> {
        if self.get_entity(name).is_some() {
            anyhow::bail!("entity of name {name} already exists!");
        }
        if self.get_struct(name).is_some() {
            anyhow::bail!("structure of name {name} already exists!");
        }
        Ok(())
    }

    fn add_entity(&mut self, entity: EntityDecl) -> Result<()> {
        self.check_existence(&entity.name)
            .context(anyhow!("failed to add entity {}", entity.name))?;
        self.entities.push(entity);
        Ok(())
    }

    fn add_structure(&mut self, user_struct: StructDecl) -> Result<()> {
        self.check_existence(&user_struct.name)
            .context(anyhow!("failed to add structure {}", user_struct.name))?;
        self.structs.push(user_struct);
        Ok(())
    }

    fn get_entity(&self, name: &str) -> Option<&EntityDecl> {
        self.entities.iter().find(|e| e.name == name)
    }

    fn get_struct(&self, name: &str) -> Option<&StructDecl> {
        self.structs.iter().find(|e| e.name == name)
    }

    fn generate_type_requests(&self) -> Result<Vec<AddTypeRequest>> {
        let mut add_type_requests = vec![];
        for entity in &self.entities {
            let mut fields = vec![];
            for field in &entity.fields {
                let field_type = self.resolve_type(&field.field_type)?;
                if field.missing_initializer {
                    if let TypeEnum::Entity(field_type_name) = &field_type {
                        eprintln!(
                            "Warning: Entity `{entity}` contains field `{field}` of entity type `{field_type}` which is not default-initialized.\n\
                            \tWhen using this field, its methods might not be available. As a temporary workaround, please consider initializing the field `{field}: {entity} = new {entity}();`\n\
                            \tFor further information, please see https://github.com/chiselstrike/chiselstrike/issues/1541",
                            entity=entity.name, field=field.name, field_type=field_type_name
                        );
                    }
                }

                fields.push(FieldDefinition {
                    name: field.name.clone(),
                    field_type: Some(field_type.into()),
                    labels: field.labels.clone(),
                    is_optional: field.is_optional,
                    default_value: field.default_value.clone(),
                    is_unique: field.is_unique,
                })
            }

            add_type_requests.push(AddTypeRequest {
                name: entity.name.to_string(),
                field_defs: fields,
            })
        }
        Ok(add_type_requests)
    }

    fn resolve_type(&self, ty: &DeclType) -> Result<TypeEnum> {
        let proto_ty = match ty {
            DeclType::String => TypeEnum::String(true),
            DeclType::Number => TypeEnum::Number(true),
            DeclType::Bool => TypeEnum::Bool(true),
            DeclType::TypeRef(type_name) => {
                if self.get_entity(type_name).is_some() || is_auth_entity_name(type_name) {
                    TypeEnum::Entity(type_name.to_string())
                } else if let Some(structure) = self.get_struct(type_name) {
                    let mut fields = vec![];
                    for field in &structure.fields {
                        let field_type = self.resolve_type(&field.field_type)?;
                        fields.push(StructField {
                            name: field.name.to_string(),
                            field_type: Some(field_type.into()),
                            is_optional: field.is_optional,
                        })
                    }
                    TypeEnum::Struct(StructType {
                        name: type_name.to_string(),
                        fields,
                    })
                } else {
                    anyhow::bail!("unable to resolve type name {type_name}")
                }
            }
            DeclType::Array(element_type) => TypeEnum::array(self.resolve_type(&*element_type)?),
        };
        Ok(proto_ty)
    }
}

pub(crate) fn parse_types<P: AsRef<Path>>(files: &[P]) -> Result<Vec<AddTypeRequest>> {
    let mut user_types = UserTypes::new();

    for filename in files {
        parse_one_file(filename, &mut user_types)?;
    }
    dbg!(user_types.generate_type_requests());
    user_types.generate_type_requests()
}
