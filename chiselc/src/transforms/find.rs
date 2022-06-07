use crate::filtering::FilterProperties;
use crate::query::Operator as QOperator;
use crate::symbols::Symbols;
use crate::transforms::utils::{extract_filter, lookup_callee_entity_type};
use crate::utils::is_call_to_entity_method;

use swc_ecmascript::ast::{CallExpr, Callee};

/// Infer filter operator from the lambda predicate of `findMany`.
pub fn infer_find(
    call_expr: &CallExpr,
    symbols: &Symbols,
) -> (Option<Box<QOperator>>, Option<FilterProperties>) {
    if !is_rewritable_find("findMany", &call_expr.callee, symbols) {
        return (None, None);
    }
    let entity_type = match lookup_callee_entity_type(&call_expr.callee) {
        Ok(entity_type) => entity_type,
        _ => return (None, None),
    };
    extract_filter(call_expr, entity_type, "__findMany".to_string())
}

fn is_rewritable_find(function: &str, callee: &Callee, symbols: &Symbols) -> bool {
    match callee {
        Callee::Expr(expr) => is_call_to_entity_method(expr, function, symbols),
        _ => false,
    }
}
