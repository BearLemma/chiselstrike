use crate::api::ApiService;
use crate::deno;
use crate::store::{Store, StoreError};
use crate::types::{Type, TypeSystem, TypeSystemError};
use chisel::chisel_rpc_server::{ChiselRpc, ChiselRpcServer};
use chisel::{
    EndPointCreationRequest, EndPointCreationResponse, StatusRequest, StatusResponse,
    TypeDefinitionRequest, TypeDefinitionResponse, TypeExportRequest, TypeExportResponse,
};
use convert_case::{Case, Casing};
use serde_json::json;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::sync::Mutex;
use tonic::{transport::Server, Request, Response, Status};

pub mod chisel {
    tonic::include_proto!("chisel");
}

/// RPC service for Chisel server.
///
/// The RPC service provides a Protobuf-based interface for Chisel control
/// plane. For example, the service has RPC calls for managing types and
/// endpoints. The user-generated data plane endpoints are serviced with REST.
pub struct RpcService {
    api: Arc<Mutex<ApiService>>,
    type_system: Arc<Mutex<TypeSystem>>,
    store: Arc<Mutex<Store>>,
}

impl RpcService {
    pub fn new(
        api: Arc<Mutex<ApiService>>,
        type_system: Arc<Mutex<TypeSystem>>,
        store: Arc<Mutex<Store>>,
    ) -> Self {
        RpcService {
            api,
            type_system,
            store,
        }
    }

    pub async fn define_type_endpoints(&self, name: &str) {
        let path = format!("/{}", name.to_case(Case::Snake));
        info!("Registered endpoint: '{}'", path);
        self.api.lock().await.get(
            &path,
            Box::new(|| {
                // Let's return an empty array because we don't do storage yet.
                let result = json!([]);
                Ok(result.to_string())
            }),
        );
    }
}

impl From<StoreError> for Status {
    fn from(err: StoreError) -> Self {
        Status::internal(format!("{}", err))
    }
}

impl From<TypeSystemError> for Status {
    fn from(err: TypeSystemError) -> Self {
        Status::internal(format!("{}", err))
    }
}

#[tonic::async_trait]
impl ChiselRpc for RpcService {
    /// Get Chisel server status.
    async fn get_status(
        &self,
        _request: Request<StatusRequest>,
    ) -> Result<Response<StatusResponse>, Status> {
        let response = chisel::StatusResponse {
            message: "OK".to_string(),
        };
        Ok(Response::new(response))
    }

    /// Define a type.
    async fn define_type(
        &self,
        request: Request<TypeDefinitionRequest>,
    ) -> Result<Response<TypeDefinitionResponse>, Status> {
        let mut type_system = self.type_system.lock().await;
        let type_def = request.into_inner();
        let name = type_def.name;
        let mut fields = Vec::new();
        for field in type_def.field_defs {
            fields.push((field.name, field.field_type));
        }
        let ty = Type {
            name: name.to_owned(),
            fields,
        };
        type_system.define_type(ty.to_owned())?;
        let store = self.store.lock().await;
        store.insert(ty).await?;
        self.define_type_endpoints(&name).await;
        let response = chisel::TypeDefinitionResponse { message: name };
        Ok(Response::new(response))
    }

    async fn export_types(
        &self,
        _request: tonic::Request<TypeExportRequest>,
    ) -> Result<tonic::Response<TypeExportResponse>, tonic::Status> {
        let type_system = self.type_system.lock().await;
        let mut type_defs = vec![];
        for ty in type_system.types.values() {
            let mut field_defs = vec![];
            for (field_name, field_type) in &ty.fields {
                field_defs.push(chisel::FieldDefinition {
                    name: field_name.to_owned(),
                    field_type: field_type.to_owned(),
                });
            }
            let type_def = chisel::TypeDefinition {
                name: ty.name.to_string(),
                field_defs,
            };
            type_defs.push(type_def);
        }
        let response = chisel::TypeExportResponse { type_defs };
        Ok(Response::new(response))
    }

    async fn create_end_point(
        &self,
        request: tonic::Request<EndPointCreationRequest>,
    ) -> Result<tonic::Response<EndPointCreationResponse>, tonic::Status> {
        let request = request.into_inner();
        let path = format!("/{}", request.path);
        let code = request.code;
        let func = {
            let path = path.clone();
            move || deno::run_js(&path, &code)
        };
        self.api.lock().await.get(&path, Box::new(func));
        let response = EndPointCreationResponse { message: path };
        Ok(Response::new(response))
    }
}

pub fn spawn(
    rpc: RpcService,
    addr: SocketAddr,
    shutdown: impl core::future::Future<Output = ()> + Send + 'static,
) -> tokio::task::JoinHandle<Result<(), tonic::transport::Error>> {
    tokio::spawn(async move {
        let ret = Server::builder()
            .add_service(ChiselRpcServer::new(rpc))
            .serve_with_shutdown(addr, shutdown)
            .await;
        info!("Tonic shutdown");
        ret
    })
}