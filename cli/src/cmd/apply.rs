// SPDX-FileCopyrightText: © 2021 ChiselStrike <info@chiselstrike.com>

pub mod deno;
pub mod node;

use crate::chisel::chisel_rpc_client::ChiselRpcClient;
use crate::chisel::{ChiselApplyRequest, PolicyUpdateRequest};
use crate::project::{read_manifest, read_to_string, Module, Optimize};
use anyhow::{anyhow, Context, Result};
use std::io::Write;
use std::path::PathBuf;
use std::process::Stdio;

static DEFAULT_APP_NAME: &str = "ChiselStrike Application";

pub(crate) enum AllowTypeDeletion {
    No,
    Yes,
}

impl From<AllowTypeDeletion> for bool {
    fn from(v: AllowTypeDeletion) -> Self {
        match v {
            AllowTypeDeletion::No => false,
            AllowTypeDeletion::Yes => true,
        }
    }
}

impl From<bool> for AllowTypeDeletion {
    fn from(v: bool) -> Self {
        match v {
            false => AllowTypeDeletion::No,
            true => AllowTypeDeletion::Yes,
        }
    }
}

#[derive(Copy, Clone)]
pub(crate) enum TypeChecking {
    No,
    Yes,
}

impl From<TypeChecking> for bool {
    fn from(v: TypeChecking) -> Self {
        match v {
            TypeChecking::No => false,
            TypeChecking::Yes => true,
        }
    }
}

impl From<bool> for TypeChecking {
    fn from(v: bool) -> Self {
        match v {
            false => TypeChecking::No,
            true => TypeChecking::Yes,
        }
    }
}

pub(crate) async fn apply<S: ToString>(
    server_url: String,
    version: S,
    allow_type_deletion: AllowTypeDeletion,
    type_check: TypeChecking,
) -> Result<()> {
    let version = version.to_string();

    let manifest = read_manifest().with_context(|| "Reading manifest file".to_string())?;
    let models = manifest.models()?;
    let endpoints = manifest.endpoints()?;
    let policies = manifest.policies()?;

    let types_req = crate::ts::parse_types(&models)?;
    let mut policy_req = vec![];

    let mut types_string = String::new();
    for t in &models {
        types_string += &read_to_string(&t)?;
    }
    let entities: Vec<String> = types_req
        .iter()
        .map(|type_req| type_req.name.clone())
        .collect();
    let use_chiselc = is_chiselc_available() && manifest.optimize == Optimize::Yes;
    let endpoints_req = if manifest.modules == Module::Node {
        node::apply(&endpoints, &entities, use_chiselc, &type_check).await
    } else {
        deno::apply(&endpoints, &entities, &types_string, use_chiselc).await
    }?;

    for p in policies {
        policy_req.push(PolicyUpdateRequest {
            policy_config: read_to_string(p)?,
        });
    }

    let package = match read_to_string("./package.json") {
        Ok(x) => {
            let val: serde_json::Result<serde_json::Value> = serde_json::from_str(&x);
            match val {
                Ok(val) => val,
                Err(_) => serde_json::json!("{}"),
            }
        }
        Err(_) => serde_json::json!("{}"),
    };

    let git_version = get_git_version();

    let app_name = package["name"]
        .as_str()
        .unwrap_or(DEFAULT_APP_NAME)
        .to_owned();
    let mut version_tag = package["version"].as_str().unwrap_or("").to_owned();

    version_tag = match git_version {
        Some(v) => {
            if version_tag.is_empty() {
                v
            } else {
                format!("{}-{}", version_tag, v)
            }
        }
        None => version_tag,
    };

    let mut client = ChiselRpcClient::connect(server_url).await?;
    let msg = execute!(
        client
            .apply(tonic::Request::new(ChiselApplyRequest {
                types: types_req,
                index_candidates: vec![],
                endpoints: endpoints_req,
                policies: policy_req,
                allow_type_deletion: allow_type_deletion.into(),
                version,
                version_tag,
                app_name,
            }))
            .await
    );

    for ty in msg.types {
        println!("Model defined: {}", ty);
    }

    for end in msg.endpoints {
        println!("End point defined: {}", end);
    }

    for lbl in msg.labels {
        println!("Policy defined for label {}", lbl);
    }

    Ok(())
}

fn output_to_string(out: &std::process::Output) -> Option<String> {
    Some(
        std::str::from_utf8(&out.stdout)
            .expect("command output not utf-8")
            .trim()
            .to_owned(),
    )
}

fn chiselc_cmd() -> Result<PathBuf> {
    let mut cmd = std::env::current_exe()?;
    cmd.pop();
    cmd.push("chiselc");
    Ok(cmd)
}

fn is_chiselc_available() -> bool {
    let cmd = match chiselc_cmd() {
        Ok(cmd) => cmd,
        _ => return false,
    };
    let mut cmd = std::process::Command::new(cmd);
    cmd.args(&["--version"]);
    match cmd.output() {
        Ok(output) => output.status.success(),
        _ => false,
    }
}

/// Spawn `chiselc` and return a reference to the child process.
fn chiselc_spawn(input: &str, output: &str, entities: &[String]) -> Result<tokio::process::Child> {
    let mut args: Vec<&str> = vec![input, "--output", output, "--target", "js"];
    if !entities.is_empty() {
        args.push("-e");
        for entity in entities.iter() {
            args.push(entity);
        }
    }
    let cmd = tokio::process::Command::new(chiselc_cmd()?)
        .args(args)
        .spawn()?;
    Ok(cmd)
}

/// Spawn `chiselc`, wait for the process to complete, and return its output.
fn chiselc_output(code: String, entities: &[String]) -> Result<std::process::Output> {
    let mut args: Vec<&str> = vec!["--target", "js"];
    if !entities.is_empty() {
        args.push("-e");
        for entity in entities.iter() {
            args.push(entity);
        }
    }
    let mut cmd = std::process::Command::new(chiselc_cmd()?)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;
    let mut stdin = cmd.stdin.take().expect("Failed to open stdin");
    std::thread::spawn(move || {
        stdin
            .write_all(code.as_bytes())
            .expect("Failed to write to stdin");
    });
    let output = cmd.wait_with_output().expect("Failed to read stdout");
    Ok(output)
}

fn get_git_version() -> Option<String> {
    let mut cmd = std::process::Command::new("git");
    cmd.args(["describe", "--exact-match", "--tags"]);

    let tag = cmd.output().ok()?;
    if tag.status.success() {
        return output_to_string(&tag);
    }

    let mut cmd = std::process::Command::new("git");
    cmd.args(["rev-parse", "--short", "HEAD"]);

    let sha = cmd.output().ok()?;
    if sha.status.success() {
        return output_to_string(&sha);
    }
    None
}
