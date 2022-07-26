use crate::common::bin_dir;
use crate::common::repo_dir;
use anyhow::{Context, Result};
use bytes::BytesMut;
use checked_command::CheckedCommand;
use rand::{distributions::Alphanumeric, Rng};
use serde_json::json;
use std::fs;
use std::pin::Pin;
use std::process::Stdio;
use std::rc::Rc;
use std::time::Duration;
use tempdir::TempDir;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWriteExt};
use tokio::process::{Child as AsyncChild, Command as AsyncCommand};

mod common;

struct GuardedChild {
    _child: AsyncChild,
    stdout: AsyncTestableOutput,
    stderr: AsyncTestableOutput,
}

impl GuardedChild {
    fn new(command: &mut AsyncCommand) -> Self {
        let mut child = command
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .kill_on_drop(true)
            .spawn()
            .expect("failed to spawn GuardedChild");

        let stdout = child.stdout.take().unwrap();
        let stderr = child.stderr.take().unwrap();
        Self {
            _child: child,
            stdout: AsyncTestableOutput::new(OutputType::Stdout, Box::pin(stdout)),
            stderr: AsyncTestableOutput::new(OutputType::Stderr, Box::pin(stderr)),
        }
    }

    #[allow(dead_code)]
    async fn show_output(&mut self) {
        self.stdout.show().await.unwrap();
        self.stderr.show().await.unwrap();
    }
}

struct AsyncTestableOutput {
    #[allow(dead_code)]
    output_type: OutputType,
    async_output: Pin<Box<dyn AsyncRead>>,
    raw_output: BytesMut,
    cursor: usize,
}

impl AsyncTestableOutput {
    fn new(output_type: OutputType, async_output: Pin<Box<dyn AsyncRead>>) -> Self {
        Self {
            output_type,
            async_output,
            raw_output: BytesMut::new(),
            cursor: 0,
        }
    }

    async fn must_contain(&mut self, pattern: &str) -> Result<()> {
        let pattern = pattern.to_string();
        let checking_fut = async {
            loop {
                if self.internal_must_contain(&pattern) {
                    break;
                }
                self.async_output
                    .read_buf(&mut self.raw_output)
                    .await
                    .unwrap();
            }
        };
        tokio::time::timeout(Duration::from_secs(1), checking_fut)
            .await
            .with_context(|| format!("failed to retrieve pattern in time: `{pattern}`"))
    }

    fn internal_must_contain(&mut self, pattern: &str) -> bool {
        let output = self.decoded_output();
        if let Some(idx) = output[self.cursor..].find(pattern) {
            self.cursor = idx + pattern.len();
            true
        } else {
            false
        }
    }

    fn decoded_output(&self) -> String {
        let colorless_output = strip_ansi_escapes::strip(&self.raw_output).unwrap();
        String::from_utf8(colorless_output).unwrap()
    }

    #[allow(dead_code)]
    async fn load_to_buffer(&mut self, timeout: core::time::Duration) {
        let _ = tokio::time::timeout(timeout, async {
            loop {
                self.async_output
                    .read_buf(&mut self.raw_output)
                    .await
                    .unwrap();
            }
        })
        .await;
    }

    #[allow(dead_code)]
    async fn show(&mut self) -> Result<()> {
        self.load_to_buffer(Duration::from_secs(1)).await;

        let mut stdout = tokio::io::stdout();
        stdout.write_all(&self.raw_output).await?;
        stdout.flush().await?;
        Ok(())
    }
}

enum OpMode {
    Deno,
    Node,
}

#[derive(Clone, Debug)]
struct ChiseldConfig {
    public_address: String,
    internal_address: String,
    rpc_address: String,
}

struct PostgresConfig {
    host: String,
    user: Option<String>,
    password: Option<String>,
    db_name: String,
}

impl PostgresConfig {
    fn new(host: String, user: Option<String>, password: Option<String>) -> PostgresConfig {
        let db_id: String = rand::thread_rng()
            .sample_iter(&Alphanumeric)
            .take(40)
            .map(char::from)
            .collect();
        let db_name = format!("datadb_{db_id}");
        PostgresConfig {
            host,
            user,
            password,
            db_name,
        }
    }

    fn get_url_prefix(&self) -> url::Url {
        let user = self.user.clone().unwrap_or_else(whoami::username);
        let mut url_prefix = "postgres://".to_string();
        url_prefix.push_str(&user);
        if let Some(password) = &self.password {
            url_prefix.push(':');
            url_prefix.push_str(&password);
        }
        url_prefix.push('@');
        url_prefix.push_str(&self.host);

        url::Url::parse(&url_prefix).expect("failed to generate postgres db url")
    }
}

enum DatabaseConfig {
    Postgres(PostgresConfig),
    Sqlite,
}

struct PostgresDb {
    config: PostgresConfig,
}

impl Drop for PostgresDb {
    fn drop(&mut self) {
        CheckedCommand::new("psql")
            .args([
                &self.config.get_url_prefix().as_str(),
                "-c",
                format!("DROP DATABASE {}", &self.config.db_name).as_str(),
            ])
            .output()
            .expect("failed to drop test database on cleanup");
    }
}

impl PostgresDb {
    fn new(config: PostgresConfig) -> Self {
        CheckedCommand::new("psql")
            .args([
                &config.get_url_prefix().as_str(),
                "-c",
                format!("CREATE DATABASE {}", &config.db_name).as_str(),
            ])
            .output()
            .expect("failed to init chisel project in node mode");
        Self { config }
    }

    fn url(&self) -> Result<String> {
        Ok(self
            .config
            .get_url_prefix()
            .join(&self.config.db_name)?
            .as_str()
            .to_string())
    }
}

struct SqliteDb {
    tmp_dir: Rc<TempDir>,
}

impl SqliteDb {
    fn url(&self) -> Result<String> {
        let path = self.tmp_dir.path().join("chiseld.db");
        Ok(format!("sqlite://{}?mode=rwc", path.display()))
    }
}

enum Database {
    Postgres(PostgresDb),
    Sqlite(SqliteDb),
}

impl Database {
    fn url(&self) -> Result<String> {
        match self {
            Database::Postgres(db) => db.url(),
            Database::Sqlite(db) => db.url(),
        }
    }
}

struct Chisel {
    config: ChiseldConfig,
    tmp_dir: Rc<TempDir>,
}

impl Chisel {
    fn exec(&self, cmd: &str, args: &[&str]) -> Result<MixedTestableOutput, ProcessError> {
        let rpc_url = format!("http://{}", self.config.rpc_address);
        let chisel_path = bin_dir().join("chisel").to_str().unwrap().to_string();
        let args = [&["--rpc-addr", &rpc_url, cmd], args].concat();

        CheckedCommand::new(chisel_path)
            .args(args)
            .current_dir(&*self.tmp_dir)
            .output()
            .map(|output| MixedTestableOutput {
                stdout: TestableOutput::new(OutputType::Stdout, &output.stdout).unwrap(),
                stderr: TestableOutput::new(OutputType::Stdout, &output.stderr).unwrap(),
            })
            .map_err(|e| e.into())
    }

    fn apply(&self) -> Result<MixedTestableOutput, ProcessError> {
        self.exec("apply", &[])
    }

    fn write(&self, path: &str, code: &str) {
        let full_path = self.tmp_dir.path().join(path);
        fs::write(full_path, code).expect("Unable to write endpoint");
    }

    fn copy<P, Q>(&self, from: P, to: Q) -> u64
    where
        P: AsRef<std::path::Path> + std::fmt::Debug,
        Q: AsRef<std::path::Path> + std::fmt::Debug,
    {
        let options = fs_extra::dir::CopyOptions {
            copy_inside: true,
            ..Default::default()
        };
        fs_extra::copy_items(&[&from], self.tmp_dir.path().join(&to), &options)
            .expect(&format!("failed to copy '{:?}' to '{:?}'", from, to))
    }

    fn copy_and_rename<P, Q>(&self, from: P, to: Q) -> u64
    where
        P: AsRef<std::path::Path> + std::fmt::Debug,
        Q: AsRef<std::path::Path> + std::fmt::Debug,
    {
        std::fs::copy(&from, self.tmp_dir.path().join(&to))
            .expect(&format!("failed to copy '{:?}' to '{:?}'", from, to))
    }

    async fn post(&self, url: &str, data: serde_json::Value) -> Result<reqwest::Response> {
        let url = url::Url::parse(&format!("http://{}", self.config.public_address))
            .unwrap()
            .join(url)
            .unwrap();
        let client = reqwest::Client::new();
        let resp = client
            .post(url.as_str())
            .body(data.to_string())
            .timeout(core::time::Duration::from_secs(5))
            .send()
            .await?
            .error_for_status()?;
        Ok(resp)
    }
}

#[derive(Debug)]
struct ProcessError {
    output: MixedTestableOutput,
}

impl From<checked_command::Error> for ProcessError {
    fn from(e: checked_command::Error) -> Self {
        if let checked_command::Error::Failure(_, Some(output)) = e {
            Self {
                output: MixedTestableOutput {
                    stdout: TestableOutput::new(OutputType::Stdout, &output.stdout).unwrap(),
                    stderr: TestableOutput::new(OutputType::Stderr, &output.stderr).unwrap(),
                },
            }
        } else {
            Self {
                output: MixedTestableOutput {
                    stdout: TestableOutput::new(OutputType::Stdout, &vec![]).unwrap(),
                    stderr: TestableOutput::new(OutputType::Stderr, &vec![]).unwrap(),
                },
            }
        }
    }
}

impl ProcessError {
    #[allow(dead_code)]
    fn stdout(&self) -> TestableOutput {
        self.output.stdout.clone()
    }

    fn stderr(&self) -> TestableOutput {
        self.output.stderr.clone()
    }
}

#[derive(PartialEq, Debug, Clone)]
enum OutputType {
    Stdout,
    Stderr,
}

impl OutputType {
    fn to_str(&self) -> &str {
        match self {
            OutputType::Stdout => "stdout",
            OutputType::Stderr => "stderr",
        }
    }
}

#[derive(Debug, Clone)]
struct MixedTestableOutput {
    stdout: TestableOutput,
    stderr: TestableOutput,
}

#[derive(Debug, Clone)]
struct TestableOutput {
    output_type: OutputType,
    output: String,
    cursor: usize,
}

impl TestableOutput {
    fn new(output_type: OutputType, raw_output: &Vec<u8>) -> Result<Self> {
        let colorless_output = strip_ansi_escapes::strip(raw_output)?;
        let output = String::from_utf8(colorless_output)?;
        Ok(Self {
            output_type,
            output,
            cursor: 0,
        })
    }

    fn must_contain_ord(&mut self, pattern: &str) -> &mut Self {
        if let Some(idx) = self.output[self.cursor..].find(pattern) {
            self.cursor = idx + pattern.len();
            self
        } else {
            let out_type = self.output_type.to_str();
            let output = &self.output;
            panic!("failed to find text in the {out_type}: {pattern}\nFull output:\n{output}");
        }
    }

    fn must_contain(self, pattern: &str) -> Self {
        assert!(self.output.contains(pattern));
        self
    }
}

fn chiseld() -> String {
    bin_dir().join("chiseld").to_str().unwrap().to_string()
}

fn get_free_ports(n: usize) -> Vec<u16> {
    let mut rng = rand::thread_rng();
    for _ in 0..1000 {
        let ports: Vec<_> = rand::seq::index::sample(&mut rng, (1 << 16) - 1000, n)
            .iter()
            .map(|p| (p + 1000) as u16)
            .collect();
        let ports = port_scanner::local_ports_available(ports);
        if ports.len() == n {
            return ports;
        }
    }
    panic!("failed to find free ports in 1000 iterations");
}

async fn generate_chiseld_config() -> ChiseldConfig {
    // chiseld.stderr.load_to_buffer(Duration::from_secs(1)).await;
    // let output = chiseld.stderr.decoded_output();

    // let re = regex::Regex::new(r"ChiselStrike is ready.*URL: http://(\S+)").unwrap();
    // let caps = re.captures(&output).unwrap();
    // let public_address = caps.get(1).unwrap().as_str().to_string();

    // let re = regex::Regex::new(r"RPC server is listening on address (\S+)").unwrap();
    // let caps = re.captures(&output).unwrap();
    // let rpc_address = caps.get(1).unwrap().as_str().to_string();

    // let re = regex::Regex::new(r"Internal HTTP server is listening on address (\S+)").unwrap();
    // let caps = re.captures(&output).unwrap();
    // let internal_address = caps.get(1).unwrap().as_str().to_string();

    let ports = get_free_ports(3);

    ChiseldConfig {
        public_address: format!("127.0.0.1:{}", ports.get(0).unwrap()),
        rpc_address: format!("127.0.0.1:{}", ports.get(1).unwrap()),
        internal_address: format!("127.0.0.1:{}", ports.get(2).unwrap()),
    }
}

async fn setup_chiseld(
    mode: OpMode,
    db_config: DatabaseConfig,
    optimize: bool,
) -> Result<(Database, GuardedChild, Chisel)> {
    let tmp_dir = Rc::new(TempDir::new("chiseld_test")?);

    let db: Database = match db_config {
        DatabaseConfig::Postgres(config) => Database::Postgres(PostgresDb::new(config)),
        DatabaseConfig::Sqlite => Database::Sqlite(SqliteDb {
            tmp_dir: tmp_dir.clone(),
        }),
    };

    let chiseld_config = generate_chiseld_config().await;
    let chiseld = GuardedChild::new(AsyncCommand::new(chiseld()).args([
        "--webui",
        "--db-uri",
        db.url()?.as_str(),
        "--api-listen-addr",
        &chiseld_config.public_address,
        "--internal-routes-listen-addr",
        &chiseld_config.internal_address,
        "--rpc-listen-addr",
        &chiseld_config.rpc_address,
    ]));

    let chisel = Chisel {
        config: chiseld_config.clone(),
        tmp_dir: tmp_dir.clone(),
    };

    let optimize_str = format!("{}", optimize);

    match mode {
        OpMode::Deno => {
            chisel
                .exec(
                    "init",
                    &[
                        "--no-examples",
                        "--optimize",
                        &optimize_str,
                        "--auto-index",
                        &optimize_str,
                    ],
                )
                .expect("chisel init failed");
        }
        OpMode::Node => {
            let create_app = repo_dir()
                .join("packages/create-chiselstrike-app/dist/index.js")
                .to_str()
                .unwrap()
                .to_string();
            CheckedCommand::new("node")
                .args([&create_app, "--chisel-version", "latest", "./"])
                .current_dir(&*tmp_dir)
                .output()
                .expect("failed to init chisel project in node mode");
        }
    }

    Ok((db, chiseld, chisel))
}

#[tokio::test]
async fn test_bad_filter() {
    let (_db, _chiseld_proc, chisel) = setup_chiseld(OpMode::Deno, DatabaseConfig::Sqlite, true)
        .await
        .expect("failed to setup chiseld");

    chisel.copy("examples/person.ts", "models");
    chisel.write(
        "endpoints/query.ts",
        r##"
        import { Person } from "../models/person.ts";

        export default async function chisel(req: Request) {
            let ret = "";
            const filtered = await Person.findMany({"foo": "bar"});
            filtered.forEach(row => {
                ret += row.first_name + " " + row.last_name + "\n";
            });
            return new Response(ret);
        }
    "##,
    );

    let err = chisel.apply().expect_err("chisel apply should have failed");
    err.stderr()
        .must_contain_ord("endpoints/query.ts:6:53 - error TS2769: No overload matches this call.")
        .must_contain_ord("Argument of type '{ foo: string; }' is not assignable to parameter of type 'Partial<Person>'");
}

#[tokio::test]
async fn test_find_by() {
    let (_db, mut chiseld, chisel) = setup_chiseld(OpMode::Deno, DatabaseConfig::Sqlite, true)
        .await
        .expect("failed to setup chiseld");

    chisel.copy("examples/person.ts", "models");
    chisel.copy("examples/find_by.ts", "endpoints");
    chisel.copy_and_rename("examples/store.ts", "endpoints/ins.ts");

    let r = chisel.apply().expect("chisel apply failed");
    r.stdout
        .must_contain("Model defined: Person")
        .must_contain("End point defined: /dev/find_by")
        .must_contain("End point defined: /dev/ins");

    chisel
        .post(
            "/dev/ins",
            json!({
                "first_name":"Glauber",
                "last_name":"Costa",
                "age": 666,
                "human": true,
                "height": 10.01
            }),
        )
        .await
        .unwrap();
    chisel
        .post(
            "/dev/ins",
            json!({
                "first_name":"Jan",
                "last_name":"Plhak",
                "age": -666,
                "human": true,
                "height": 10.02
            }),
        )
        .await
        .unwrap();

    let resp_txt = chisel
        .post(
            "/dev/find_by",
            json!({
                "field_name":"first_name",
                "value":"Jan"
            }),
        )
        .await
        .unwrap()
        .text()
        .await
        .unwrap();
    assert_eq!(resp_txt, "Jan Plhak -666 true 10.02 ");

    let resp_txt = chisel
        .post(
            "/dev/find_by",
            json!({
                "field_name":"last_name",
                "value":"Costa"
            }),
        )
        .await
        .unwrap()
        .text()
        .await
        .unwrap();
    assert_eq!(resp_txt, "Glauber Costa 666 true 10.01 ");

    let resp_txt = chisel
        .post(
            "/dev/find_by",
            json!({
                "field_name":"last_name",
                "value":"bagr"
            }),
        )
        .await
        .unwrap()
        .text()
        .await
        .unwrap();
    assert_eq!(resp_txt, "");

    let resp_txt = chisel
        .post(
            "/dev/find_by",
            json!({
                "field_name":"age",
                "value":-666
            }),
        )
        .await
        .unwrap()
        .text()
        .await
        .unwrap();
    assert_eq!(resp_txt, "Jan Plhak -666 true 10.02 ");

    let resp_txt = chisel
        .post(
            "/dev/find_by",
            json!({
                "field_name":"human",
                "value":true
            }),
        )
        .await
        .unwrap()
        .text()
        .await
        .unwrap();
    assert_eq!(
        resp_txt,
        "Glauber Costa 666 true 10.01 Jan Plhak -666 true 10.02 "
    );

    let resp_txt = chisel
        .post(
            "/dev/find_by",
            json!({
                "field_name":"height",
                "value":10.01
            }),
        )
        .await
        .unwrap()
        .text()
        .await
        .unwrap();
    assert_eq!(resp_txt, "Glauber Costa 666 true 10.01 ");

    let resp_txt = chisel
        .post(
            "/dev/find_by",
            json!({
                "field_name":"height",
            }),
        )
        .await
        .unwrap()
        .text()
        .await
        .unwrap();
    assert_eq!(
        resp_txt,
        "Glauber Costa 666 true 10.01 Jan Plhak -666 true 10.02 "
    );

    let r = chisel
        .post(
            "/dev/find_by",
            json!({
                "field_name":"misspelled_field_name",
                "value":10.01
            }),
        )
        .await;
    assert!(r.is_err());

    chiseld
        .stderr
        .must_contain(
            "Error: expression error: entity 'Person' doesn't have field 'misspelled_field_name'",
        )
        .await
        .unwrap();
}
