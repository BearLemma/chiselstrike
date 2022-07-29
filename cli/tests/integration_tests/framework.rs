use anyhow::{Context, Result};
use bytes::BytesMut;
use checked_command::CheckedCommand;
use rand::{distributions::Alphanumeric, Rng};
use std::fs;
use std::pin::Pin;
use std::process::Stdio;
use std::rc::Rc;
use std::time::Duration;
use tempdir::TempDir;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWriteExt};
use tokio::process::{Child as AsyncChild, Command as AsyncCommand};

#[derive(Clone, Debug)]
pub struct TestConfig {
    pub db_config: DatabaseConfig,
    pub optimize: bool,
}

pub enum OpMode {
    Deno,
    Node,
}

#[derive(Debug, Clone)]
pub struct PostgresConfig {
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
}

#[derive(Clone, Debug)]
pub struct ChiseldConfig {
    public_address: String,
    internal_address: String,
    rpc_address: String,
}

impl PostgresConfig {
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

#[derive(Debug, Clone)]
pub enum DatabaseConfig {
    Postgres(PostgresConfig),
    Sqlite,
}

pub enum Database {
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

pub struct GuardedChild {
    _child: AsyncChild,
    pub stdout: AsyncTestableOutput,
    pub stderr: AsyncTestableOutput,
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
    pub async fn show_output(&mut self) {
        self.stdout.show().await.unwrap();
        self.stderr.show().await.unwrap();
    }
}

#[derive(Debug)]
pub struct ProcessError {
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
    pub fn stdout(&self) -> TestableOutput {
        self.output.stdout.clone()
    }

    pub fn stderr(&self) -> TestableOutput {
        self.output.stderr.clone()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum OutputType {
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
pub struct MixedTestableOutput {
    pub stdout: TestableOutput,
    pub stderr: TestableOutput,
}

#[derive(Debug, Clone)]
pub struct TestableOutput {
    pub output_type: OutputType,
    pub output: String,
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

    pub fn must_contain_ord(&mut self, pattern: &str) -> &mut Self {
        if let Some(idx) = self.output[self.cursor..].find(pattern) {
            self.cursor = idx + pattern.len();
            self
        } else {
            let out_type = self.output_type.to_str();
            let output = &self.output;
            panic!("failed to find text in the {out_type}: {pattern}\nFull output:\n{output}");
        }
    }

    pub fn must_contain(self, pattern: &str) -> Self {
        assert!(self.output.contains(pattern));
        self
    }
}

pub struct AsyncTestableOutput {
    #[allow(dead_code)]
    pub output_type: OutputType,
    async_output: Pin<Box<dyn AsyncRead>>,
    pub raw_output: BytesMut,
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

    pub async fn must_contain(&mut self, pattern: &str) -> Result<()> {
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

    pub fn internal_must_contain(&mut self, pattern: &str) -> bool {
        let output = self.decoded_output();
        if let Some(idx) = output[self.cursor..].find(pattern) {
            self.cursor = idx + pattern.len();
            true
        } else {
            false
        }
    }

    pub fn decoded_output(&self) -> String {
        let colorless_output = strip_ansi_escapes::strip(&self.raw_output).unwrap();
        String::from_utf8(colorless_output).unwrap()
    }

    #[allow(dead_code)]
    pub async fn load_to_buffer(&mut self, timeout: core::time::Duration) {
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
    pub async fn show(&mut self) -> Result<()> {
        self.load_to_buffer(Duration::from_secs(1)).await;

        let mut stdout = tokio::io::stdout();
        stdout.write_all(&self.raw_output).await?;
        stdout.flush().await?;
        Ok(())
    }
}

pub struct Chisel {
    config: ChiseldConfig,
    tmp_dir: Rc<TempDir>,
}

impl Chisel {
    pub fn exec(&self, cmd: &str, args: &[&str]) -> Result<MixedTestableOutput, ProcessError> {
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

    pub fn apply(&self) -> Result<MixedTestableOutput, ProcessError> {
        self.exec("apply", &[])
    }

    pub fn write(&self, path: &str, code: &str) {
        let full_path = self.tmp_dir.path().join(path);
        fs::write(full_path, code).expect("Unable to write endpoint");
    }

    pub fn copy<P, Q>(&self, from: P, to: Q) -> u64
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

    pub fn copy_and_rename<P, Q>(&self, from: P, to: Q) -> u64
    where
        P: AsRef<std::path::Path> + std::fmt::Debug,
        Q: AsRef<std::path::Path> + std::fmt::Debug,
    {
        std::fs::copy(&from, self.tmp_dir.path().join(&to))
            .expect(&format!("failed to copy '{:?}' to '{:?}'", from, to))
    }

    pub async fn post(&self, url: &str, data: serde_json::Value) -> Result<reqwest::Response> {
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

    pub async fn post_text(&self, url: &str, data: serde_json::Value) -> String {
        self.post(url, data).await.unwrap().text().await.unwrap()
    }
}

pub fn bin_dir() -> std::path::PathBuf {
    let mut path = std::env::current_exe().unwrap();
    path.pop();
    path.pop();
    path
}

#[allow(dead_code)]
pub fn repo_dir() -> std::path::PathBuf {
    let mut path = bin_dir();
    path.pop();
    path.pop();
    path
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

pub async fn setup_chiseld(
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

impl TestConfig {
    pub async fn setup_env(self, mode: OpMode) -> TestContext {
        let (db, chiseld, chisel) = setup_chiseld(OpMode::Deno, DatabaseConfig::Sqlite, true)
            .await
            .expect("failed to setup chiseld");
        TestContext {
            mode,
            db,
            chisel,
            chiseld,
        }
    }
}

pub struct TestContext {
    pub mode: OpMode,
    db: Database,
    pub chiseld: GuardedChild,
    pub chisel: Chisel,
}

impl TestContext {
    pub fn get_chisels(&mut self) -> (&mut Chisel, &mut GuardedChild) {
        return (&mut self.chisel, &mut self.chiseld);
    }
}
