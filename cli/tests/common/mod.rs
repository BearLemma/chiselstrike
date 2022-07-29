use std::env;
use std::ffi::OsString;
use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};
use std::path::PathBuf;
use std::process;
use std::str::FromStr;

use once_cell::sync::Lazy;

pub static CHISEL_BIN_DIR: Lazy<PathBuf> = Lazy::new(|| repo_dir().join(".chisel_dev"));
pub static CHISEL_LOCAL_PATH: Lazy<OsString> = Lazy::new(|| {
    let new_path = format!("{}/bin:{}", CHISEL_BIN_DIR.display(), env!("PATH"));
    OsString::from(new_path)
});

pub struct Command {
    pub inner: process::Command,
}

impl Drop for Command {
    fn drop(&mut self) {
        let out = self.inner.output().unwrap();
        let stderr = std::str::from_utf8(&out.stderr).unwrap();
        eprintln!("{stderr}");
        assert!(out.status.success());
    }
}

impl Deref for Command {
    type Target = process::Command;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Command {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[allow(dead_code)]
pub fn run_in<'a, T: IntoIterator<Item = &'a str>>(cmd: &str, args: T, dir: PathBuf) -> Command {
    assert!(
        dir.exists(),
        "{:?} does not exist. Current directory is {:?}",
        dir,
        env::current_dir().unwrap()
    );
    assert!(dir.is_dir(), "{:?} is not a directory", dir);
    let mut inner = process::Command::new(cmd);
    inner.args(args).current_dir(dir);

    inner.env("PATH", &*CHISEL_LOCAL_PATH);

    Command { inner }
}

#[allow(dead_code)]
pub fn run<'a, T: IntoIterator<Item = &'a str>>(cmd: &str, args: T) -> Command {
    run_in(cmd, args, repo_dir())
}

#[allow(dead_code)]
pub fn bin_dir() -> PathBuf {
    let mut path = env::current_exe().unwrap();
    path.pop();
    path.pop();
    path
}

#[allow(dead_code)]
pub fn chisel_exe() -> PathBuf {
    bin_dir().join("chisel")
}

#[allow(dead_code)]
pub fn repo_dir() -> PathBuf {
    let mut path = bin_dir();
    path.pop();
    path.pop();
    path
}

#[derive(Debug, Clone)]
pub enum Database {
    Postgres,
    Sqlite,
}

impl Display for Database {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Database::Postgres => write!(f, "postgres"),
            Database::Sqlite => write!(f, "sqlite"),
        }
    }
}

type ParseError = &'static str;

impl FromStr for Database {
    type Err = ParseError;

    fn from_str(database: &str) -> Result<Self, Self::Err> {
        match database {
            "postgres" => Ok(Database::Postgres),
            "sqlite" => Ok(Database::Sqlite),
            _ => Err("Unsupported database"),
        }
    }
}

use structopt::StructOpt;

#[derive(Debug, StructOpt, Clone)]
#[structopt(name = "lit_test", about = "Runs integration tests")]
pub(crate) struct Opt {
    /// Name of a signle lit test to run (e.g. `populate.lit`)
    #[structopt(short, long)]
    pub test: Option<String>,
    /// Database system to test with. Supported values: `sqlite` (default) and `postgres`.
    #[structopt(long, default_value = "sqlite")]
    pub database: Database,
    /// Database host name. Default: `localhost`.
    #[structopt(long, default_value = "localhost")]
    pub database_host: String,
    /// Database username.
    #[structopt(long)]
    pub database_user: Option<String>,
    /// Database password.
    #[structopt(long)]
    pub database_password: Option<String>,
}
