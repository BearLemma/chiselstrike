// SPDX-FileCopyrightText: © 2021 ChiselStrike <info@chiselstrike.com>
use crate::common::{bin_dir, run, Opt};
use structopt::StructOpt;

#[path = "../common/mod.rs"]
pub mod common;

mod framework;
mod lit;
mod native;
mod rust;

fn main() {
    // install the current packages in our package.json. This will make things like esbuild
    // generally available. Tests that want a specific extra package can then install on top
    run("npm", ["install"]);

    let opt = Opt::from_args();

    let bd = bin_dir();
    let mut args = vec!["build"];
    if bd.ends_with("release") {
        args.push("--release");
    }
    run("cargo", args);

    let ok_rust_tests = rust::run_tests(&opt);
    let ok_without_optimization = lit::run_tests(opt.clone(), false);
    let ok_with_optimization = lit::run_tests(opt.clone(), true);
    std::process::exit(
        if ok_with_optimization && ok_without_optimization && ok_rust_tests {
            0
        } else {
            1
        },
    );
}
