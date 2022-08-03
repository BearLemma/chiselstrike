// SPDX-FileCopyrightText: © 2021 ChiselStrike <info@chiselstrike.com>
#[macro_use]
extern crate test_macro;

use crate::common::{bin_dir, run, Opt};
use structopt::StructOpt;

#[path = "../common/mod.rs"]
pub mod common;

mod framework;
mod lit;
mod rust;
mod rust_tests;

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

    let run_results = vec![
        rust::run_tests(&opt),
        // lit::run_tests(opt.clone(), false),
        // lit::run_tests(opt.clone(), true),
    ];
    std::process::exit(if run_results.iter().all(|x| *x) { 0 } else { 1 });
}
