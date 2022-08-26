pub mod build;
pub mod runner;

use clap::{AppSettings, Parser};

#[derive(Parser)]
#[clap(rename_all = "snake_case", setting = AppSettings::DisableVersionFlag)]
enum Arguments {
    Build(build::Options),
    Run(runner::Options),
}

fn main() -> Result<(), xshell::Error> {
    match Arguments::parse() {
        Arguments::Build(build_options) => build::build(build_options),
        Arguments::Run(run_options) => runner::run(run_options),
    }
}
