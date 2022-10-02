use std::env;

fn main() {
    let linker_script = env::var("SPARK_LINKER_SCRIPT").unwrap();
    println!("cargo:rustc-link-arg-bin=spark=--script={linker_script}");
    println!("cargo:rerun-if-changed={linker_script}");
}
