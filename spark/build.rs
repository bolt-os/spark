use std::env;

fn main() {
    let linker_script = env::var("SPARK_LINKER_SCRIPT").unwrap();
    println!("cargo:rustc-link-arg-bin=spark=--script={linker_script}");

    // The linker will create `__start_SECTION_NAME` and `__stop_SECTION_NAME` symbols
    // for the beginning and end of each section, however we cannot access them from Rust
    // by default. `nostart-stop-gc` prevents the symbols from being removed, and allows
    // us to get access to our linker sets without specifying them all explicitly in
    // the linker script.
    println!("cargo:rustc-link-arg-bin=spark=-znostart-stop-gc");

    println!("cargo:rerun-if-changed={linker_script}");

    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap();
    if target_os == "uefi" {
        println!("cargo:rustc-cfg=uefi");
    } else {
        println!("cargo:rustc-cfg=sbi");
    }
}
