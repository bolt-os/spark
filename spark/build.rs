use std::env;

fn main() {
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap();
    if target_os == "uefi" {
        println!("cargo:rustc-cfg=uefi");
    } else {
        println!("cargo:rustc-cfg=sbi");
    }
}
