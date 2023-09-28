use crate::{BuildCtx, SparkBuildOptions};
use anyhow::anyhow;
use clap::Parser;
use elf::{Elf, SegmentKind, SymbolKind};
use std::{
    ffi::OsStr,
    fs::{self, File},
    io::{Seek, SeekFrom, Write},
    mem,
    path::{Path, PathBuf},
    process::Stdio,
};
use xtask::{concat_paths, process::CommandExt};

#[derive(Parser)]
pub struct Options {
    #[clap(flatten)]
    pub general_options: SparkBuildOptions,
}

impl core::ops::Deref for Options {
    type Target = SparkBuildOptions;

    fn deref(&self) -> &Self::Target {
        &self.general_options
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BuildCmd {
    Build,
    Check,
    Doc,
}

pub fn build(ctx: &BuildCtx, options: Options, cmd: BuildCmd) -> anyhow::Result<()> {
    let rustflags = format!(
        "--cfg {} {}",
        match options.target {
            crate::Target::riscv_sbi => "sbi",
            crate::Target::riscv_uefi => "uefi",
        },
        if options.verbose { "-v " } else { "" }
    );

    let envs: &[(&dyn AsRef<OsStr>, &dyn AsRef<OsStr>)] = &[(&"RUSTFLAGS", &rustflags)];

    // Format the source.
    ctx.cmds
        .cargo()
        .arg("fmt")
        .arg_if(options.verbose, "-vv")
        .arg_if(options.ci, "--check")
        .current_dir(&ctx.paths.spark_dir)
        .envs(envs.iter().copied())
        .execute()?;

    // Run cargo command.
    ctx.cmds
        .cargo()
        .args::<&[_], _>(match cmd {
            BuildCmd::Build => &["build"],
            BuildCmd::Check => &["clippy"],
            BuildCmd::Doc => &["doc", "--package", "spark", "--document-private-items"],
        })
        .args(["--profile", &ctx.rust_profile, "--target"])
        .arg(&ctx.rust_target)
        .arg("--target-dir")
        .arg(&ctx.paths.rust_target_dir)
        .arg_if(options.verbose, "-vv")
        .current_dir(&ctx.paths.spark_dir)
        .envs(envs.iter().copied())
        .execute()?;

    // Actually building the bootloader requires some more steps.
    if cmd == BuildCmd::Build {
        // Generate the symbol map.
        let spark_elf = link_spark(ctx, &options, None)?;
        let symbol_map = create_symbol_map(ctx, spark_elf)?;
        // Re-link the executable, this time with the symbol map.
        let spark_elf = link_spark(ctx, &options, Some(&symbol_map))?;

        // Create a flat binary
        let spark_bin = spark_elf.with_extension("bin");
        elf_to_binary(&spark_elf, &spark_bin)?;
    }

    Ok(())
}

fn link_spark(
    ctx: &BuildCtx,
    options: &Options,
    symbol_map: Option<&Path>,
) -> anyhow::Result<PathBuf> {
    let spark_lib = ctx.paths.rust_build_dir.join("libspark.a");
    let spark_elf = ctx.paths.build_dir.join("spark.elf");
    let linker_script =
        concat_paths!(ctx.paths.spark_dir, "conf", options.target.to_string()).with_extension("ld");

    let mut linker = ctx.cmds.linker();

    #[rustfmt::skip]
    linker.args([
        "-static",
        "-pie",
        "--no-dynamic-linker",
        "--whole-archive",
        "--gc-sections",
        "--eh-frame-hdr",
        // The linker will create `__start_SECTION_NAME` and `__stop_SECTION_NAME` symbols
        // for the beginning and end of each section, however we cannot access them from Rust
        // by default. `nostart-stop-gc` prevents the symbols from being removed, and allows
        // us to get access to our linker sets without specifying them all explicitly in
        // the linker script.
        "-znostart-stop-gc",
        "-zrelro",
        &format!("--script={}", linker_script.display()),
    ]).arg("-o").arg(&spark_elf)
    .arg(spark_lib);
    if let Some(symbol_map) = symbol_map {
        linker.arg(symbol_map);
    }
    linker.execute()?;

    Ok(spark_elf)
}

/// Generate a flat binary from an ELF executable
///
/// This function assumes that the ELF file was linked to `0x0`.
fn elf_to_binary(elf_path: &Path, bin_path: &Path) -> anyhow::Result<()> {
    let file_data = fs::read(elf_path)?;
    let elf = Elf::new(&file_data).unwrap();
    let mut out = File::create(bin_path)?;
    for sgmt in elf
        .segments()
        .filter(|sgmt| sgmt.kind() == SegmentKind::Load)
    {
        out.seek(SeekFrom::Start(sgmt.virtual_address()))?;
        out.write_all(sgmt.file_data())?;
    }
    Ok(())
}

fn create_symbol_map(ctx: &BuildCtx, elf_path: impl AsRef<Path>) -> anyhow::Result<PathBuf> {
    let buf = create_symbol_map_blob(elf_path)?;
    let map_bin = ctx.paths.build_dir.join("symbols.bin");
    let map_obj = ctx.paths.build_dir.join("symbols.o");
    fs::write(&map_bin, buf)?;
    rustc_create_obj_from_bin(
        ctx,
        "__symbol_map",
        r#".rodata.__symbol_map,"a",@progbits"#,
        map_bin,
        &map_obj,
    )?;
    Ok(map_obj)
}

mod symbol_map {
    include!("../../spark/raw_symbol_map.rs");
}

/// Generate a symbol map from an executable
fn create_symbol_map_blob(elf_path: impl AsRef<Path>) -> anyhow::Result<Box<[u8]>> {
    fn _create_symbol_map_blob(elf_path: &Path) -> anyhow::Result<Box<[u8]>> {
        use symbol_map::*;

        let file_data = fs::read(elf_path)?;
        let elf = Elf::new(&file_data).map_err(|err| anyhow!("{}: {err}", elf_path.display()))?;
        let symbol_table = elf
            .symbol_table()
            .ok_or_else(|| anyhow!("{} does not have a symbol table", elf_path.display()))?;

        let mut symbols = vec![];
        let mut names = String::new();

        for symbol in symbol_table.symbols() {
            if symbol.kind() != SymbolKind::Func {
                continue;
            }
            if let Some(name) = symbol.name() {
                let name_offset = names.len();
                let name = format!("{:#}", rustc_demangle::demangle(name));
                names.push_str(&name);
                symbols.push(SymbolRaw {
                    addr: symbol.value(),
                    size: symbol.size(),
                    name: u32::try_from(name_offset).unwrap(),
                    name_len: u32::try_from(name.len()).unwrap(),
                });
            }
        }

        // Sort by address.
        symbols.sort_unstable_by_key(|s| s.addr);

        let hdr_offset = 0;
        let hdr_size = mem::size_of::<SymbolMapHeader>();
        let sym_offset = hdr_offset + hdr_size;
        let sym_size = mem::size_of_val::<[SymbolRaw]>(&symbols);
        let str_offset = sym_offset + sym_size;
        let str_size = names.len();

        let total_size = hdr_size + sym_size + str_size;

        let header = SymbolMapHeader {
            magic: MAGIC,
            _reserved: 0,
            symbols_offset: u32::try_from(sym_offset).unwrap(),
            symbols_len: u32::try_from(sym_size).unwrap(),
            strings_offset: u32::try_from(str_offset).unwrap(),
            strings_len: u32::try_from(str_size).unwrap(),
        };

        let mut buf = vec![0u8; total_size].into_boxed_slice();

        unsafe {
            unsafe fn as_bytes<T>(val: &T) -> &[u8] {
                core::slice::from_raw_parts(val as *const T as *const u8, mem::size_of::<T>())
            }
            unsafe fn slice_as_bytes<T>(val: &[T]) -> &[u8] {
                core::slice::from_raw_parts(val as *const [T] as *const u8, mem::size_of_val(val))
            }

            buf[hdr_offset..][..hdr_size].copy_from_slice(as_bytes(&header));
            buf[sym_offset..][..sym_size].copy_from_slice(slice_as_bytes(&symbols));
            buf[str_offset..][..str_size].copy_from_slice(names.as_bytes());
        }

        Ok(buf)
    }
    _create_symbol_map_blob(elf_path.as_ref())
}

/// Generate an object file from a binary blob
///
/// The binary data will
fn rustc_create_obj_from_bin(
    ctx: &BuildCtx,
    name: &str,
    section_name: &str,
    bin_path: impl AsRef<Path>,
    obj_path: impl AsRef<Path>,
) -> anyhow::Result<()> {
    fn _rustc_create_obj_from_bin(
        ctx: &BuildCtx,
        name: &str,
        section: &str,
        bin_path: &Path,
        obj_path: &Path,
    ) -> anyhow::Result<()> {
        let mut rustc = ctx
            .cmds
            .rustc()
            .args(["+nightly", "--emit", "obj", "-", "--target"])
            .arg(&ctx.rust_target)
            .arg("-o")
            .arg(obj_path)
            .log_command()
            .stdin(Stdio::piped())
            .spawn()?;

        rustc.stdin.take().unwrap().write_all(
            format!(
                r##"
                #![allow(internal_features)]
                #![feature(no_core, rustc_attrs)]
                #![no_core]
                #![no_main]

                #[rustc_builtin_macro]
                macro_rules! global_asm {{ () => (); }}

                global_asm!(
                    r#"
                    .pushsection {section}
                    .global {name}
                    .p2align 4
                    {name}:
                    .incbin "{}"
                    .popsection
                    "#
                );
                "##,
                bin_path.display()
            )
            .as_bytes(),
        )?;

        rustc.wait()?.exit_ok()?;

        Ok(())
    }
    _rustc_create_obj_from_bin(
        ctx,
        name,
        section_name,
        bin_path.as_ref(),
        obj_path.as_ref(),
    )
}
