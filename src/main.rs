mod assembler;
mod commands;
mod i8008;
mod utils;

use i8008::*;
use utils::*;

use std::io;
use std::io::Write;
use std::path::PathBuf;

use ansi_term;
use clap::{Parser, ValueEnum};

#[derive(Clone, ValueEnum)]
enum Architecture {
    I8008,
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(long, help = "Target architecture")]
    arch: Architecture,

    #[arg(
        long = "assemble",
        help = "Input file to be assembled",
        requires = "output_path"
    )]
    assembly_path: Option<PathBuf>,

    #[arg(
        short = 'o',
        long = "out",
        help = "Output file's location",
        requires = "assembly_path"
    )]
    output_path: Option<PathBuf>,
}

fn run_i8008() {
    let mut cpu_sim: I8008 = I8008::new();
    let mut mem_controller: MemoryController = MemoryController::new();

    let mut address_store: u16 = 0x0000;

    loop {
        print!("> ");
        let _ = io::stdout().flush();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        match commands::parse_command(input) {
            Err(error) => println!("error: {}", error),
            Ok(command) => commands::run_command(
                command,
                &mut cpu_sim,
                &mut mem_controller,
                &mut address_store,
            ),
        }
    }
}

fn main() {
    let args: Args = Args::parse();
    match (args.assembly_path, args.arch) {
        (Some(path), Architecture::I8008) => {
            assembler::run_i8008_assembler(path, args.output_path.unwrap())
        }
        (None, Architecture::I8008) => run_i8008(),
    }
}
