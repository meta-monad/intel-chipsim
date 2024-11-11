mod i8008;
mod utils;
mod commands;
mod assembler;

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

    #[arg(long="assemble", help = "Input file to be assembled", requires="output_path")]
    assembly_path: Option<PathBuf>,

    #[arg(short='o', long="out", help = "Output file's location", requires="assembly_path")]
    output_path: Option<PathBuf>,
}

fn run_i8008() {
    let mut cpu_sim: I8008 = I8008::new();
    let mut mem_controller: MemoryController = MemoryController::new();

    let mut address_store: u16 = 0x0000;
    // populating the memory
    // first instruction is read in twice in this case
    /*
    mem_controller.load_into(
        0x00,
        &[
            I8008Ins::Lrr as u8 | 0x00, // Laa - 0xC0
            I8008Ins::INr as u8 | 0x08, // INb - 0x08
            I8008Ins::DCr as u8 | 0x18, // DCd - 0x19
            I8008Ins::Lrr as u8 | 0x23, // Led - 0xE3
            I8008Ins::LrM as u8 | 0x10, // LcM - 0xD7
            I8008Ins::LMr as u8 | 0x03, // LMd - 0xFB 
            I8008Ins::LrI as u8 | 0x08, // LbI - 0x0E
            0xab,
            I8008Ins::LMI as u8, // LMI - 0x3E
            0xbc,
            // I8008Ins::RST as u8 | 0x10, // RST 0x10
            I8008Ins::AOr as u8 | AluOp::OR as u8 | 0x01, // ORb - 0xB1
            I8008Ins::AOr as u8 | AluOp::SU as u8 | 0x01, // SUBb - 0x91
            I8008Ins::AOM as u8 | AluOp::SU as u8, // SUBM - 0x97
            I8008Ins::AOI as u8 | AluOp::AD as u8, // ADDI - 0x04 
            0x11,
            I8008Ins::RET as u8,
        ],
    );
    */
    loop {
        print!("> ");
        let _ = io::stdout().flush();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        match commands::parse_command(input) {
            Err(error) => println!("error: {}", error),
            Ok(command) => commands::run_command(command, &mut cpu_sim, &mut mem_controller, &mut address_store),
        }
    }
}

fn main() {
    let args: Args = Args::parse();
    match (args.assembly_path, args.arch) {
        (Some(path), Architecture::I8008) => assembler::run_i8008_assembler(path, args.output_path.unwrap()), 
        (None, Architecture::I8008) => run_i8008(),
    }
}
