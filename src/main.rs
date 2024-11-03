mod i8008;
mod utils;
mod commands;

use i8008::*;
use utils::*;

use std::io;
use std::io::Write;

fn main() {
    let mut cpu_sim: I8008 = I8008::new();
    let mut mem_controller: MemoryController = MemoryController::new();

    let mut address_store: u16 = 0x0000;
    // populating the memory
    // first instruction is read in twice in this case
    mem_controller.load_into(
        0x00,
        &[
            I8008Ins::Lrr as u8 | 0x00, // Laa - 0xC0 
            I8008Ins::INr as u8 | 0x08, // INb - 0x08
            I8008Ins::DCr as u8 | 0x18, // DCd - 0x19
            I8008Ins::Lrr as u8 | 0x23, // Led - 0xE3
            I8008Ins::LrM as u8 | 0x10, // LcM - 0xD7
            I8008Ins::LMr as u8 | 0x03, // LMd - 0xFB 
            I8008Ins::AOr as u8 | AluOp::OR as u8 | 0x01, // ADDb - 0xB1
            I8008Ins::AOr as u8 | AluOp::SU as u8 | 0x01, // SUBb - 0x91
            I8008Ins::AOM as u8 | AluOp::SU as u8, // SUBM - 0x97
        ],
    );
    // 0xC0, 0x08, 0x19, 0xE3, 0xD7
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
