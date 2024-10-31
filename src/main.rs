mod i8008;
use i8008::*;

use std::io;
use std::io::Write;

macro_rules! equiv_select {
    ($v1:expr, $v2:expr) => {{
        if $v1 == $v2 {
            "(*)"
        } else {
            "   "
        }
    }};
}

fn main() {
    let mut cpu_sim: I8008 = I8008::new();
    let mut mem_controller: I8008MemoryController = I8008MemoryController::new();

    let mut address_store: u16 = 0x0000;
    // populating the memory
    mem_controller.load_into(
        0x00,
        &[
            I8008Ins::Lrr as u8 | 0x00, // Laa == NOP -- this is read in twice, first for the
            // interrupt, second as the read after the interrupt
            I8008Ins::INr as u8 | 0x08, // INb
            I8008Ins::DCr as u8 | 0x18, // DCd
            I8008Ins::Lrr as u8 | 0x23, // Led
            I8008Ins::LrM as u8 | 0x10, // LcM
        ],
    );
    // 0xC0, 0x08, 0x19, 0xE3, 0xD7
    loop {
        print!("> ");
        let _ = io::stdout().flush();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        input = input.trim().to_string();
        let mut words = input.split(" ");

        match words.next() {
            Some("state") => {
                println!("state: {:?}", cpu_sim.get_state());
            }
            Some("step") => {
                let s: CpuState = *cpu_sim.get_state();
                step_with_mem(&mut cpu_sim, &mut mem_controller, &mut address_store, s);
            }
            Some("cycle") => {
                let mut s: CpuState = *cpu_sim.get_state();
                while s != CpuState::T1 && s != CpuState::STOPPED {
                    println!("[DEBUG] running a cycle on {:?}", s);
                    step_with_mem(&mut cpu_sim, &mut mem_controller, &mut address_store, s);
                    s = *cpu_sim.get_state();
                }
            }
            Some("address") => {
                println!("{:#06X}", address_store);
            }
            Some("set_line") => {
                let value: bool = match words.nth(2) {
                    Some("1") => true,
                    Some("high") => true,
                    Some("0") => false,
                    Some("low") => false,
                    Some(&_) => panic!("error: unexpected value"),
                    None => true,
                };
                match input.split(" ").nth(1) {
                    Some("interrupt") => cpu_sim.line_interrupt = value,
                    Some("ready") => cpu_sim.line_ready = value,
                    Some(&_) => panic!("error: unrecognized argument"),
                    None => panic!("error: expected an argument"),
                }
            }
            Some("databus") => match words.nth(1) {
                Some(val) => cpu_sim.databus = val.parse::<u8>().unwrap().reverse_bits(),
                None => panic!("error: expected an argument"),
            },
            Some("ram") => {
                let width = 2;
                for (i, elem) in mem_controller.memory.iter_mut().enumerate() {
                    match elem {
                        0x00 => {
                            println!("0x00 ...");
                            break;
                        }
                        byte => {
                            if i % width == width - 1 {
                                println!("{:#04X}", byte);
                            } else if i % width == 0 {
                                print!("{:#06X}: {:#04X} ", i, byte);
                            } else {
                                print!("{:#04X} ", byte);
                                let _ = io::stdout().flush();
                            }
                        }
                    }
                }
            }
            Some("status") => {
                let sp: u8 = cpu_sim.get_stack_pointer();
                println!("stack:    scratchpad:  internal:");
                println!(
                    "A|{:#04X}{} A|{:#04X}       A|{:#04X}",
                    cpu_sim.get_stack_register(0),
                    equiv_select!(sp, 0),
                    cpu_sim.get_scratchpad_register(false, false, false),
                    cpu_sim.get_register_a()
                );
                println!(
                    "B|{:#04X}{} B|{:#04X}       B|{:#04X}",
                    cpu_sim.get_stack_register(1),
                    equiv_select!(sp, 1),
                    cpu_sim.get_scratchpad_register(false, false, true),
                    cpu_sim.get_register_b()
                );
                println!(
                    "C|{:#04X}{} C|{:#04X}      IR|{:#04X} ",
                    cpu_sim.get_stack_register(2),
                    equiv_select!(sp, 2),
                    cpu_sim.get_scratchpad_register(false, true, false),
                    cpu_sim.get_instruction_register()
                );
                println!(
                    "D|{:#04X}{} D|{:#04X}",
                    cpu_sim.get_stack_register(3),
                    equiv_select!(sp, 3),
                    cpu_sim.get_scratchpad_register(false, true, true)
                );
                println!(
                    "E|{:#04X}{} E|{:#04X}",
                    cpu_sim.get_stack_register(4),
                    equiv_select!(sp, 4),
                    cpu_sim.get_scratchpad_register(true, false, false)
                );
                println!(
                    "F|{:#04X}{} L|{:#04X}",
                    cpu_sim.get_stack_register(5),
                    equiv_select!(sp, 5),
                    cpu_sim.get_scratchpad_register(true, false, true)
                );
                println!(
                    "G|{:#04X}{} H|{:#04X}",
                    cpu_sim.get_stack_register(6),
                    equiv_select!(sp, 6),
                    cpu_sim.get_scratchpad_register(true, true, false)
                );
                println!(
                    "H|{:#04X}{}",
                    cpu_sim.get_stack_register(7),
                    equiv_select!(sp, 7)
                );
                println!("");
                println!("flags:");
                if !cpu_sim.select_flag(false, false)
                    && !cpu_sim.select_flag(false, true)
                    && !cpu_sim.select_flag(true, false)
                    && !cpu_sim.select_flag(true, true)
                {
                    println!("(none set)")
                }
                if cpu_sim.select_flag(false, false) {
                    println!("CARRY")
                }
                if cpu_sim.select_flag(false, true) {
                    println!("ZERO")
                }
                if cpu_sim.select_flag(true, false) {
                    println!("SIGN")
                }
                if cpu_sim.select_flag(true, true) {
                    println!("PARITY")
                }
                println!();
                println!("cycle: {:?}", cpu_sim.get_cycle());
                println!("state: {:?}", cpu_sim.get_state());
                println!();
                println!("databus: {:#04X}", cpu_sim.databus);
                println!("ready line: {}", cpu_sim.line_ready);
                println!("interrupt line: {}", cpu_sim.line_interrupt);
            }
            Some("all") => println!("{:#?}", cpu_sim),
            Some("quit") | Some("") => break,
            Some(&_) => {
                println!("error: unknown command supplied");
            }
            None => (),
        }
    }
}
