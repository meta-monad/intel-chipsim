use std::fs::File;
use std::io;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::process;

use crate::i8008::*;
use crate::utils::*;

macro_rules! equiv_select {
    ($v1:expr, $v2:expr) => {{
        if $v1 == $v2 {
            "(*)"
        } else {
            "   "
        }
    }};
}

/* macro - doesn't work because Rust can't generate new identifiers this way
macro_rules! gen_commands {
    ( $ ($name:ident, $help:expr, <$ ($field:ty) , *>) | +) => {
        enum Command2 {
            $( $name ($($field, )*) ,)*
        }

        impl Command2 {
            fn gen_help(&self) {
                $(
                    println!("{}: {}", $name, $help);
                )*
            }
        }
    }
}
*/

pub enum ParseError {
    UnknownCommand(String),
    UnexpectedArg(String),
    MissingArg(Vec<&'static str>),
    BadArg(String, Vec<&'static str>),
}

pub enum Command {
    Empty,
    State,
    Status,
    Ram,
    Step,
    Cycle,
    Address,
    SetLine(Line, bool),
    FullState,
    Quit,
    Help,
    Load(PathBuf),
}

pub enum Line {
    Interrupt,
    Ready,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ParseError::UnknownCommand(c) => write!(f, "unknown command '{}'", c),
            ParseError::UnexpectedArg(a) => write!(f, "unexpected argument '{}'", a),
            ParseError::MissingArg(va) => {
                write!(f, "missing argument, expected one of {}", va.join(", "))
            }
            ParseError::BadArg(a, va) => {
                write!(f, "bad argument '{}', expected one of {}", a, va.join(", "))
            }
        }
    }
}

fn end_of_args_parser(token: Option<&str>, command: Command) -> Result<Command, ParseError> {
    match token {
        Some(a) => Err(ParseError::UnexpectedArg(a.to_string())),
        None => Ok(command),
    }
}

fn parse_line(token: Option<&str>) -> Result<Line, ParseError> {
    let valid_tokens: Vec<&str> = vec!["interrupt", "int", "ready", "rdy"];
    match token {
        Some("interrupt") => Ok(Line::Interrupt),
        Some("int") => Ok(Line::Interrupt),
        Some("ready") => Ok(Line::Ready),
        Some("rdy") => Ok(Line::Ready),
        Some(a) => Err(ParseError::BadArg(a.to_string(), valid_tokens)),
        None => Err(ParseError::MissingArg(valid_tokens)),
    }
}

fn parse_line_value(token: Option<&str>, line: Line) -> Result<(Line, bool), ParseError> {
    let valid_tokens = vec!["high", "1", "low", "0"];
    match token {
        Some("high") => Ok((line, true)),
        Some("1") => Ok((line, true)),
        Some("low") => Ok((line, false)),
        Some("0") => Ok((line, false)),
        Some(a) => Err(ParseError::BadArg(a.to_string(), valid_tokens)),
        None => Err(ParseError::MissingArg(valid_tokens)),
    }
}

pub fn parse_command(input: String) -> Result<Command, ParseError> {
    // Ctrl-D will continue to behave funky
    let input_t = input.trim().to_string();
    let mut input_words: std::str::Split<'_, char> = input_t.split(' ');
    match input_words.next() {
        Some("state") => end_of_args_parser(input_words.next(), Command::State),
        Some("status") => end_of_args_parser(input_words.next(), Command::Status),
        Some("full_debug") => end_of_args_parser(input_words.next(), Command::FullState),
        Some("ram") => end_of_args_parser(input_words.next(), Command::Ram),
        Some("step") => end_of_args_parser(input_words.next(), Command::Step),
        Some("cycle") => end_of_args_parser(input_words.next(), Command::Cycle),
        Some("address") => end_of_args_parser(input_words.next(), Command::Address),
        Some("set_line") => {
            let line: Line = parse_line(input_words.next())?;
            let (line, value) = parse_line_value(input_words.next(), line)?;
            match input_words.next() {
                None => Ok(Command::SetLine(line, value)),
                Some(a) => Err(ParseError::UnexpectedArg(a.to_string())),
            }
        }
        Some("quit") | Some("exit") => end_of_args_parser(input_words.next(), Command::Quit),
        Some("help") => end_of_args_parser(input_words.next(), Command::Help),
        Some("load") => {
            let file: PathBuf = input_words
                .next()
                .ok_or(ParseError::MissingArg(vec!["FILE"]))?
                .into();
            match input_words.next() {
                None => Ok(Command::Load(file)),
                Some(a) => Err(ParseError::UnexpectedArg(a.to_string())),
            }
        }
        Some("") | None => Ok(Command::Empty),
        Some(c) => Err(ParseError::UnknownCommand(c.to_string())),
    }
}

pub fn run_command(
    command: Command,
    cpu_sim: &mut I8008,
    mem_controller: &mut MemoryController,
    address_store: &mut u16,
) {
    match command {
        Command::Empty => (),
        Command::State => println!("state: {:?}", cpu_sim.get_state()),
        Command::Status => {
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
        Command::Ram => {
            let width = 8;
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
        Command::Step => {
            let s: CpuState = *cpu_sim.get_state();
            step_with_mem(cpu_sim, mem_controller, address_store, s);
        }
        Command::Cycle => {
            let mut s: CpuState = *cpu_sim.get_state();
            while {
                println!(
                    "[DEBUG] running on cycle {:?}, state {:?} ",
                    *cpu_sim.get_cycle(),
                    s
                );
                step_with_mem(cpu_sim, mem_controller, address_store, s);
                s = *cpu_sim.get_state();
                s != CpuState::T1 && s != CpuState::STOPPED && s != CpuState::WAIT
            } {}
        }
        Command::Address => println!("{:#06X}", address_store),
        Command::SetLine(line, value) => match line {
            Line::Interrupt => cpu_sim.line_interrupt = value,
            Line::Ready => cpu_sim.line_ready = value,
        },
        Command::FullState => println!("{:#?}", cpu_sim),
        Command::Load(path) => {
            let mut f = File::open(path).expect("error: unable to open file");
            // read in bytecode
            let mut bytecode = Vec::new();
            let bytes_read = f
                .read_to_end(&mut bytecode)
                .expect("error: unable to read file");
            println!("[DEBUG] read in {} bytes", bytes_read);

            // load bytecode into memory
            if bytecode.len() > mem_controller.memory.len() {
                println!("error: failed loading into memory, file too large");
            } else {
                mem_controller.load_into(0, &bytecode);
            }
        }
        Command::Help => {
            println!("state: display the CPU's state, as indicated by the S0, S1, and S2 pins");
            println!("status: print out an overview of the CPU including all its registers, flags,\n\taddress stack, pin lines, databus, cycle, and internal state");
            println!("ram: display the contents of the RAM in hexadecimal");
            println!("step: make the chip process its current state");
            println!("cycle: make the chip process an entire cycle ending on the next T1");
            println!("address: display the address register of the RAM as is");
            println!("set_line LINE VALUE: sets an IO line of the chip to a value");
            println!("full_state: RESERVED FOR DEBUGGING, prints out full debug formatting of the internal I8008 struct");
            println!("quit: quit the program");
            println!("help: displays this message");
        }
        Command::Quit => process::exit(0),
    }
}
