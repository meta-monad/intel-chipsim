use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;

use ansi_term::{Color, Style};
use std::collections::HashMap;

use crate::i8008::AluOp;
use crate::i8008::I8008Ins;

// unexp_operand insert a return, this is not possible with an inline function
macro_rules! unexp_operand {
    ($t:expr) => {{
        match $t {
            Some(w) => return Err(ParseError::UnexpectedOperand(w.to_string())),
            None => ()
        }
    }};
}

#[derive(Debug)]
enum Instruction {
    Nop,
    MoveRegReg(Register, Register),
    MoveRegMem(Register),
    MoveMemReg(Register),
    MoveRegValue(Register, u8),
    MoveMemValue(u8),
    Inc(Register),
    Dec(Register),
    AluOpReg(AluOp, Register),
    AluOpMem(AluOp),
    AluOpValue(AluOp, u8),
    RotateLeftCarry,
    RotateRightCarry,
    RotateAroundLeft,
    RotateAroundRight,
    Jump(Address),
    JumpFalse(Condition, Address),
    JumpTrue(Condition, Address),
    Call(Address),
    CallFalse(Condition, Address),
    CallTrue(Condition, Address),
    Return,
    ReturnFalse(Condition),
    ReturnTrue(Condition),
    Restart(u8),
    Input(u8),
    Output(u8),
    Halt,
    DefineByte(u8),
    Space(u8),
    Label(String), // assembler directive
}

#[derive(Debug)]
enum Address {
    RawAddress(u16),
    Label(String),
}

enum Operand {
    Register(Register),
    Memory,
    RawByte(u8),
    RawAddress(u16),
    Label(String),
}

#[derive(Debug)]
enum Register {
    // no offset
    RegA = 0x00,
    RegB = 0x01,
    RegC = 0x02,
    RegD = 0x03,
    RegE = 0x04,
    RegH = 0x05,
    RegL = 0x06,
}

#[derive(Debug)]
enum Condition {
    Carry = 0x00, // XX X00 XXX
    Zero = 0x08, // XX X01 XXX
    Sign = 0x10, // XX X10 XXX
    Parity = 0x18, // XX X11 XXX
}

enum ParseError {
    InvalidInstruction(String),
    UnexpectedOperand(String),
    MissingOperand(Vec<&'static str>),
    BadOperand(String, Vec<&'static str>),
    UnknownLabel(String),
    DuplicateLabel(String),
}

enum Resolvable {
    Resolved(u8),
    Unresolved(String),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ParseError::InvalidInstruction(w) => {
                write!(f, "invalid instruction '{}'", Style::new().bold().paint(w))
            }
            ParseError::UnexpectedOperand(w) => {
                write!(f, "unexpected operand '{}'", Style::new().bold().paint(w))
            }
            ParseError::MissingOperand(expected) => write!(
                f,
                "missing operand, expected one of: {}",
                expected.join(", ")
            ),
            ParseError::BadOperand(w, expected) => write!(
                f,
                "bad operand '{}', expected one of: {}",
                Style::new().bold().paint(w),
                expected.join(", ")
            ),
            ParseError::UnknownLabel(l) => write!(
                f,
                "unknown label '{}'",
                Style::new().bold().paint(l),
            ),
            ParseError::DuplicateLabel(l) => write!(
                f,
                "label '{}' appears more than twice",
                Style::new().bold().paint(l),
            ),
        }
    }
}

#[inline(always)]
fn move_to_ddd(val: u8) -> u8 {
    val << 3
}

// dummy trait to attach to Vec<Resolvable>
trait VecResolve {
    fn push_resolved(&mut self, item: u8);
}

impl VecResolve for Vec<Resolvable> {
    fn push_resolved(&mut self, item: u8 ) {
        // shorthand for less typing
        self.push(Resolvable::Resolved(item));
    }
}

fn eol_parser(token: Option<&str>, ins: Instruction) -> Result<Option<Instruction>, ParseError> {
    match token {
        Some(w) => Err(ParseError::UnexpectedOperand(w.to_string())),
        None => Ok(Some(ins)),
    }
}

fn parse_operand(operand: &String) -> Option<Operand> {
    match operand.as_str() {
        "a" => Some(Operand::Register(Register::RegA)),
        "b" => Some(Operand::Register(Register::RegB)),
        "c" => Some(Operand::Register(Register::RegC)),
        "d" => Some(Operand::Register(Register::RegD)),
        "e" => Some(Operand::Register(Register::RegE)),
        "h" => Some(Operand::Register(Register::RegH)),
        "l" => Some(Operand::Register(Register::RegL)),
        "m" => Some(Operand::Memory),
        e => match &e[0..2] {
            "0x" => match (u16::from_str_radix(&e[2..], 16), e.len()) {
                (Ok(val), 4) => Some(Operand::RawByte(val as u8)),
                (Ok(val), 6) => Some(Operand::RawAddress(val)),
                (Ok(_), _) => None,
                (Err(_), _) => None,
            },
            _ => {
                if e.chars().all(|c| c.is_alphanumeric()) {
                    Some(Operand::Label(e.to_string()))
                } else {
                    None
                }
            },
        },
    }
}

fn parse_alu_op(op: &str) -> Option<AluOp> {
    match op {
        "add" => Some(AluOp::AD),
        "addc" => Some(AluOp::AC),
        "sub" => Some(AluOp::SU),
        "subc" => Some(AluOp::SB),
        "and" => Some(AluOp::ND),
        "xor" => Some(AluOp::XR),
        "or" => Some(AluOp::OR),
        "cmp" => Some(AluOp::CP),
        _ => None,
    }
}

fn parse_condition_word(word: &str) -> Option<(Condition, bool)> {
    // pattern matching on both sets allows the reuse of the function
    // this is desirable since we only care about the Condition produced
    match word {
        "jc" | "cc" | "rc"=> Some((Condition::Carry, true)),
        "jnc" | "cnc" | "rnc"=> Some((Condition::Carry, false)),
        "jz" | "cz" | "rz"=> Some((Condition::Zero, true)),
        "jnz" | "cnz" | "rnz" => Some((Condition::Zero, false)),
        "js" | "cs" | "rs" => Some((Condition::Sign, true)),
        "jns" | "cns" | "rns" => Some((Condition::Sign, false)),
        "jp" | "cp" | "rp" => Some((Condition::Parity, true)),
        "jnp" | "cnp" | "rnp" => Some((Condition::Parity, false)),
        _ => None,
    }
}

fn parse_line(line: String) -> Result<Option<Instruction>, ParseError> {
    let line_t = line.split(';').nth(0).unwrap_or(&line).trim();
    let mut input_words = line_t.split(' ');
    // let argc: usize = input_words.clone().collect::<Vec<&str>>().len();

    // idea: branch arms still largely repeat:
    //     especially for mov, inc, dec, hlt, rst, db, space
    // 1. take [n] operands
    //  1.1. .next() on [iterator]
    //  1.2. unwrap with .ok_or()? to send missing operand error up
    // 2. parse operands 
    // 3. early return on unexpected operands with unexp_operand! macro
    // 4. pattern match on operands
    //  4.1 if [n]>1, form n-tuples
    //   4.1.1 then guard pattern on correct i-th operand => send error with (i+1)-th operand
    //  4.2 else, throw error
    //
    //  no-operand (nop, rlc, rrc, ral, rar) instructions already solved with eol_parser
 
    match input_words.next() {
        Some("nop") => eol_parser(
            input_words.next(),
            Instruction::Nop,
        ),
        Some("mov") => {
            let move_operands = vec!["a", "b", "c", "d", "e", "h", "l", "m", "0x[..]"];
            let fst: String = input_words
                .next()
                .ok_or(ParseError::MissingOperand(move_operands.clone()))?
                .to_string();
            let snd: String = input_words
                .next()
                .ok_or(ParseError::MissingOperand(move_operands.clone()))?
                .to_string();
            let fst_op = parse_operand(&fst);
            let snd_op = parse_operand(&snd);

            unexp_operand!(input_words.next());
            match (fst_op, snd_op) {
                (Some(Operand::Register(r1)), Some(Operand::Register(r2))) => {
                    Ok(Some(Instruction::MoveRegReg(r1, r2)))
                }
                (Some(Operand::Register(r)), Some(Operand::Memory)) => {
                    Ok(Some(Instruction::MoveRegMem(r)))
                }
                (Some(Operand::Memory), Some(Operand::Register(r))) => {
                    Ok(Some(Instruction::MoveMemReg(r)))
                }
                (Some(Operand::Register(r)), Some(Operand::RawByte(b))) => {
                    Ok(Some(Instruction::MoveRegValue(r, b)))
                }
                (Some(Operand::Memory), Some(Operand::RawByte(b))) => {
                    Ok(Some(Instruction::MoveMemValue(b)))
                }
                (Some(Operand::Register(_)), _) | (Some(Operand::Memory), _) => {
                    Err(ParseError::BadOperand(snd, move_operands))
                }
                (..) => Err(ParseError::BadOperand(fst, move_operands)),
            }
        },
        Some("inc") => {
            let inc_operands = vec!["a", "b", "c", "d", "e", "h", "l"];
            let fst = input_words.next().ok_or(ParseError::MissingOperand(inc_operands.clone()))?.to_string();
            let fst_op = parse_operand(&fst);

            unexp_operand!(input_words.next());
            match fst_op {
                Some(Operand::Register(r)) => Ok(Some(Instruction::Inc(r))),
                _ => Err(ParseError::BadOperand(fst, inc_operands))
            }
        },
        Some("dec") => {
            let dec_operands = vec!["a", "b", "c", "d", "e", "h", "l"];
            let fst = input_words.next().ok_or(ParseError::MissingOperand(dec_operands.clone()))?.to_string();
            let fst_op = parse_operand(&fst);
            
            unexp_operand!(input_words.next());
            match fst_op {
                Some(Operand::Register(r)) => Ok(Some(Instruction::Dec(r))),
                _ => Err(ParseError::BadOperand(fst, dec_operands))
            }
        },
        Some(op @ "add") | Some(op @ "addc") | Some(op @ "sub") | Some(op @ "subc") | Some(op @ "and") | Some(op @ "xor") | Some(op @ "or") | Some(op @ "cmp") => {
            let alu_operands = vec!["a", "b", "c", "d", "e", "h", "l", "m", "0x[..]"];
            let fst = input_words.next().ok_or(ParseError::MissingOperand(alu_operands.clone()))?.to_string();
            let fst_op = parse_operand(&fst);

            let op_enum: AluOp = parse_alu_op(op).unwrap();

            unexp_operand!(input_words.next());
            match fst_op {
                Some(Operand::Register(r)) => Ok(Some(Instruction::AluOpReg(op_enum, r))),
                Some(Operand::Memory) => Ok(Some(Instruction::AluOpMem(op_enum))),
                Some(Operand::RawByte(b)) => Ok(Some(Instruction::AluOpValue(op_enum, b))),
                _ => Err(ParseError::BadOperand(fst, alu_operands))
            }
        }
        Some("rlc") => eol_parser(
            input_words.next(),
            Instruction::RotateLeftCarry,
        ),
        Some("rrc") => eol_parser(
            input_words.next(),
            Instruction::RotateRightCarry,
        ),
        Some("ral") => eol_parser(
            input_words.next(),
            Instruction::RotateAroundLeft,
        ),
        Some("rar") => eol_parser(
            input_words.next(),
            Instruction::RotateAroundRight,
        ),
        Some("jmp") => {
            let valid_operands = vec!["0x[....]", "label"];

            let fst = input_words.next().ok_or(ParseError::MissingOperand(valid_operands.clone()))?.to_string();
            let fst_op = parse_operand(&fst);

            unexp_operand!(input_words.next());
            match fst_op {
                Some(Operand::RawAddress(a)) => Ok(Some(Instruction::Jump(Address::RawAddress(a)))),
                Some(Operand::Label(l)) => Ok(Some(Instruction::Jump(Address::Label(l)))),
                _ => Err(ParseError::BadOperand(fst, valid_operands)),
            }
        },
        Some(cond_word @ "jc") | Some(cond_word @ "jz") | Some(cond_word @ "js") | Some(cond_word @ "jp") | 
        Some(cond_word @ "jnc") | Some(cond_word @ "jnz") | Some(cond_word @ "jns") | Some(cond_word @ "jnp") => {
            let valid_operands = vec!["0x[....]", "label"];

            let fst = input_words.next().ok_or(ParseError::MissingOperand(valid_operands.clone()))?.to_string();
            let fst_op = parse_operand(&fst);
            
            let (condition, truth) = parse_condition_word(cond_word).unwrap();
            unexp_operand!(input_words.next());
            match (fst_op, truth) {
                (Some(Operand::RawAddress(a)), false) => Ok(Some(Instruction::JumpFalse(condition, Address::RawAddress(a)))),
                (Some(Operand::RawAddress(a)), true) => Ok(Some(Instruction::JumpTrue(condition, Address::RawAddress(a)))),
                (Some(Operand::Label(l)), false) => Ok(Some(Instruction::JumpFalse(condition, Address::Label(l)))),
                (Some(Operand::Label(l)), true) => Ok(Some(Instruction::JumpTrue(condition, Address::Label(l)))),
                _ => Err(ParseError::BadOperand(fst, valid_operands)),
            }
        },
        Some("call") => {
            let valid_operands = vec!["0x[....]", "label"];

            let fst = input_words.next().ok_or(ParseError::MissingOperand(valid_operands.clone()))?.to_string();
            let fst_op = parse_operand(&fst);

            unexp_operand!(input_words.next());
            match fst_op {
                Some(Operand::RawAddress(a)) => Ok(Some(Instruction::Call(Address::RawAddress(a)))),
                Some(Operand::Label(l)) => Ok(Some(Instruction::Call(Address::Label(l)))),
                _ => Err(ParseError::BadOperand(fst, valid_operands)),
            }
        },
        Some(cond_word @ "cc") | Some(cond_word @ "cz") | Some(cond_word @ "cs") | Some(cond_word @ "cp") | 
        Some(cond_word @ "cnc") | Some(cond_word @ "cnz") | Some(cond_word @ "cns") | Some(cond_word @ "cnp") => {
            let valid_operands = vec!["0x[....]", "label"];

            let fst = input_words.next().ok_or(ParseError::MissingOperand(valid_operands.clone()))?.to_string();
            let fst_op = parse_operand(&fst);
            
            let (condition, truth) = parse_condition_word(cond_word).unwrap();
            unexp_operand!(input_words.next());
            match (fst_op, truth) {
                (Some(Operand::RawAddress(a)), false) => Ok(Some(Instruction::CallFalse(condition, Address::RawAddress(a)))),
                (Some(Operand::RawAddress(a)), true) => Ok(Some(Instruction::CallTrue(condition, Address::RawAddress(a)))),
                (Some(Operand::Label(l)), false) => Ok(Some(Instruction::CallFalse(condition, Address::Label(l)))),
                (Some(Operand::Label(l)), true) => Ok(Some(Instruction::CallTrue(condition, Address::Label(l)))),
                _ => Err(ParseError::BadOperand(fst, valid_operands)),
            }
        },
        Some("ret") => eol_parser(
            input_words.next(),
            Instruction::Return,
        ),
        Some(cond_word @ "rc") | Some(cond_word @ "rz") | Some(cond_word @ "rs") | Some(cond_word @ "rp") | 
        Some(cond_word @ "rnc") | Some(cond_word @ "rnz") | Some(cond_word @ "rns") | Some(cond_word @ "rnp") => {
            
            let (condition, truth) = parse_condition_word(cond_word).unwrap();
            unexp_operand!(input_words.next());
            match truth {
                true => Ok(Some(Instruction::ReturnFalse(condition))),
                false => Ok(Some(Instruction::ReturnTrue(condition))),
            }
        },

        Some("rst") => {
            let fst = input_words.next().ok_or(ParseError::MissingOperand(vec!["0x[..]"]))?.to_string();
            let fst_op = parse_operand(&fst);
            
            unexp_operand!(input_words.next());
            match fst_op {
                Some(Operand::RawByte(v)) => Ok(Some(Instruction::Restart(v))),
                _ => Err(ParseError::BadOperand(fst, vec!["0x[..]"])),
            }
        },
        Some("hlt") => eol_parser(
            input_words.next(),
            Instruction::Halt,
        ),
        Some("label") => {
            let fst = input_words.next().ok_or(ParseError::MissingOperand(vec!["0x[..]"]))?.to_string();
            let fst_op = parse_operand(&fst);
            
            unexp_operand!(input_words.next());
            match fst_op {
                Some(Operand::Label(l)) => Ok(Some(Instruction::Label(l))),
                _ => Err(ParseError::BadOperand(fst, vec!["label"]))
            }

        }
        Some("db") => {
            let fst = input_words.next().ok_or(ParseError::MissingOperand(vec!["0x[..]"]))?.to_string();
            let fst_op = parse_operand(&fst);
            
            unexp_operand!(input_words.next());
            match fst_op {
                Some(Operand::RawByte(b)) => Ok(Some(Instruction::DefineByte(b))),
                _ => Err(ParseError::BadOperand(fst, vec!["0x[..]"]))
            }
        }
        Some("space") => {
            let fst = input_words.next().ok_or(ParseError::MissingOperand(vec!["0x[..]"]))?.to_string();
            let fst_op = parse_operand(&fst);
            
            unexp_operand!(input_words.next());
            match fst_op {
                Some(Operand::RawByte(b)) => Ok(Some(Instruction::Space(b))),
                _ => Err(ParseError::BadOperand(fst, vec!["0x[..]"]))
            }
        }
        Some("") => Ok(None),
        Some(ins) => Err(ParseError::InvalidInstruction(ins.to_string())),
        None => Ok(None),
    }
}

fn instructions_to_hex(instructions: Vec<Instruction>, filename: &str) -> Result<Vec<u8>, ParseError> {
    let mut program_pass_one: Vec<Resolvable> = Vec::new();
    let mut program: Vec<u8> = Vec::new();
    let mut label_hashmap: HashMap<String, u16> = HashMap::new();

    // first pass
    for instruction in instructions {
        match instruction {
            Instruction::Nop => program_pass_one.push_resolved(I8008Ins::Lrr as u8 | 0x00),
            // Laa as NOP 

            Instruction::MoveRegReg(r1, r2) => program_pass_one.push_resolved(I8008Ins::Lrr as u8 | move_to_ddd(r1 as u8) | r2 as u8),
            // 11 DDD SSS

            Instruction::MoveRegMem(r) => program_pass_one.push_resolved(I8008Ins::LrM as u8 | move_to_ddd(r as u8)),
            // 11 DDD 111

            Instruction::MoveMemReg(r) => program_pass_one.push_resolved(I8008Ins::LMr as u8 | r as u8),
            // 11 111 SSS

            Instruction::MoveRegValue(r, b) => {
                program_pass_one.push_resolved(I8008Ins::LrI as u8 | move_to_ddd(r as u8));
                program_pass_one.push_resolved(b);
            }
            // 11 DDD 110
            // BB BBB BBB

            Instruction::MoveMemValue(b) => {
                program_pass_one.push_resolved(I8008Ins::LMI as u8);
                program_pass_one.push_resolved(b)
            }
            // 11 111 110
            // BB BBB BBB

            Instruction::Inc(r) => program_pass_one.push_resolved(I8008Ins::INr as u8 | move_to_ddd(r as u8)),
            // 00 DDD 000

            Instruction::Dec(r) => program_pass_one.push_resolved(I8008Ins::DCr as u8 | move_to_ddd(r as u8)),
            // 00 DDD 001
            
            Instruction::AluOpReg(op, r) => program_pass_one.push_resolved(I8008Ins::AOr as u8 | op as u8 | r as u8),
            // 10 OOO SSS

            Instruction::AluOpMem(op) => program_pass_one.push_resolved(I8008Ins::AOM as u8 | op as u8),
            // 10 OOO 111

            Instruction::AluOpValue(op, b) => {
                program_pass_one.push_resolved(I8008Ins::AOI as u8 | op as u8);
                program_pass_one.push_resolved(b);
            }
            // 00 OOO 100
            // BB BBB BBB

            Instruction::RotateLeftCarry => program_pass_one.push_resolved(I8008Ins::RLC as u8),
            // 00 000 010

            Instruction::RotateRightCarry => program_pass_one.push_resolved(I8008Ins::RRC as u8),
            // 00 001 010

            Instruction::RotateAroundLeft => program_pass_one.push_resolved(I8008Ins::RAL as u8),
            // 00 010 010

            Instruction::RotateAroundRight => program_pass_one.push_resolved(I8008Ins::RAR as u8),
            // 00 011 010

            Instruction::Jump(Address::RawAddress(a)) => {
                program_pass_one.push_resolved(I8008Ins::JMP as u8);
                // lower half of the address comes first
                program_pass_one.push_resolved((a & 0x00FF) as u8);
                program_pass_one.push_resolved(((a & 0xFF00) >> 8) as u8);
                if a & 0xC000 != 0x0000 {
                    println!("{}: {}: two highest bits of address in jump instruction are set, but the intel 8008 can only address 14 bits of memory",
                        Style::new()
                            .bold()
                            .paint(filename),
                        Color::Yellow.paint("warning"),
                    );
                }
            },
            Instruction::Jump(Address::Label(l)) => {
                program_pass_one.push_resolved(I8008Ins::JMP as u8);

                program_pass_one.push(Resolvable::Unresolved(l));
                program_pass_one.push(Resolvable::Unresolved(String::new()));
                // push dummy empty reference in order to reserve the space as keep
                // the program counter equal to program.len()
            }
            // 01 XXX 100
            // BB BBB BBB
            // BB BBB BBB

            Instruction::JumpFalse(condition, Address::RawAddress(a)) => {
                program_pass_one.push_resolved(I8008Ins::JFc as u8 | condition as u8);
                program_pass_one.push_resolved((a & 0x00FF) as u8);
                program_pass_one.push_resolved(((a & 0xFF00) >> 8) as u8);

                if a & 0xC000 != 0x0000 {
                    println!("{}: {}: two highest bits of address in jump instruction are set, but the intel 8008 can only address 14 bits of memory",
                        Style::new()
                            .bold()
                            .paint(filename),
                        Color::Yellow.paint("warning"),
                    );
                }
            }
            Instruction::JumpFalse(condition, Address::Label(l)) => {
                program_pass_one.push_resolved(I8008Ins::JFc as u8 | condition as u8);
                program_pass_one.push(Resolvable::Unresolved(l));
                program_pass_one.push(Resolvable::Unresolved(String::new()));
            }
            // 01 0CC 000
            // BB BBB BBB
            // BB BBB BBB

            Instruction::JumpTrue(condition, Address::RawAddress(a)) => {
                program_pass_one.push_resolved(I8008Ins::JTc as u8 | condition as u8);
                program_pass_one.push_resolved((a & 0x00FF) as u8);
                program_pass_one.push_resolved(((a & 0xFF00) >> 8) as u8);

                if a & 0xC000 != 0x0000 {
                    println!("{}: {}: two highest bits of address in jump instruction are set, but the intel 8008 can only address 14 bits of memory",
                        Style::new()
                            .bold()
                            .paint(filename),
                        Color::Yellow.paint("warning"),
                    );
                }
            }
            Instruction::JumpTrue(condition, Address::Label(l)) => {
                program_pass_one.push_resolved(I8008Ins::JTc as u8 | condition as u8);
                program_pass_one.push(Resolvable::Unresolved(l));
                program_pass_one.push(Resolvable::Unresolved(String::new()));
            }
            // 01 1CC 000
            // BB BBB BBB
            // BB BBB BBB

            Instruction::Call(Address::RawAddress(a)) => {
                program_pass_one.push_resolved(I8008Ins::CAL as u8);
                program_pass_one.push_resolved((a & 0x00FF) as u8);
                program_pass_one.push_resolved(((a & 0xFF00) >> 8) as u8);
            },
            Instruction::Call(Address::Label(l)) => {
                program_pass_one.push_resolved(I8008Ins::CAL as u8);
                program_pass_one.push(Resolvable::Unresolved(l));
                program_pass_one.push(Resolvable::Unresolved(String::new()));
            }
            // 01 XXX 110
            // BB BBB BBB
            // BB BBB BBB

            Instruction::CallFalse(condition, Address::RawAddress(a)) => {
                program_pass_one.push_resolved(I8008Ins::CFc as u8 | condition as u8);
                program_pass_one.push_resolved((a & 0x00FF) as u8);
                program_pass_one.push_resolved(((a & 0xFF00) >> 8) as u8);
            }
            Instruction::CallFalse(condition, Address::Label(l)) => {
                program_pass_one.push_resolved(I8008Ins::CFc as u8 | condition as u8);
                program_pass_one.push(Resolvable::Unresolved(l));
                program_pass_one.push(Resolvable::Unresolved(String::new()));
            }
            // 01 0CC 010
            // BB BBB BBB
            // BB BBB BBB

            Instruction::CallTrue(condition, Address::RawAddress(a)) => {
                program_pass_one.push_resolved(I8008Ins::CTc as u8 | condition as u8);
                program_pass_one.push_resolved((a & 0x00FF) as u8);
                program_pass_one.push_resolved(((a & 0xFF00) >> 8) as u8);
            }
            Instruction::CallTrue(condition, Address::Label(l)) => {
                program_pass_one.push_resolved(I8008Ins::CTc as u8 | condition as u8);
                program_pass_one.push(Resolvable::Unresolved(l));
                program_pass_one.push(Resolvable::Unresolved(String::new()));
            }
            // 01 1CC 010
            // BB BBB BBB
            // BB BBB BBB

            Instruction::Return => {
                program_pass_one.push_resolved(I8008Ins::RET as u8);
            }
            // 00 XXX 111 == 00 000 111

            Instruction::ReturnFalse(condition) => {
                program_pass_one.push_resolved(I8008Ins::RFc as u8 | condition as u8);
            }
            // 00 0CC 011

            Instruction::ReturnTrue(condition) => {
                program_pass_one.push_resolved(I8008Ins::RTc as u8 | condition as u8);
            }
            // 00 1CC 011

            Instruction::Restart(a) => {
                program_pass_one.push_resolved(I8008Ins::RST as u8 | a);
            }

            Instruction::Halt => program_pass_one.push_resolved(0x00),
            // 00 000 00X
            // or
            // 11 111 111

            Instruction::Label(l) => {
                if label_hashmap.contains_key(&l) {
                    return Err(ParseError::DuplicateLabel(l));
                } else {
                    label_hashmap.insert(l, program_pass_one.len() as u16);
                }
            }
            // assembler directive - places a key in the lookup table or raises error if the label
            // is a duplicate

            Instruction::DefineByte(b) => {
                program_pass_one.push_resolved(b);
            }
            // assembler directive - puts a single byte into the program

            Instruction::Space(num) => {
                for _ in 0..num {
                    program_pass_one.push_resolved(0x00); // HLT
                }
            }
            // assembler directive - repeats 00 (i.e. a Halt instruction) a given amount of times

            _ => panic!("error: unimplemented translation for assembly instruction"),

        };
    }
    
    // second pass
    // resolve unresolved label references
    for instruction in program_pass_one {
        match instruction {
            Resolvable::Resolved(b) => program.push(b), 
            Resolvable::Unresolved(label) => match label.as_str() {
                "" => (),
                _ => {
                    match label_hashmap.get(&label) {
                        Some(address) => {
                            program.push((address & 0x00FF) as u8);
                            program.push(((address & 0xFF00) >> 8) as u8);
                        },
                        None => return Err(ParseError::UnknownLabel(label)),
                    }
                }
            }
        };
    }
    Ok(program)
}

pub fn run_i8008_assembler(assembly_path: PathBuf, output_path: PathBuf) {
    let mut f: File = File::open(assembly_path.as_path()).expect("error: unable to open file");
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("error: unable to read file");
    let instructions_raw: std::str::Lines<'_> = contents.lines();

    let mut instructions_parse: Vec<Instruction> = Vec::new();
    let mut errors: Vec<(usize, ParseError)> = Vec::new();

    for (line_n, line) in instructions_raw.enumerate() {
        match parse_line(line.to_string()) {
            Ok(Some(ins)) => instructions_parse.push(ins),
            Ok(None) => (),
            Err(e) => errors.push((line_n, e)),
        }
    }

    if errors.is_empty() {
        // successful parse
        let mut fo: File = File::create(output_path.clone()).expect("error: unable to create output");
        let filename: &str = assembly_path.file_name().unwrap().to_str().unwrap();
        match instructions_to_hex(instructions_parse, filename ) {
            Err(error) => println!(
                "{}: {}: {}",
                Style::new()
                    .bold()
                    .paint(filename),
                Color::Red.paint("error"),
                error, 
                ),
            Ok(program_bytecode) => {
                match fo.write_all(&program_bytecode) {
                    Err(e) => println!("while writing output, encountered the following error: {}", e),
                    Ok(_) => println!("Successfully assembled program. Bytecode is located at {}", output_path.to_str().unwrap()),
                };
            }
        };
    } else {
        println!("Error assembling {}:\n", assembly_path.to_str().unwrap());
        for (line_n, error) in errors {
            println!(
                "in {}:{}: {}: {}",
                Style::new()
                    .bold()
                    .paint(assembly_path.file_name().unwrap().to_str().unwrap()),
                line_n+1,
                Color::Red.paint("error"),
                error
            );
        }
    }
}
