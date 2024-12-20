use crate::utils::*;

#[derive(Debug)]
pub struct I8008 {
    // I/O
    state_external: CpuState, // 3 bits out to signal state, abstracted away from pins s0,s1,s2
    pub line_interrupt: bool, // f1 & f2 ignored in this model since they are more electronics
    pub line_ready: bool,
    pub databus: u8, // 8 bits I/O

    // internal
    state_internal: CpuStateI, // T1-T5 are a subset of the external states
    cycle: CpuCycle,
    register_a: u8, // internal register
    register_b: u8, // internal register
    instruction_register: u8,
    read_control_bit: bool, // presumed undocumented bit used to differentiate between PCR cycled

    // scratchpad
    scratchpad_a: u8, // accumulator register
    scratchpad_b: u8,
    scratchpad_c: u8,
    scratchpad_d: u8,
    scratchpad_e: u8,
    scratchpad_l: u8, // 8 lower  bits of a memory address
    scratchpad_h: u8, // 6 higher bits of a memory address, bits 6 & 7 set 0

    // ALU flags
    flag_carry: bool,
    flag_zero: bool,
    flag_sign: bool,
    flag_parity: bool,

    // address stack
    address_stack_a: u16, // 8 registers of 14 bits
    address_stack_b: u16,
    address_stack_c: u16,
    address_stack_d: u16,
    address_stack_e: u16,
    address_stack_f: u16,
    address_stack_g: u16,
    address_stack_h: u16,
    stack_pointer: u8, // 3 bits to point to the program counter
}

#[derive(Debug)]
#[repr(u8)]
pub enum I8008Ins {
    // HLT = 0x00, // avoid enum conflict
    LrM = 0xC7, // does not apply to M
    LMr = 0xF8, // does not apply to M
    Lrr = 0xC0, // does not apply to M
    LrI = 0x06, // does not apply to M
    LMI = 0x3E,
    INr = 0x00, // does not apply to A or M
    DCr = 0x01, // does not apply to A or M
    AOM = 0x87,
    AOr = 0x80,
    AOI = 0x04,
    RLC = 0x02,
    RRC = 0x0A,
    RAL = 0x12,
    RAR = 0x1A,
    JMP = 0x44,
    JFc = 0x40,
    JTc = 0x60,
    CAL = 0x46,
    CFc = 0x42,
    CTc = 0x62,
    RET = 0x07,
    RFc = 0x03,
    RTc = 0x23,
    RST = 0x05,
}

#[derive(Debug)]
#[repr(u8)]
pub enum AluOp {
    // 4th, 5th, and 6th bits from the left
    AD = 0x00, // ADD
    AC = 0x08, // ADD with carry
    SU = 0x10, // SUBSTRACT
    SB = 0x18, // SUBSTRACT with borrow
    ND = 0x20, // logical AND
    XR = 0x28, // logical XOR
    OR = 0x30, // logical OR
    CP = 0x38, // COMPARE
}

// to help decode the instruction, apply these bitmasks
impl I8008Ins {
    fn mask(&self) -> u8 {
        match self {
            // I8008Ins::HLT => 0xFF,
            I8008Ins::LrM => 0xC7, // 11000111
            I8008Ins::LMr => 0xF8, // 11111000
            I8008Ins::Lrr => 0xC0, // 11000000
            I8008Ins::LrI => 0xC7, // 11000111
            I8008Ins::LMI => 0xFF, // 11111111
            I8008Ins::INr => 0xC7, // 11000111
            I8008Ins::DCr => 0xC7, // 11000111

            I8008Ins::AOM => 0xC7, // 11000111
            I8008Ins::AOr => 0xC0, // 11000000
            I8008Ins::AOI => 0xC7, // 11000111
            I8008Ins::RLC => 0xFF, // 11111111
            I8008Ins::RRC => 0xFF, // 11111111
            I8008Ins::RAL => 0xFF, // 11111111
            I8008Ins::RAR => 0xFF, // 11111111

            I8008Ins::JMP => 0xC7, // 11000111
            I8008Ins::JFc => 0xE7, // 11100111 
            I8008Ins::JTc => 0xE7, // 11100111 
            I8008Ins::CAL => 0xC7, // 11000111
            I8008Ins::CFc => 0xE7, // 11100111 
            I8008Ins::CTc => 0xE7, // 11100111 
            I8008Ins::RET => 0xC7, // 11000111
            I8008Ins::RFc => 0xE7, // 11100111 
            I8008Ins::RTc => 0xE7, // 11100111 
                                   //
            I8008Ins::RST => 0xC7, // 11000111
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum CpuState {
    T1,
    T1I,
    T2,
    WAIT,
    T3,
    STOPPED,
    T4,
    T5,
}

#[derive(Debug)]
pub enum CpuStateI {
    T1,
    T2,
    T3,
    T4,
    T5,
}

#[derive(Debug, PartialEq)]
pub enum CpuCycle {
    PCI, // cycle 1 - instruction read
    PCR, // cycle 2/3 - data reading
    PCW, // cycle 2/3 - data writing
    PCC, // cycle 2/3 - I/O operations
}

impl TryFrom<u8> for AluOp {
    type Error = ();

    // we check if the bitpattern is correct, if it is so, we transmute safely into the enum
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value & 0x38 != value {
            Err(())
        } else {
            Ok(unsafe {
                std::mem::transmute::<u8, AluOp>(value)
            })
        }
    }
}

impl I8008 {
    pub fn get_state(&self) -> &CpuState {
        &self.state_external
    }

    pub fn get_cycle(&self) -> &CpuCycle {
        &self.cycle
    }

    // debugging purposes only, the real thing does not show this
    pub fn get_stack_pointer(&self) -> u8 {
        self.stack_pointer
    }

    // debugging only
    pub fn get_register_a(&self) -> u8 {
        self.register_a
    }

    // debugging only
    pub fn get_register_b(&self) -> u8 {
        self.register_b
    }

    pub fn get_instruction_register(&self) -> u8 {
        self.instruction_register
    }

    pub fn select_flag(&self, c4: bool, c5: bool) -> bool {
        match (c4, c5) {
            (false, false) => self.flag_carry,
            (false, true) => self.flag_zero,
            (true, false) => self.flag_sign,
            (true, true) => self.flag_parity,
        }
    }

    fn get_program_counter(&self) -> u16 {
        match self.stack_pointer {
            0 => self.address_stack_a,
            1 => self.address_stack_b,
            2 => self.address_stack_c,
            3 => self.address_stack_d,
            4 => self.address_stack_e,
            5 => self.address_stack_f,
            6 => self.address_stack_g,
            7 => self.address_stack_h,
            _ => panic!(
                "intel 8008 stack pointer has gone out of range, value: {}",
                self.stack_pointer
            ),
        }
    }

    pub fn get_scratchpad_register(&mut self, d1: bool, d2: bool, d3: bool) -> u8 {
        match (d1, d2, d3) {
            (false, false, false) => self.scratchpad_a,
            (false, false, true) => self.scratchpad_b,
            (false, true, false) => self.scratchpad_c,
            (false, true, true) => self.scratchpad_d,
            (true, false, false) => self.scratchpad_e,
            (true, false, true) => self.scratchpad_l,
            (true, true, false) => self.scratchpad_h,
            (true, true, true) => panic!("intel 8008 tried to get the memory register directly!"),
        }
    }

    pub fn get_stack_register(&mut self, selector: u8) -> u16 {
        match selector {
            0 => self.address_stack_a,
            1 => self.address_stack_b,
            2 => self.address_stack_c,
            3 => self.address_stack_d,
            4 => self.address_stack_e,
            5 => self.address_stack_f,
            6 => self.address_stack_g,
            7 => self.address_stack_h,
            _ => panic!("error: tried to get the value of a stack register that doesn't exist"),
        }
    }

    fn set_scratchpad_register(&mut self, value: u8, d1: bool, d2: bool, d3: bool) {
        match (d1, d2, d3) {
            (false, false, false) => self.scratchpad_a = value,
            (false, false, true) => self.scratchpad_b = value,
            (false, true, false) => self.scratchpad_c = value,
            (false, true, true) => self.scratchpad_d = value,
            (true, false, false) => self.scratchpad_e = value,
            (true, false, true) => self.scratchpad_l = value,
            (true, true, false) => self.scratchpad_h = value,
            (true, true, true) => panic!("intel 8008 tried to set the memory register directly!"),
        }
    }

    fn set_program_counter(&mut self, value: u16) {
        match self.stack_pointer {
            0 => self.address_stack_a = value,
            1 => self.address_stack_b = value,
            2 => self.address_stack_c = value,
            3 => self.address_stack_d = value,
            4 => self.address_stack_e = value,
            5 => self.address_stack_f = value,
            6 => self.address_stack_g = value,
            7 => self.address_stack_h = value,
            _ => panic!(
                "intel 8008 stack pointer has gone out of range, value: {}",
                self.stack_pointer
            ),
        };
    }

    pub fn new() -> Self {
        Self {
            state_external: CpuState::STOPPED,
            line_interrupt: false,
            line_ready: true, // always ready
            databus: 0,
            state_internal: CpuStateI::T3,
            cycle: CpuCycle::PCI,
            register_a: 0,
            register_b: 0,
            instruction_register: 0,
            read_control_bit: false,
            scratchpad_a: 0,
            scratchpad_b: 0,
            scratchpad_c: 0,
            scratchpad_d: 0,
            scratchpad_e: 0,
            scratchpad_l: 0,
            scratchpad_h: 0,
            flag_carry: false,
            flag_zero: false,
            flag_sign: false,
            flag_parity: false,
            address_stack_a: 0,
            address_stack_b: 0,
            address_stack_c: 0,
            address_stack_d: 0,
            address_stack_e: 0,
            address_stack_f: 0,
            address_stack_g: 0,
            address_stack_h: 0,
            stack_pointer: 0, // initally points to the first element of the stack
        }
    }
    fn clear_flags(&mut self) {
        self.flag_carry = false;
        self.flag_zero = false;
        self.flag_sign = false;
        self.flag_parity = false;
    }

    fn alu(&mut self, operation: AluOp, value: u8) {
        println!("[DEBUG] ALU OP '{:?}' on value {:#04X}", operation, value);
        let carry_bit: u8 = match self.flag_carry {
            true => 1,
            false => 0,
        };
        match operation {
            AluOp::AD => {
                self.flag_carry = self.scratchpad_a.overflowing_add(value).1;
                self.scratchpad_a = self.scratchpad_a.wrapping_add(value);
            }
            AluOp::AC => {
                if value == 0xFF {
                    self.flag_carry = true; // 0xFF + 1 overflows once, but overflowing_add ignores it
                    // skip adding 0x100
                } else {
                    self.flag_carry = self.scratchpad_a.overflowing_add(value+1).1;
                    self.scratchpad_a = self.scratchpad_a.wrapping_add(value + carry_bit);
                };
            }
            AluOp::SU => {
                self.flag_carry = self.scratchpad_a.overflowing_sub(value).1;
                self.scratchpad_a = self.scratchpad_a.wrapping_sub(value);
            }
            AluOp::SB => {
                if value == 0xFF {
                    self.flag_carry = true;
                    // skip adding 0x100
                } else {
                    self.flag_carry = self.scratchpad_a.overflowing_sub(value - carry_bit).1;
                    self.scratchpad_a = self.scratchpad_a.wrapping_sub(value - carry_bit);
                }
            }
            AluOp::ND => self.scratchpad_a &= value,
            AluOp::XR => self.scratchpad_a ^= value,
            AluOp::OR => self.scratchpad_a |= value,
            AluOp::CP => {
                let delta: i16 = self.scratchpad_a as i16 - value as i16;
                println!("[DEBUG] produced delta {}", delta);
                if delta == 0 {
                    self.flag_zero = true;
                } else {
                    self.flag_zero = false;
                }

                if delta < 0 { // A < value
                    self.flag_carry = true;
                } else {
                    self.flag_carry = false;
                }
            },
        }
    }

    fn push_stack(&mut self) {
        if self.stack_pointer < 7 {
            self.stack_pointer += 1;
        } else {
            self.stack_pointer = 0;
        }
    }

    fn pop_stack(&mut self) {
        if self.stack_pointer == 0 {
            self.stack_pointer = 7;
        } else {
            self.stack_pointer -= 1;
        }
    }

    fn check_condition(&self, conditions: u8) -> bool {
        // presume that the incoming bits are already cleared
        match conditions {
            0x00 => self.flag_carry,
            0x08 => self.flag_zero,
            0x10 => self.flag_sign,
            0x18 => self.flag_parity,
            _ => panic!("error: unsanitized input to self.check_condition"),
        }
    }

    fn set_flags(&mut self, result: u8) {
        // doesn't set CARRY since we can't know it just from the value

        // ZERO: result is zero
        if result == 0x00 {
            self.flag_zero = true;
        } else {
            self.flag_zero = false;
        }

        // SIGN: result is a signed integer, >= 0x80. Alternatively expressed as MSB(r) = 1
        if result & 0x80 == 0x80 {
            self.flag_sign = true;
        } else {
            self.flag_sign = false;
        }

        // PARITY: result is even. Alternatively expressed as LSB(r) = 1
        if result & 0x01 == 0x01 {
            // I am aware that you can do `result % 2`
            self.flag_parity = true;
        } else {
            self.flag_parity = false;
        }
    }

    // takes two clock cycles and processes a state
    pub fn step(&mut self) {
        match self.cycle {
            CpuCycle::PCI => match self.state_internal {
                CpuStateI::T1 => {
                    let pc: u16 = self.get_program_counter();
    
                    self.databus = ((self.get_program_counter() & 0x00FF) as u8).reverse_bits();
                    // send out 8 lower bits
    
                    if !self.line_interrupt {
                        if (pc & 0x00FF) == 0xFF {
                            self.flag_carry = true;
                        }
                        self.set_program_counter(pc + 1);
                    } else {
                        self.line_interrupt = false;
                    }
                    self.state_internal = CpuStateI::T2;
                    self.state_external = CpuState::T2;
                }
                CpuStateI::T2 => {
                    self.databus = (((self.get_program_counter() & 0x3F00) >> 8) as u8).reverse_bits(); // send out 6 higher bits
                    self.state_internal = CpuStateI::T3;
                    self.state_external = CpuState::T3;
                }
                CpuStateI::T3 => 'block: {
                    if compare_instruction_mask(self.databus, I8008Ins::RST as u8, I8008Ins::RST.mask() ) {
                        self.register_b = self.databus;
                        self.register_a = 0x00;
                        self.push_stack();
                        self.state_external = CpuState::T4;
                        self.state_internal = CpuStateI::T4;
                        break 'block
                    };

                    self.instruction_register = self.databus;
                    self.register_b = self.instruction_register;

                    let mut t1: bool = false;
                    let mut t4: bool = false;
                    let mut t5: bool = false;

                    if !self.line_ready {
                        self.state_external = CpuState::WAIT;
                        break 'block 
                    } else {
                        self.state_external = CpuState::T3;
                    }
    
                    // order matters!
                    // HLT = 11 111 111
                    //     = 00 000 00X - hence the mask with 0xFE
                    if self.instruction_register == 0xFF || self.instruction_register & 0xFE == 0x00 {
                        // filter out overlapping cases of HLT instructions
                        if self.line_interrupt {
                            self.state_internal = CpuStateI::T1;
                            self.state_external = CpuState::T1I;
                        } else {
                            self.state_external = CpuState::STOPPED;
                        }
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::LrM as u8, I8008Ins::LrM.mask() ) {
                        self.cycle = CpuCycle::PCR;
                        t1 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::LMr as u8, I8008Ins::LMr.mask() ) {
                        t4 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::Lrr as u8, I8008Ins::Lrr.mask() ) {
                        t4 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::LMI as u8, I8008Ins::LMI.mask() ) {
                        self.cycle = CpuCycle::PCR;
                        t1 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::LrI as u8, I8008Ins::LrI.mask() ) {
                        self.cycle = CpuCycle::PCR;
                        t1 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::INr as u8, I8008Ins::INr.mask() ) {
                        t5 = true; // idle in t4
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::DCr as u8, I8008Ins::DCr.mask() ) {
                        t5 = true; // idle in t4
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::AOM as u8, I8008Ins::AOM.mask()) {
                        self.cycle = CpuCycle::PCR;
                        t1 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::AOr as u8, I8008Ins::AOr.mask()) {
                        self.register_b = self.instruction_register;
                        t4 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::AOI as u8, I8008Ins::AOI.mask()) {
                        self.cycle = CpuCycle::PCR;
                        t1 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::RLC as u8, I8008Ins::RLC.mask()) {
                        t5 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::RRC as u8, I8008Ins::RRC.mask()) {
                        t5 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::RAL as u8, I8008Ins::RAL.mask()) {
                        t5 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::RAR as u8, I8008Ins::RAR.mask()) {
                        t5 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::JMP as u8, I8008Ins::JMP.mask()) 
                        || compare_instruction_mask(self.instruction_register, I8008Ins::JFc as u8, I8008Ins::JFc.mask())
                        || compare_instruction_mask(self.instruction_register, I8008Ins::JTc as u8, I8008Ins::JTc.mask())
                    {
                        self.cycle = CpuCycle::PCR;
                        self.read_control_bit = false;
                        t1 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::CAL as u8, I8008Ins::CAL.mask()) 
                        || compare_instruction_mask(self.instruction_register, I8008Ins::CFc as u8, I8008Ins::CFc.mask()) 
                        || compare_instruction_mask(self.instruction_register, I8008Ins::CTc as u8, I8008Ins::CTc.mask())
                    {
                        self.cycle = CpuCycle::PCR;
                        self.read_control_bit = false;
                        t1 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::RET as u8, I8008Ins::RET.mask()) {
                        t4 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::RFc as u8, I8008Ins::RFc.mask()) {
                        t4 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::RTc as u8, I8008Ins::RTc.mask()) {
                        t4 = true;
                    } else {
                        panic!("error: reached unimplemented instruction {:#04X} in cycle PCI state T3", self.instruction_register)
                    }
    
                    if t5 {
                        self.state_internal = CpuStateI::T5;
                        self.state_external = CpuState::T5;
                    } else if t4 {
                        self.state_internal = CpuStateI::T4;
                        self.state_external = CpuState::T4;
                    } else if t1 {
                        self.state_internal = CpuStateI::T1;
                        self.state_external = CpuState::T1;
                    }
                }
                CpuStateI::T4 => {
                    let mut t5: bool = false;
                    let mut t1: bool = false;
                    if compare_instruction_mask(self.instruction_register, I8008Ins::LMr as u8, I8008Ins::LMr.mask() ) {
                        let register_selector = self.instruction_register & (!I8008Ins::LMr.mask());
                        self.register_b = self.get_scratchpad_register(
                            (register_selector & 0x04) >> 2 != 0,
                            (register_selector & 0x02) >> 1 != 0,
                            (register_selector & 0x01) >> 0 != 0,
                        );
                        self.cycle = CpuCycle::PCW;
                        t1 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::Lrr as u8, I8008Ins::Lrr.mask() ) {
                        let register_selector: u8 = self.instruction_register & 0x07;
                        self.register_b = self.get_scratchpad_register(
                            (register_selector & 0x04) >> 2 != 0,
                            (register_selector & 0x02) >> 1 != 0,
                            (register_selector & 0x01) >> 0 != 0,
                        );
    
                        t5 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::AOr as u8, I8008Ins::AOr.mask()) {
                        let register_selector: u8 = self.instruction_register & 0x07;
                        self.register_b = self.get_scratchpad_register(
                            (register_selector & 0x04) >> 2 != 0,
                            (register_selector & 0x02) >> 1 != 0,
                            (register_selector & 0x01) >> 0 != 0,
                        );
                        t5 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::RET as u8, I8008Ins::RET.mask()) {
                        self.pop_stack();
                        t1 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::RFc as u8, I8008Ins::RFc.mask()) {
                        if ! (self.check_condition(self.instruction_register & 0x18)) { // select flag bits
                            self.pop_stack();
                        } else {
                        }
                        t1 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::RTc as u8, I8008Ins::RTc.mask()) {
                        if self.check_condition(self.instruction_register & 0x18) { // select flag bits
                            self.pop_stack();
                        } else {
                        }
                        t1 = true;
                    } else if compare_instruction_mask(self.register_b, I8008Ins::RST as u8, I8008Ins::RST.mask()) {
                        self.set_program_counter((self.register_a as u16) << 8);
                        t5 = true;
                    }
    
                    if t5 {
                        self.state_internal = CpuStateI::T5;
                        self.state_external = CpuState::T5;
                    } else if t1 {
                        self.state_internal = CpuStateI::T1;
                        self.state_external = CpuState::T1;
                    }
                }
                CpuStateI::T5 => {
                    if compare_instruction_mask(self.instruction_register, I8008Ins::Lrr as u8, I8008Ins::Lrr.mask() ) {
                        let register_selector = self.instruction_register & 0x38; 
                        self.set_scratchpad_register(self.register_b,
                            (register_selector & 0x20) >> 5 != 0,
                            (register_selector & 0x10) >> 4 != 0, 
                            (register_selector & 0x08) >> 3 != 0
                        );
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::INr as u8, I8008Ins::INr.mask() ) {
                        let register_selector = self.instruction_register & (!I8008Ins::INr.mask());

    
                        // we borrow register a for this
                        self.register_a = self.get_scratchpad_register(
                            (register_selector & 0x20) >> 5 != 0,
                            (register_selector & 0x10) >> 4 != 0,
                            (register_selector & 0x08) >> 3 != 0,
                        );
    
                        if self.register_a == 0xFF {
                            self.register_a = 0x00 // explicit overflow
                        } else {
                            self.register_a += 1;
                        }
    
                        self.clear_flags();
                        if self.register_a == 0x00 {
                            // 0xFF++ = 0x00 + carry
                            self.flag_carry = true;
                        } else {
                            self.flag_carry = false;
                        }
                        self.set_flags(self.register_a);
                        
                        self.set_scratchpad_register(
                            self.register_a,
                            (register_selector & 0x20) >> 5 != 0,
                            (register_selector & 0x10) >> 4 != 0,
                            (register_selector & 0x08) >> 3 != 0,
                        );
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::DCr as u8, I8008Ins::DCr.mask() ) {
                        let register_selector = self.instruction_register & (!I8008Ins::DCr.mask());

    
                        // we borrow register a for this
                        self.register_a = self.get_scratchpad_register(
                            (register_selector & 0x20) >> 5 != 0,
                            (register_selector & 0x10) >> 4 != 0,
                            (register_selector & 0x08) >> 3 != 0,
                        );
    
                        if self.register_a == 0x00 {
                            self.register_a = 0xFF // explicit underflow
                        } else {
                            self.register_a -= 1;
                        }
    
                        self.clear_flags();
                        if self.register_a == 0xFF {
                            self.flag_carry = true; // 0x00-- = 0xFF with carry
                        } else {
                            self.flag_carry = false;
                        }
    
                        self.set_flags(self.register_a);
                        
    
                        self.set_scratchpad_register(
                            self.register_a,
                            (register_selector & 0x20) >> 5 != 0,
                            (register_selector & 0x10) >> 4 != 0,
                            (register_selector & 0x08) >> 3 != 0,
                        );
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::AOr as u8, I8008Ins::AOr.mask()) {
                        let operation_selector: u8 = self.instruction_register & 0x38;
                        self.clear_flags();

                        self.alu(AluOp::try_from(operation_selector).unwrap(), self.register_b); // sets CARRY flag
                        if operation_selector != 0x38 { // 0x38 == 00111000 == compare sets ZERO flag
                            self.set_flags(self.register_a); // set rest of the flags
                        }
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::RLC as u8, I8008Ins::RLC.mask()) {
                        if self.scratchpad_a & 0x80 == 0x80 {
                            self.flag_carry = true;
                        } else {
                            self.flag_carry = false;
                        }
                        self.scratchpad_a = self.scratchpad_a.rotate_left(1);
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::RRC as u8, I8008Ins::RRC.mask()) {
                        if self.scratchpad_a & 0x01 == 0x01 {
                            self.flag_carry = true;
                        } else {
                            self.flag_carry = false;
                        }
                        self.scratchpad_a = self.scratchpad_a.rotate_right(1);
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::RAL as u8, I8008Ins::RAL.mask()) {
                        let carry_t: bool = if self.scratchpad_a & 0x80 == 0x80 {
                            true
                        } else {
                            false
                        };
                        self.scratchpad_a <<= 1;
                        self.scratchpad_a |= self.flag_carry as u8;
                        self.flag_carry = carry_t;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::RAR as u8, I8008Ins::RAR.mask()) {
                        let carry_t: bool = if self.scratchpad_a & 0x01 == 0x01 {
                            true
                        } else {
                            false
                        };
                        self.scratchpad_a >>= 1;
                        self.scratchpad_a |= (self.flag_carry as u8) << 7;
                        self.flag_carry = carry_t;
                    } else if compare_instruction_mask(self.register_b, I8008Ins::RST as u8, I8008Ins::RST.mask()) {
                        self.set_program_counter((self.register_b & !I8008Ins::RST.mask()) as u16);
                    }

                    self.state_internal = CpuStateI::T1;
                    self.state_external = CpuState::T1;
                }
            },
            CpuCycle::PCR => match self.state_internal {
                CpuStateI::T1 => {
                    if compare_instruction_mask(self.instruction_register, I8008Ins::LrM as u8, I8008Ins::LrM.mask() ) {
                        self.databus = self.scratchpad_l.reverse_bits();
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::AOM as u8, I8008Ins::AOM.mask() ) {
                        self.databus = self.scratchpad_l.reverse_bits();
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::LMI as u8, I8008Ins::LMI.mask() ) || 
                              compare_instruction_mask(self.instruction_register, I8008Ins::LrI as u8, I8008Ins::LrI.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::AOI as u8, I8008Ins::AOI.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::JMP as u8, I8008Ins::JMP.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::JFc as u8, I8008Ins::JFc.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::JTc as u8, I8008Ins::JTc.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::CAL as u8, I8008Ins::CAL.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::CFc as u8, I8008Ins::CFc.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::CTc as u8, I8008Ins::CTc.mask() )
                    {
                        let pc: u16 = self.get_program_counter();
    
                        self.databus = ((self.get_program_counter() & 0x00FF) as u8).reverse_bits();
                        // send out 8 lower bits
        
                        if (pc & 0x00FF) == 0xFF {
                            self.flag_carry = true;
                        }
                        self.set_program_counter(pc + 1);
                    }
                    self.state_internal = CpuStateI::T2;
                    self.state_external = CpuState::T2;
                }
                CpuStateI::T2 => {
                    if compare_instruction_mask(self.instruction_register, I8008Ins::LrM as u8, I8008Ins::LrM.mask() ) {
                        self.databus = self.scratchpad_h.reverse_bits();
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::AOM as u8, I8008Ins::AOM.mask() ) {
                        self.databus = self.scratchpad_h.reverse_bits();
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::LMI as u8, I8008Ins::LMI.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::LrI as u8, I8008Ins::LrI.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::AOI as u8, I8008Ins::AOI.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::JMP as u8, I8008Ins::JMP.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::JFc as u8, I8008Ins::JFc.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::JTc as u8, I8008Ins::JTc.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::CAL as u8, I8008Ins::CAL.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::CFc as u8, I8008Ins::CFc.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::CTc as u8, I8008Ins::CTc.mask() )
                    {
                        self.databus = (((self.get_program_counter() & 0x3F00) >> 8) as u8).reverse_bits();
                    }
                    self.state_internal = CpuStateI::T3;
                    self.state_external = CpuState::T3;
                }
                CpuStateI::T3 => {
                    let mut t1: bool = false;
                    let mut t5: bool = false;
                    let mut t4: bool = false;
                    if compare_instruction_mask(self.instruction_register, I8008Ins::LrM as u8, I8008Ins::LrM.mask() ) {
                        self.register_b = self.databus;
                        t5 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::AOM as u8, I8008Ins::AOM.mask() ) {
                        self.register_b = self.databus;
                        t5 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::LMI as u8, I8008Ins::LMI.mask() ) { 
                        self.register_b = self.databus;
                        t1 = true;
                        self.cycle = CpuCycle::PCW;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::LrI as u8, I8008Ins::LrI.mask() ) { 
                        self.register_b = self.databus;
                        t5 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::AOI as u8, I8008Ins::AOI.mask() ) { 
                        self.register_b = self.databus;
                        t5 = true;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::JMP as u8, I8008Ins::JMP.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::CAL as u8, I8008Ins::CAL.mask() )
                    {
                        match self.read_control_bit {
                            false => {
                                self.register_b = self.databus;
                                t1 = true;
                                self.read_control_bit = true;
                            },
                            true => {
                                self.register_a = self.databus;
                                t4 = true;
                            },
                        }
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::JFc as u8, I8008Ins::JFc.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::JTc as u8, I8008Ins::JTc.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::CFc as u8, I8008Ins::CFc.mask() ) ||
                              compare_instruction_mask(self.instruction_register, I8008Ins::CTc as u8, I8008Ins::CTc.mask() )
                    {
                        match self.read_control_bit {
                            false => {
                                self.register_b = self.databus;
                                t1 = true;
                                self.read_control_bit = true;
                            },
                            true => {
                                // finished reading in address
                                self.read_control_bit = false;
                                let condition_met: bool = self.check_condition(self.instruction_register & 0x18); // select bit pattern 00011000

                                // based on condition's status, check if the corresponding
                                // instruction matches or not
                                if condition_met {
                                    if compare_instruction_mask(
                                        self.instruction_register,
                                        I8008Ins::JTc as u8,
                                        I8008Ins::JTc.mask(),
                                    ) {
                                        self.register_a = self.databus;
                                        t4 = true;
                                    } else if compare_instruction_mask(
                                        self.instruction_register,
                                        I8008Ins::CTc as u8,
                                        I8008Ins::CTc.mask(),
                                    ) {
                                        self.register_a = self.databus;
                                        self.push_stack();
                                        t4 = true;
                                    } else {
                                        self.cycle = CpuCycle::PCI;
                                        t1 = true;
                                    }
                                } else {
                                    if compare_instruction_mask(
                                        self.instruction_register,
                                        I8008Ins::JFc as u8,
                                        I8008Ins::JFc.mask(),
                                    ) {
                                        self.register_a = self.databus;
                                        t4 = true;
                                    } else if compare_instruction_mask(
                                        self.instruction_register,
                                        I8008Ins::CFc as u8,
                                        I8008Ins::CFc.mask(),
                                    ) {
                                        self.register_a = self.databus;
                                        self.push_stack();
                                        t4 = true;
                                    } else {
                                        self.cycle = CpuCycle::PCI;
                                        t1 = true;
                                    }
                                }
                            },
                        }
                    } else {
                        panic!("error: reached unimplemented instruction {:#04X} in cycle PCR state T3", self.instruction_register)
                    }
                    if t5 {
                        self.state_internal = CpuStateI::T5;
                        self.state_external = CpuState::T5;
                    } else if t4 {
                        self.state_internal = CpuStateI::T4;
                        self.state_external = CpuState::T4;
                    } else if t1 {
                        self.state_internal = CpuStateI::T1;
                        self.state_external = CpuState::T1;
                    }
                }
                CpuStateI::T4 => {
                    if compare_instruction_mask(self.instruction_register, I8008Ins::JMP as u8, I8008Ins::JMP.mask() ) ||
                       compare_instruction_mask(self.instruction_register, I8008Ins::JFc as u8, I8008Ins::JFc.mask() ) ||
                       compare_instruction_mask(self.instruction_register, I8008Ins::JTc as u8, I8008Ins::JTc.mask() ) ||
                       compare_instruction_mask(self.instruction_register, I8008Ins::CAL as u8, I8008Ins::CAL.mask() ) ||
                       compare_instruction_mask(self.instruction_register, I8008Ins::CFc as u8, I8008Ins::CFc.mask() ) ||
                       compare_instruction_mask(self.instruction_register, I8008Ins::CTc as u8, I8008Ins::CTc.mask() )
                    {
                        self.set_program_counter((self.register_a as u16) << 8);
                    }
                    self.state_internal = CpuStateI::T5;
                    self.state_external = CpuState::T5;
                }
                CpuStateI::T5 => {
                    if compare_instruction_mask(self.instruction_register, I8008Ins::LrM as u8, I8008Ins::LrM.mask() ) {
                        let register_selector = self.instruction_register & 0x38; 
                        self.set_scratchpad_register(self.register_b,
                            (register_selector & 0x20) >> 5 != 0,
                            (register_selector & 0x10) >> 4 != 0, 
                            (register_selector & 0x08) >> 3 != 0
                        );
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::AOM as u8, I8008Ins::AOM.mask() ) {
                        let operation_selector: u8 = self.instruction_register & 0x38;
                        self.clear_flags();

                        self.alu(AluOp::try_from(operation_selector).unwrap(), self.register_b); // sets CARRY flag
                        if operation_selector != 0x38 { // 0x38 == 00111000 == compare sets ZERO flag
                            self.set_flags(self.register_a); // set rest of the flags
                        }
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::LrI as u8, I8008Ins::LrI.mask() ) { 
                        // shouldn't get here with LMI
                        let register_selector = self.instruction_register & 0x38; 
                        self.set_scratchpad_register(self.register_b,
                            (register_selector & 0x20) >> 5 != 0,
                            (register_selector & 0x10) >> 4 != 0, 
                            (register_selector & 0x08) >> 3 != 0
                        );
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::AOI as u8, I8008Ins::AOI.mask() ) {
                        let operation_selector: u8 = self.instruction_register & 0x38;
                        self.clear_flags();
                        self.alu(AluOp::try_from(operation_selector).unwrap(), self.register_b); // sets CARRY flag
                        if operation_selector != 0x38 { // 0x38 == 00111000 == compare sets ZERO flag
                            self.set_flags(self.register_a); // set rest of the flags
                        }
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::JMP as u8, I8008Ins::JMP.mask() ) ||
                       compare_instruction_mask(self.instruction_register, I8008Ins::JFc as u8, I8008Ins::JFc.mask() ) ||
                       compare_instruction_mask(self.instruction_register, I8008Ins::JTc as u8, I8008Ins::JTc.mask() ) ||
                       compare_instruction_mask(self.instruction_register, I8008Ins::CAL as u8, I8008Ins::CAL.mask() ) ||
                       compare_instruction_mask(self.instruction_register, I8008Ins::CFc as u8, I8008Ins::CFc.mask() ) ||
                       compare_instruction_mask(self.instruction_register, I8008Ins::CTc as u8, I8008Ins::CTc.mask() )
                    {
                        let pc = self.get_program_counter() & 0x3F; // extract higher bits
                        self.set_program_counter(self.register_b as u16 | pc);
                    }

                    self.cycle = CpuCycle::PCI;
                    self.state_internal = CpuStateI::T1;
                    self.state_external = CpuState::T1;
                }
            },
            CpuCycle::PCW => match self.state_internal {
                CpuStateI::T1 => {
                    if compare_instruction_mask(self.instruction_register, I8008Ins::LMr as u8, I8008Ins::LMr.mask() ) {
                        self.databus = self.scratchpad_l.reverse_bits();
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::LMI as u8, I8008Ins::LMI.mask() ) {
                        self.databus = self.scratchpad_l.reverse_bits();
                    }
                    self.state_internal = CpuStateI::T2;
                    self.state_external = CpuState::T2;
                },
                CpuStateI::T2 => {
                    if compare_instruction_mask(self.instruction_register, I8008Ins::LMr as u8, I8008Ins::LMr.mask() ) {
                        self.databus = self.scratchpad_h.reverse_bits();
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::LMI as u8, I8008Ins::LMI.mask() ) {
                        self.databus = self.scratchpad_h.reverse_bits();
                    }
                    self.state_internal = CpuStateI::T3;
                    self.state_external = CpuState::T3;
                },
                CpuStateI::T3 => {
                    let mut t1: bool = false;
                    if compare_instruction_mask(self.instruction_register, I8008Ins::LMr as u8, I8008Ins::LMr.mask() ) {
                        self.databus = self.register_b;

                        t1 = true;
                        self.cycle = CpuCycle::PCI;
                    } else if compare_instruction_mask(self.instruction_register, I8008Ins::LMI as u8, I8008Ins::LMI.mask() ) {
                        self.databus = self.register_b;

                        t1 = true;
                        self.cycle = CpuCycle::PCI;
                    }

                    if t1 {
                        self.state_internal = CpuStateI::T1;
                        self.state_external = CpuState::T1;
                    }
                },
                CpuStateI::T4 => {},
                CpuStateI::T5 => {},
            },
            _ => (),
        };
    }
}

pub fn step_with_mem(
    chip: &mut I8008,
    memory: &mut MemoryController,
    address_reg: &mut u16,
    state: CpuState,
) {
    match state {
        CpuState::T1 => {
            chip.step();
            *address_reg = (*address_reg & 0xFF00) + (chip.databus as u16);
        }
        CpuState::T2 => {
            chip.step();
            *address_reg = (*address_reg & 0x00FF) + ((chip.databus as u16) << 8);
        }
        CpuState::T3 => match chip.get_cycle() {
            CpuCycle::PCI | CpuCycle::PCR => {
                println!(
                    "[DEBUG] read in address {:#06X}, providing {:#04X}",
                    u14_to_u16(*address_reg),
                    memory.get_value(*address_reg)
                );
                chip.databus = memory.get_value(*address_reg);
                chip.step();
            },
            CpuCycle::PCW => {
                println!("[DEBUG] read in address {:#06X}, setting new value", u14_to_u16(*address_reg));
                chip.step();
                memory.memory[u14_to_u16(*address_reg) as usize] = chip.databus;
            },
            _ => ()
        }
        _ => chip.step(),
    }
}


#[inline(always)]
pub fn u14_to_u16(address: u16) -> u16 {
    ((address & 0x00FF) as u8).reverse_bits() as u16 + // first byte
    (((((address & 0x3F00) >> 8) as u8).reverse_bits() as u16) << 8) // second byte
}

#[inline(always)]
fn compare_instruction_mask(data: u8, instruction: u8, mask: u8) -> bool {
    (data & mask) == instruction
}
