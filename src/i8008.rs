pub struct I8008MemoryController {
    pub memory: [u8; 16384],     // 16K memory
}

#[derive(Debug)]
pub struct I8008 {
    // I/O
    state_external: CpuState,       // 3 bits out to signal state, abstracted away from pins s0,s1,s2
    pub line_interrupt: bool,           // f1 & f2 ignored in this model since they are more electronics
    pub line_ready: bool,
    pub databus: u8,                    // 8 bits I/O

    // internal
    state_internal: CpuStateI,      // T1-T5 are a subset of the external states
    cycle: CpuCycle,
    register_a: u8,                 // internal register
    register_b: u8,                 // internal register
    instruction_register: u8,

    // scratchpad
    scratchpad_a: u8,               // accumulator register
    scratchpad_b: u8,
    scratchpad_c: u8,
    scratchpad_d: u8,
    scratchpad_e: u8,
    scratchpad_l: u8,               // 8 lower  bits of a memory address
    scratchpad_h: u8,               // 6 higher bits of a memory address, bits 6 & 7 set 0

    // ALU flags
    flag_carry: bool,
    flag_zero: bool,
    flag_sign: bool,
    flag_parity: bool,

    // address stack
    address_stack_a: u16,          // 8 registers of 14 bits
    address_stack_b: u16,
    address_stack_c: u16,
    address_stack_d: u16,
    address_stack_e: u16,
    address_stack_f: u16,
    address_stack_g: u16, 
    address_stack_h: u16,
    stack_pointer: u8,             // 3 bits to point to the program counter

}

#[derive(Debug)]
pub enum I8008Ins {
    // HLT = 0x00, // avoid enum conflict
    LrM = 0xC7,
    LMr = 0xCF,
    Lrr = 0xC0, // does not apply to M
    LrI = 0x06,
    LMI = 0x3E,
    INr = 0x00, // doesn't apply to A or M
    DCr = 0x01, 
}

#[derive(Debug)]
enum AluOp {
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
        I8008Ins::LrM => 0xC7,
        I8008Ins::LMr => 0xF8,
        I8008Ins::Lrr => 0xC0,
        I8008Ins::LrI => 0xC7,
        I8008Ins::LMI => 0xFF,
        I8008Ins::INr => 0xC7,
        I8008Ins::DCr => 0xC7,
    }
    }
}

#[derive(Debug)]
pub enum CpuState {
    T1, T1I, T2, WAIT, T3, STOPPED, T4, T5
}

#[derive(Debug)]
pub enum CpuStateI {
    T1, T2, T3, T4, T5
}

#[derive(Debug, PartialEq)]
pub enum CpuCycle {
    PCI, // cycle 1 - instruction read
    PCR, // cycle 2/3 - data reading
    PCW, // cycle 2/3 - data writing
    PCC  // cycle 2/3 - I/O operations
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
            _ => panic!("intel 8008 stack pointer has gone out of range, value: {}", self.stack_pointer)
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
            (true, true, true) => panic!("intel 8008 tried to get the memory register directly!") 
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
            _ => panic!("error: tried to get the value of a stack register that doesn't exist")
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
            (true, true, true) => panic!("intel 8008 tried to set the memory register directly!") 
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
            _ => panic!("intel 8008 stack pointer has gone out of range, value: {}" ,self.stack_pointer)
        };
    }

    pub fn new() -> Self {
        Self {
            state_external: CpuState::STOPPED,
            line_interrupt: false,
            line_ready: false,
            databus: 0,
            state_internal: CpuStateI::T3,
            cycle: CpuCycle::PCI,
            register_a: 0,
            register_b: 0,
            instruction_register: 0,
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

    fn alu(&mut self, operation: AluOp ) {
        // TODO
        match operation {
            AluOp::AD => {
                ()
            }
            _ => ()
        };
    }
   
    
    // takes two clock cycles and processes a state 
    pub fn step(&mut self) {
        match self.state_internal {
            CpuStateI::T1 => {
                let pc: u16 = self.get_program_counter();

                if self.cycle == CpuCycle::PCR && (
                    compare_instruction_mask(self.instruction_register, I8008Ins::LrM as u8, I8008Ins::LrM.mask() ) ||
                    compare_instruction_mask(self.instruction_register, I8008Ins::LMr as u8, I8008Ins::LMr.mask() )
                    ) {
                    self.databus = reverse_u8(self.scratchpad_l);
                } else {
                    self.databus = reverse_u8((self.get_program_counter() & 0x00FF ) as u8); // send out 8 lower bits
                }

                if !self.line_interrupt {
                    if (pc & 0x00FF) == 0xFF {
                        self.flag_carry = true;
                    }
                    println!("[DEBUG]: incremented pc by 1");
                    self.set_program_counter(pc + 1);
                } else {
                    self.line_interrupt = false;
                }
                self.state_internal = CpuStateI::T2;
                self.state_external = CpuState::T2;
            },
            CpuStateI::T2 => {
                if self.cycle == CpuCycle::PCR && (
                    compare_instruction_mask(self.instruction_register, I8008Ins::LrM as u8, I8008Ins::LrM.mask() ) ||
                    compare_instruction_mask(self.instruction_register, I8008Ins::LMr as u8, I8008Ins::LMr.mask() )
                    ) {
                    self.databus = reverse_u8(self.scratchpad_h);
                } else {
                    self.databus = reverse_u8((self.get_program_counter() & 0x3F00 ) as u8); // send out 6 higher bits
                }
                self.state_internal = CpuStateI::T3;
                self.state_external = CpuState::T3;
            },
            CpuStateI::T3 => {
                self.instruction_register = self.databus;
                let mut t1: bool = false;
                let mut t4: bool = false;
                let mut t5: bool = false;

                // order matters!
                if self.instruction_register == 0xFF || self.instruction_register == 0x00 {
                    if self.line_interrupt {
                        self.state_internal = CpuStateI::T1;
                        self.state_external = CpuState::T1I;
                    } else {
                        self.state_external = CpuState::STOPPED;
                    }
                    // filter out overlapping cases of HLT instructions
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
                    self.register_b = self.instruction_register; // & (!I8008Ins::INr.mask());
                    t5 = true; // idle in t4
                } else if compare_instruction_mask(self.instruction_register, I8008Ins::DCr as u8, I8008Ins::DCr.mask() ) {
                    self.register_b = self.instruction_register & (!I8008Ins::DCr.mask());
                    t5 = true; // idle in t4
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
            },
            CpuStateI::T4 => {
                let mut t5: bool = false;
                let mut t1: bool = false;
                if compare_instruction_mask(self.instruction_register, I8008Ins::LMr as u8, I8008Ins::LMr.mask() ) {
                    self.register_b = self.instruction_register & (!I8008Ins::LMr.mask());
                    t1 = true;
                    self.cycle = CpuCycle::PCR;
                } else if compare_instruction_mask(self.instruction_register, I8008Ins::Lrr as u8, I8008Ins::Lrr.mask() ) {
                    let register_selector: u8 = self.instruction_register & (!I8008Ins::Lrr.mask());
                    self.register_b = self.get_scratchpad_register((register_selector & 0x04 ) >> 2 != 0, (register_selector & 0x02) >> 1 != 0, (register_selector & 0x01) != 0);
                     
                    t5 = true;
                }

                if t5 {
                    self.state_internal = CpuStateI::T5;
                    self.state_external = CpuState::T5;
                } else if t1 {
                    self.state_internal = CpuStateI::T1;
                    self.state_external = CpuState::T1;
                }
            },
            CpuStateI::T5 => {
                if compare_instruction_mask(self.instruction_register, I8008Ins::Lrr as u8, I8008Ins::Lrr.mask() ) {
                    let register_selector = self.instruction_register & (!I8008Ins::Lrr.mask());
                    self.set_scratchpad_register(self.register_b,
                        (register_selector & 0x20) >> 5 != 0,
                        (register_selector & 0x10) >> 4 != 0, 
                        (register_selector & 0x08) >> 3 != 0
                    );

                } if compare_instruction_mask(self.instruction_register, I8008Ins::INr as u8, I8008Ins::INr.mask() ) {
                    let register_selector = self.instruction_register & (!I8008Ins::INr.mask());

                    // we borrow register a for this
                    self.register_a = self.get_scratchpad_register(
                        (register_selector & 0x20) >> 5 != 0,
                        (register_selector & 0x10) >> 4 != 0, 
                        (register_selector & 0x08) >> 3 != 0
                        );
                    if self.register_a == 0xFF {
                        // 0xFF++ = 0 with overflow
                        self.flag_carry = true;
                        self.flag_zero = true;
                        self.flag_parity = true;
                    }
                    self.register_a += 1;
                    if self.register_a & 0x01 == 0x00 {
                        // LSB at 0
                        self.flag_parity = true;
                    } 
                    if self.register_a & 0x80 == 0x80 {
                        // MSB at 1
                        self.flag_sign = true;
                    }

                    self.set_scratchpad_register(self.register_a,
                        (register_selector & 0x20) >> 5 != 0,
                        (register_selector & 0x10) >> 4 != 0, 
                        (register_selector & 0x08) >> 3 != 0
                    );
                } if compare_instruction_mask(self.instruction_register, I8008Ins::DCr as u8, I8008Ins::DCr.mask() ) {
                     if self.register_b == 0x00 {
                        // 0x00-- = 0xFF with underflow
                        self.flag_carry = true;
                    }
                    self.register_b -= 1;
                    if self.register_b == 0x00 {
                        self.flag_zero = true;
                    }
                    if self.register_b & 0x01 == 0x00 {
                        // LSB at 0
                        self.flag_parity = true;
                    } 
                    if self.register_b & 0x80 == 0x80 {
                        // MSB at 1
                        self.flag_sign = true;
                    }

                    let register_selector = self.instruction_register & (!I8008Ins::DCr.mask());
                    self.set_scratchpad_register(self.register_b,
                        (register_selector & 0x20) >> 5 != 0,
                        (register_selector & 0x10) >> 4 != 0, 
                        (register_selector & 0x08) >> 3 != 0
                    );
                }
                self.state_internal = CpuStateI::T1;
                self.state_external = CpuState::T1;
            }
        };
    }
}

impl I8008MemoryController {
    pub fn new() -> Self {
        Self {
            memory: [0 as u8; 16384]
        }
    } 

    pub fn load_into(&mut self, start: usize, data: &[u8]) {
        let mut i = start;
        for value in data.into_iter() {
            self.memory[i] = *value;
            i+=1;
        } 
    }

    pub fn get_value(&self, address: u16) -> u8 {
        self.memory[u14_to_u16(address) as usize]
    }
}

pub fn into_state(s0: bool, s1: bool, s2: bool) -> CpuState {
    match (s0, s1, s2) {
        (false, false, false) => CpuState::WAIT,  // 000
        (false, false, true) => CpuState::T2,     // 001
        (false, true, false) => CpuState::T1,     // 010
        (false, true, true) => CpuState::T1I,     // 011
        (true, false, false) => CpuState::T3,     // 100
        (true, false, true) => CpuState::T5,      // 101
        (true, true, false) => CpuState::STOPPED, // 110
        (true, true, true) => CpuState::T4,       // 111
    }
}

#[inline(always)]
pub fn reverse_u8(data: u8) -> u8 {
    ((data & 0x80 ) >> 7) +
    ((data & 0x40 ) >> 5) +
    ((data & 0x20 ) >> 3) +
    ((data & 0x10 ) >> 1) +
    ((data & 0x08 ) << 1) +
    ((data & 0x04 ) << 3) +
    ((data & 0x02 ) << 5) +
    ((data & 0x01 ) << 7)
}

#[inline(always)]
pub fn u14_to_u16(address: u16) -> u16 {
    ((address & 0x8000) >> 15) + // D7
    ((address & 0x4000) >> 13) + // D6
    ((address & 0x2000) >> 11) + // D5
    ((address & 0x1000) >> 9) +  // D4
    ((address & 0x0800) >> 7) +  // D3
    ((address & 0x0400) >> 5) +  // D2
    ((address & 0x0200) >> 3) +  // D1
    ((address & 0x0100) >> 1) +  // D0
                                 // second byte
//  ((address & 0x0080) << 1) +  // X 
//  ((address & 0x0040) << 3) +  // X
    ((address & 0x0020) << 5) +  // D5
    ((address & 0x0010) << 7) +  // D4
    ((address & 0x0008) << 9) +  // D3
    ((address & 0x0004) << 11) + // D2
    ((address & 0x0002) << 13) + // D1
    ((address & 0x0001) << 15)   // D0
}

#[inline(always)]
fn compare_instruction_mask(data: u8, instruction: u8, mask: u8) -> bool {
    (data & mask) == instruction
}
