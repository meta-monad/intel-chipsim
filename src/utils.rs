use crate::i8008::*;

pub struct MemoryController {
    pub memory: [u8; 16384], // 16K memory
}

impl MemoryController {
    pub fn new() -> Self {
        Self {
            memory: [0 as u8; 16384],
        }
    }

    pub fn load_into(&mut self, data: Vec<u8>) {
        self.memory[..data.len()].copy_from_slice(&data);
    }

    pub fn get_value(&self, address: u16) -> u8 {
        self.memory[u14_to_u16(address) as usize]
    }
}
