use std::collections::HashMap;
use crate::opcodes::{OpCode, OpCodeType, OPCODES};

pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
    None,
}

pub enum StatusFlag {
    CARRY = 0b0000_0001,
    ZERO = 0b0000_0010,
    INTERRUPT = 0b0000_0100,
    DECIMAL = 0b0000_1000,
    BREAK = 0b0001_0000,
    OVERFLOW = 0b0010_0000,
    NEGATIVE = 0b0100_0000
}

impl StatusFlag {
    fn val(self) -> u8 {
        return self as u8;
    }
}

// TODO: See if we need to optimize out the fn call
fn set_flag(status: u8, flag: StatusFlag) -> u8 {
    return status | flag.val()
}

fn clear_flag(status: u8, flag: StatusFlag) -> u8 {
    return status & !flag.val()
}

fn is_flag_set(status: u8, flag: StatusFlag) -> bool {
    return status & flag.val() != 0
}

struct Registers {
    a: u8,
    x: u8,
    y: u8,
    sp: u8,
}

impl Registers {
    pub fn new() -> Self {
        Registers {
            a: 0,
            x: 0,
            y: 0,
            sp: 0,
        }
    }
}

pub struct CPU {
    registers: Registers,
    opcodes: HashMap<u8, &'static OpCode>,
    status: u8,
    pc: u16,
    memory: [u8; 0xFFFF],
    halt_requested: bool
}

impl CPU {
    pub fn new() -> Self {
        // This could instead be function pointers on the CPU itself, but I decided it would make debugging
        // simpler to be able to unwrap in a match in the step function instead
        let mut opcodes = HashMap::new();
        for cpuop in &*OPCODES {
            opcodes.insert(cpuop.code, cpuop);
        }

        CPU {
            registers: Registers::new(),
            opcodes,
            status: 0,
            pc: 0,
            memory: [0; 0xFFFF],
            halt_requested: false
        }
    }

    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn get_address(&self, _mode: &AddressingMode) -> u16 {
        self.pc
    }

    fn set_zero_negative_flags(&mut self, register_val: u8) {
        if register_val == 0 {
            self.status = set_flag(self.status, StatusFlag::ZERO);
        } else {
            self.status = clear_flag(self.status, StatusFlag::ZERO)
        }

        if register_val & 0b1000_0000 != 0 {
            self.status = set_flag(self.status, StatusFlag::NEGATIVE);
        } else {
            self.status = clear_flag(self.status, StatusFlag::NEGATIVE);
        }
    }

    // I'm not good enough at Rust to figure out a better way to do this,
    // but this feels as clean to me as possible and the unsafe is OK because
    // we will not be multithreading
    // fn load(&mut self, param: u8, register: *mut u8) {
    //     unsafe {
    //         *register = param;
    //         self.set_zero_negative_flags(*register);
    //     }
    // }
    //
    // fn load(&mut self, mode: &AddressingMode, register: *mut u8) {
    //     let (addr, page_cross) = self.get_operand_address(&mode);
    //     let value = self.mem_read(addr);
    //     unsafe {
    //         *register = value;
    //         self.set_zero_negative_flags(*register);
    //     }
    //
    //     if page_cross {
    //         self.bus.tick(1);
    //     }
    // }

    fn ld(&mut self, mode: &AddressingMode, opcode_type: &OpCodeType) {
        // TODO: Should this take in the offset?
        // TODO: Should figure out who is allowed to update the pc
        let addr = self.get_address(&mode);
        let value = self.mem_read(addr);

        let r = match opcode_type {
            OpCodeType::LDA => { &mut self.registers.a }
            OpCodeType::LDX => { &mut self.registers.x }
            OpCodeType::LDY => { &mut self.registers.y }
            _ => panic!("Incorrect OpCode")
        };
        *r = value;

        self.set_zero_negative_flags(value);
    }

    pub fn load_program(&mut self, program: Vec<u8>) {
        self.memory[0x8000 .. (0x8000 + program.len())].copy_from_slice(&program[..]);
        self.pc = 0x8000;
    }

    pub fn run(&mut self) {
        loop {
            if self.halt_requested {
                break
            }
            self.step();
        }
    }

    fn step(&mut self) {
        let code = self.mem_read(self.pc);
        self.pc += 1; // TODO: Should do this or shouldn't until execute?

        let next_pc = if let Some(opcode) = self.opcodes.get(&code) {
            self.execute(opcode)
        } else {
            panic!("Unknown instruction found for: 0x{:x}", code);
        };

        self.pc = next_pc;
    }

    fn execute(&mut self, opcode: &OpCode) -> u16 {
        match opcode.opcode_type {
            OpCodeType::LDA | OpCodeType::LDX | OpCodeType::LDY => {
                self.ld(&opcode.mode, &opcode.opcode_type);
            }
            OpCodeType::TAX => {
                self.registers.x = self.registers.y;
                self.set_zero_negative_flags(self.registers.x);
            }
            OpCodeType::TAY => {
                self.registers.y = self.registers.a;
                self.set_zero_negative_flags(self.registers.y);
            }
            OpCodeType::TSX => {
                self.registers.x = self.registers.sp;
                self.set_zero_negative_flags(self.registers.x);
            }
            OpCodeType::TXA => {
                self.registers.a = self.registers.x;
                self.set_zero_negative_flags(self.registers.a);
            }
            OpCodeType::TXS => {
                self.registers.sp = self.registers.x;
                self.set_zero_negative_flags(self.registers.sp);
            }
            OpCodeType::TYA => {
                self.registers.a = self.registers.y;
                self.set_zero_negative_flags(self.registers.a);
            }
            OpCodeType::BRK => {
                self.halt_requested = true;
            }
        }

        // TODO: Better or worse with the -1 here?
        return self.pc + (opcode.len - 1) as u16;
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_program(vec![0xa9, 0x05, 0x00]);
        cpu.run();
        assert_eq!(cpu.registers.a, 0x05);
        assert!(!is_flag_set(cpu.status, StatusFlag::ZERO));
        assert!(!is_flag_set(cpu.status, StatusFlag::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_program(vec![0xa9, 0x00, 0x00]);
        cpu.run();
        assert!(is_flag_set(cpu.status, StatusFlag::ZERO));
    }
}