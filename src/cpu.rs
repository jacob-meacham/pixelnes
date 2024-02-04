use std::collections::HashMap;
use crate::opcodes::{OpCode, OpCodeType, OPCODES};

#[derive(PartialEq, Eq)]
pub enum AddressingMode {
    Immediate,
    Accumulator,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndexedIndirect,
    IndirectIndexed,
    Implicit,
    Relative,
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

// TODO: Make this a type instead? Or mutate?
// TODO: See if we need to optimize out the fn call
fn set_flag(status: &mut u8, flag: StatusFlag) {
    *status = *status | flag.val();
}

fn clear_flag(status: &mut u8, flag: StatusFlag) {
    *status = *status & !flag.val();
}

fn is_flag_set(status: u8, flag: StatusFlag) -> bool {
    return status & flag.val() != 0
}

fn get_flag(status: u8, flag: StatusFlag) -> u8 {
    return status & flag.val()
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

    // TODO: Move this to its own struct
    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_read_u16(&self, addr: u16) -> u16 {
        let (bytes, _) = self.memory.split_at(addr as usize);
        u16::from_le_bytes(bytes.try_into().unwrap())
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn get_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.pc,
            AddressingMode::ZeroPage => self.mem_read(self.pc) as u16,
            AddressingMode::ZeroPageX => {
                let base = self.mem_read(self.pc);
                base.wrapping_add(self.registers.x) as u16
            },
            AddressingMode::ZeroPageY => {
                let base = self.mem_read(self.pc);
                base.wrapping_add(self.registers.y) as u16
            },
            AddressingMode::Absolute => self.mem_read_u16(self.pc),
            AddressingMode::AbsoluteX => {
                let base = self.mem_read_u16(self.pc);
                base.wrapping_add(self.registers.x as u16)
            },
            AddressingMode::AbsoluteY => {
                let base = self.mem_read_u16(self.pc);
                base.wrapping_add(self.registers.y as u16)
            },
            AddressingMode::Indirect => {
                let base = self.mem_read(self.pc);
                self.mem_read_u16(base as u16)
            },
            AddressingMode::IndexedIndirect => {
                let base = self.mem_read(self.pc);
                let ptr = base.wrapping_add(self.registers.x);
                self.mem_read_u16(ptr as u16)
            },
            AddressingMode::IndirectIndexed => {
                let base = self.mem_read(self.pc);
                let ptr = self.mem_read_u16(base as u16);
                ptr.wrapping_add(self.registers.y as u16)
            }
            _ => panic!("Unsupported mode for dereferencing")
        }
    }

    fn set_zero_negative_flags(&mut self, register_val: u8) {
        if register_val == 0 {
            set_flag(&mut self.status, StatusFlag::ZERO);
        } else {
            clear_flag(&mut self.status, StatusFlag::ZERO)
        }

        if register_val & 0b1000_0000 != 0 {
            set_flag(&mut self.status, StatusFlag::NEGATIVE);
        } else {
            clear_flag(&mut self.status, StatusFlag::NEGATIVE);
        }
    }

    fn update_carry_flag(&mut self, bit: bool) {
        if bit {
            set_flag(&mut self.status, StatusFlag::CARRY)
        } else {
            clear_flag(&mut self.status, StatusFlag::CARRY)
        }
    }

    fn ld_(&mut self, mode: &AddressingMode, opcode_type: &OpCodeType) {
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

    fn st_(&mut self, val: u8, mode: &AddressingMode) {
        let addr = self.get_address(&mode);
        self.mem_write(addr, val);
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(&mode);
        let val = self.mem_read(addr);

        self.registers.a |= val;
        self.set_zero_negative_flags(self.registers.a);
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(&mode);
        let val = self.mem_read(addr);

        self.registers.a &= val;
        self.set_zero_negative_flags(self.registers.a);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(&mode);
        let val = self.mem_read(addr);

        self.registers.a ^= val;
        self.set_zero_negative_flags(self.registers.a);
    }

    // TODO: Fast enough?
    fn shift_operation(&mut self, mode: &AddressingMode, op:fn(u8, u8) -> (u8, bool)) {
        let val = if mode == &AddressingMode::Accumulator { self.registers.a } else {
            let addr = self.get_address(&mode);
            self.mem_read(addr)
        };

        let (val, carry) = op(val, self.status);
        self.update_carry_flag(carry);

        if mode == &AddressingMode::Accumulator {
            self.registers.a = val;
        } else {
            let addr = self.get_address(&mode);
            self.mem_write(addr, val);
        }

        self.set_zero_negative_flags(val);
    }

    fn branch(&mut self, condition: bool) {
        if condition {
            let jump = self.mem_read(self.pc);
            let addr = self.pc.wrapping_add(jump as u16).wrapping_add(1);

            // TODO: Should this be set here?
            self.pc = addr
        }
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
        // TODO: Should we just pull the address here instead of always passing it in?
        match opcode.opcode_type {
            // Load operations
            OpCodeType::LDA | OpCodeType::LDX | OpCodeType::LDY => {
                self.ld_(&opcode.mode, &opcode.opcode_type);
            }

            // Store operations
            OpCodeType::STA => self.st_(self.registers.a, &opcode.mode),
            OpCodeType::STX => self.st_(self.registers.x, &opcode.mode),
            OpCodeType::STY => self.st_(self.registers.y, &opcode.mode),
            OpCodeType::TAX => {
                self.registers.x = self.registers.y;
                self.set_zero_negative_flags(self.registers.x);
            }

            // Transfer operations
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
                // Doesn't impact flags
            }
            OpCodeType::TYA => {
                self.registers.a = self.registers.y;
                self.set_zero_negative_flags(self.registers.a);
            }

            // Branch operations
            OpCodeType::BCC => self.branch(!is_flag_set(self.status, StatusFlag::CARRY)),
            OpCodeType::BCS => self.branch(is_flag_set(self.status, StatusFlag::CARRY)),
            OpCodeType::BEQ => self.branch(is_flag_set(self.status, StatusFlag::ZERO)),
            OpCodeType::BNE => self.branch(!is_flag_set(self.status, StatusFlag::ZERO)),
            OpCodeType::BMI => self.branch(is_flag_set(self.status, StatusFlag::NEGATIVE)),
            OpCodeType::BPL => self.branch(!is_flag_set(self.status, StatusFlag::NEGATIVE)),
            OpCodeType::BVC => self.branch(!is_flag_set(self.status, StatusFlag::OVERFLOW)),
            OpCodeType::BVS => self.branch(is_flag_set(self.status, StatusFlag::OVERFLOW)),

            // Flag operations
            OpCodeType::CLC => clear_flag(&mut self.status, StatusFlag::CARRY),
            OpCodeType::SEC => set_flag(&mut self.status, StatusFlag::CARRY),
            OpCodeType::CLD => clear_flag(&mut self.status, StatusFlag::DECIMAL),
            OpCodeType::SED => set_flag(&mut self.status, StatusFlag::DECIMAL),
            OpCodeType::CLI => clear_flag(&mut self.status, StatusFlag::INTERRUPT),
            OpCodeType::SEI => set_flag(&mut self.status, StatusFlag::INTERRUPT),
            OpCodeType::CLV => clear_flag(&mut self.status, StatusFlag::OVERFLOW),

            // Shift operations
            OpCodeType::ASL => self.shift_operation(&opcode.mode, |val: u8, _: u8| {
                return (val << 1, val >> 7 == 1);
            }),
            OpCodeType::LSR => self.shift_operation(&opcode.mode, |val: u8, _: u8| {
                return (val >> 1, val & 1 == 1);
            }),
            OpCodeType::ROL => self.shift_operation(&opcode.mode, |val: u8, status: u8| {
                return (val << 1 | get_flag(status, StatusFlag::CARRY), val >> 7 == 1);
            }),
            OpCodeType::ROR => self.shift_operation(&opcode.mode, |val: u8, status: u8| {
                return (val >> 1 | get_flag(status, StatusFlag::CARRY) << 7, val & 1 == 1);
            }),

            // Logical operations
            OpCodeType::AND => self.and(&opcode.mode),
            OpCodeType::ORA => self.ora(&opcode.mode),
            OpCodeType::EOR => self.eor(&opcode.mode),

            // Special
            OpCodeType::BRK => {
                self.halt_requested = true;
            }
            OpCodeType::NOP => { }
            _ => panic!("Not yet implemented!")
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