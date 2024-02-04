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

struct Status {
    pub val: u8
}

impl Status {
    pub fn new() -> Self {
        Status {
            val: 0
        }
    }

    pub fn set_flag(&mut self, flag: StatusFlag) {
        self.val |= flag.val()
    }

    fn update_flag(&mut self, bit: bool, flag: StatusFlag) {
        if bit {
            self.set_flag(flag)
        } else {
            self.clear_flag(flag)
        }
    }

    fn clear_flag(&mut self, flag: StatusFlag) {
        self.val & !flag.val();
    }

    fn is_flag_set(&self, flag: StatusFlag) -> bool {
        return self.val & flag.val() != 0
    }

    fn get_flag(&self, flag: StatusFlag) -> u8 {
        return self.val & flag.val()
    }
}

struct Registers {
    a: u8,
    x: u8,
    y: u8,
    sp: u8,
}

const STACK: u16 = 0x0100;
const STACK_RESET: u8 = 0xfd;

impl Registers {
    pub fn new() -> Self {
        Registers {
            a: 0,
            x: 0,
            y: 0,
            sp: STACK_RESET,
        }
    }
}

pub struct CPU {
    registers: Registers,
    opcodes: HashMap<u8, &'static OpCode>,
    status: Status,
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
            status: Status::new(),
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

    fn stack_push(&mut self, val: u8) {
        self.mem_write(STACK + self.registers.sp as u16, val);
        self.registers.sp = self.registers.sp.wrapping_sub(1);
    }

    fn stack_pop(&mut self) -> u8 {
        self.registers.sp = self.registers.sp.wrapping_add(1);
        self.mem_read(STACK + self.registers.sp as u16)
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
        self.status.update_flag(register_val == 0, StatusFlag::ZERO);
        self.status.update_flag(register_val & 0b1000_0000 != 0, StatusFlag::NEGATIVE);
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
    fn shift_operation(&mut self, mode: &AddressingMode, op:fn(u8, &Status) -> (u8, bool)) {
        let val = if mode == &AddressingMode::Accumulator { self.registers.a } else {
            let addr = self.get_address(&mode);
            self.mem_read(addr)
        };

        let (val, carry) = op(val, &self.status);
        self.status.update_flag(carry, StatusFlag::CARRY);

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

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(&mode);
        let val = self.mem_read(addr);

        self.status.update_flag(self.registers.a & val == 0, StatusFlag::ZERO);
        self.status.update_flag(val & 0b0100_0000 != 0, StatusFlag::OVERFLOW);
        self.status.update_flag(val & 0b1000_0000 != 0, StatusFlag::NEGATIVE);
    }

    fn add_to_accumulator(&mut self, val: u8) {
        let mut sum = self.registers.a as u16 + val as u16;
        if self.status.is_flag_set(StatusFlag::CARRY) {
            sum += 1;
        }

        self.status.update_flag(sum > 0xFF, StatusFlag::CARRY);

        let result = sum as u8;

        // Check if the value to add and the accumulator have the same sign and the value to add and new value
        // have different signs
        let overflow = (val & 0b1000_0000) == (self.registers.a & 0b1000_0000) &&
            (self.registers.a & 0b1000_0000) != (result & 0b1000_0000);
        self.status.update_flag(overflow, StatusFlag::OVERFLOW);

        self.registers.a = result;
        self.set_zero_negative_flags(result);
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(&mode);
        let val = self.mem_read(addr);

        self.add_to_accumulator(val);
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(&mode);
        let val = self.mem_read(addr) as i8;

        self.add_to_accumulator(val.wrapping_neg().wrapping_add(1) as u8);
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

            // Accumulator operations
            OpCodeType::ADC => self.adc(&opcode.mode),
            OpCodeType::SBC => self.sbc(&opcode.mode),

            // Branch operations
            OpCodeType::BCC => self.branch(!self.status.is_flag_set(StatusFlag::CARRY)),
            OpCodeType::BCS => self.branch(self.status.is_flag_set(StatusFlag::CARRY)),
            OpCodeType::BEQ => self.branch(self.status.is_flag_set(StatusFlag::ZERO)),
            OpCodeType::BNE => self.branch(!self.status.is_flag_set(StatusFlag::ZERO)),
            OpCodeType::BMI => self.branch(self.status.is_flag_set(StatusFlag::NEGATIVE)),
            OpCodeType::BPL => self.branch(!self.status.is_flag_set(StatusFlag::NEGATIVE)),
            OpCodeType::BVC => self.branch(!self.status.is_flag_set(StatusFlag::OVERFLOW)),
            OpCodeType::BVS => self.branch(self.status.is_flag_set(StatusFlag::OVERFLOW)),

            // Flag operations
            OpCodeType::CLC => self.status.clear_flag(StatusFlag::CARRY),
            OpCodeType::SEC => self.status.set_flag(StatusFlag::CARRY),
            OpCodeType::CLD => self.status.clear_flag(StatusFlag::DECIMAL),
            OpCodeType::SED => self.status.set_flag(StatusFlag::DECIMAL),
            OpCodeType::CLI => self.status.clear_flag(StatusFlag::INTERRUPT),
            OpCodeType::SEI => self.status.set_flag(StatusFlag::INTERRUPT),
            OpCodeType::CLV => self.status.clear_flag(StatusFlag::OVERFLOW),

            // Shift operations
            OpCodeType::ASL => self.shift_operation(&opcode.mode, |val: u8, _: &Status| {
                return (val << 1, val >> 7 == 1);
            }),
            OpCodeType::LSR => self.shift_operation(&opcode.mode, |val: u8, _: &Status| {
                return (val >> 1, val & 1 == 1);
            }),
            OpCodeType::ROL => self.shift_operation(&opcode.mode, |val: u8, status: &Status| {
                return (val << 1 | status.get_flag(StatusFlag::CARRY), val >> 7 == 1);
            }),
            OpCodeType::ROR => self.shift_operation(&opcode.mode, |val: u8, status: &Status| {
                return (val >> 1 | status.get_flag(StatusFlag::CARRY) << 7, val & 1 == 1);
            }),

            // Push operations
            OpCodeType::PHA => self.stack_push(self.registers.a),
            OpCodeType::PHP => self.stack_push(self.status.val),
            OpCodeType::PLA => self.registers.a = self.stack_pop(),
            OpCodeType::PLP => self.status.val = self.stack_pop(),

            // Logical operations
            OpCodeType::AND => self.and(&opcode.mode),
            OpCodeType::ORA => self.ora(&opcode.mode),
            OpCodeType::EOR => self.eor(&opcode.mode),

            // Misc operations
            OpCodeType::BIT => self.bit(&opcode.mode),

            // Special
            OpCodeType::BRK => {
                self.halt_requested = true;
            }
            OpCodeType::NOP => { }
            _ => panic!("Not yet implemented!")
        }

        // TODO: Better or worse with the -1 here?
        // TODO: Won't work with branches.
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
        assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_program(vec![0xa9, 0x00, 0x00]);
        cpu.run();
        assert!(cpu.status.is_flag_set(StatusFlag::ZERO));
    }
}