use std::collections::HashMap;
use crate::opcodes::{OpCode, OpCodeType, OPCODES};

// TODO: Should functions be responsible for consuming their operand? More state in the functions then
// TODO: Should get_address be separated from the consume? Then we'd need to peel out the next_u8/u16 from consume_address and have another match case...

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
    OVERFLOW = 0b0100_0000,
    NEGATIVE = 0b1000_0000
}

impl StatusFlag {
    fn val(self) -> u8 {
        return self as u8;
    }
}

#[derive(Clone)]
struct Status {
    pub val: u8
}

impl Status {
    pub fn new() -> Self {
        Status {
            val: 0b0010_0000 // This bit is always set - see https://www.nesdev.org/wiki/Status_flags
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
        self.val &= !flag.val();
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
        let bytes = &self.memory[addr as usize..addr as usize + 2];
        u16::from_le_bytes(bytes.try_into().unwrap())
    }

    // TODO: Would it be better to only ever change the PC at the end of step or in the loop?
    fn next_u8(&mut self) -> u8 {
        let val = self.mem_read(self.pc);
        self.pc += 1;

        val
    }

    fn next_u16(&mut self) -> u16 {
        let val = self.mem_read_u16(self.pc);
        self.pc += 2;

        val
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        let [b1, b2] = u16::to_le_bytes(data);
        self.memory[addr as usize] = b1;
        self.memory[(addr + 1) as usize] = b2;
    }

    fn stack_push(&mut self, val: u8) {
        self.mem_write(STACK + self.registers.sp as u16, val);
        self.registers.sp = self.registers.sp.wrapping_sub(1);
    }

    fn stack_push_u16(&mut self, val: u16) {
        let hi = (val >> 8) as u8;
        let lo = (val & 0xff) as u8;
        self.stack_push(hi);
        self.stack_push(lo);
    }

    fn stack_pop(&mut self) -> u8 {
        self.registers.sp = self.registers.sp.wrapping_add(1);
        self.mem_read(STACK + self.registers.sp as u16)
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let lo = self.stack_pop() as u16;
        let hi = self.stack_pop() as u16;

        hi << 8 | lo
    }

    fn consume_address(&mut self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => {
                let val = self.pc;
                self.pc += 1;

                val
            },
            AddressingMode::ZeroPage => self.next_u8() as u16,
            AddressingMode::ZeroPageX => {
                let base = self.next_u8();
                base.wrapping_add(self.registers.x) as u16
            },
            AddressingMode::ZeroPageY => {
                let base = self.next_u8();
                base.wrapping_add(self.registers.y) as u16
            },
            AddressingMode::Absolute => self.next_u16(),
            AddressingMode::AbsoluteX => {
                let base = self.next_u16();
                base.wrapping_add(self.registers.x as u16)
            },
            AddressingMode::AbsoluteY => {
                let base = self.next_u16();
                base.wrapping_add(self.registers.y as u16)
            },
            AddressingMode::Indirect => {
                // TODO: Errata in JMP http://wiki.nesdev.com/w/index.php/Errata
                let base = self.next_u16();
                self.mem_read_u16(base)
            },
            AddressingMode::IndexedIndirect => {
                let base = self.next_u8();
                let ptr = base.wrapping_add(self.registers.x);
                self.mem_read_u16(ptr as u16)
            },
            AddressingMode::IndirectIndexed => {
                let base = self.next_u8();
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
        let addr = self.consume_address(&mode);
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
        let addr = self.consume_address(&mode);
        self.mem_write(addr, val);
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.consume_address(&mode);
        let val = self.mem_read(addr).wrapping_add(1);
        self.mem_write(addr, val);

        self.set_zero_negative_flags(val)
    }

    fn in_(&mut self, opcode_type: &OpCodeType) {
        let r = match opcode_type {
            OpCodeType::INX => { &mut self.registers.x }
            OpCodeType::INY => { &mut self.registers.y }
            _ => panic!("Incorrect Opcode")
        };
        let val = r.wrapping_add(1);
        *r = val;

        self.set_zero_negative_flags(val);
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.consume_address(&mode);
        let val = self.mem_read(addr).wrapping_sub(1);
        self.mem_write(addr, val);

        self.set_zero_negative_flags(val)
    }

    fn de_(&mut self, opcode_type: &OpCodeType) {
        let r = match opcode_type {
            OpCodeType::DEX => { &mut self.registers.x }
            OpCodeType::DEY => { &mut self.registers.y }
            _ => panic!("Incorrect Opcode")
        };
        let val = r.wrapping_sub(1);
        *r = val;

        self.set_zero_negative_flags(val);
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.consume_address(&mode);
        let val = self.mem_read(addr);

        self.registers.a |= val;
        self.set_zero_negative_flags(self.registers.a);
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.consume_address(&mode);
        let val = self.mem_read(addr);

        self.registers.a &= val;
        self.set_zero_negative_flags(self.registers.a);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.consume_address(&mode);
        let val = self.mem_read(addr);

        self.registers.a ^= val;
        self.set_zero_negative_flags(self.registers.a);
    }

    fn shift_operation(&mut self, mode: &AddressingMode, op:fn(u8, &Status) -> (u8, bool)) {
        let (val, addr) = if mode == &AddressingMode::Accumulator { (self.registers.a, None) } else {
            let addr = self.consume_address(&mode);
            (self.mem_read(addr), Some(addr))
        };

        let (val, carry) = op(val, &self.status);
        self.status.update_flag(carry, StatusFlag::CARRY);

        if mode == &AddressingMode::Accumulator {
            self.registers.a = val;
        } else {
            self.mem_write(addr.unwrap(), val);
        }

        self.set_zero_negative_flags(val);
    }

    fn branch(&mut self, condition: bool) {
        let rel = self.next_u8();
        if condition {
            let addr = self.pc.wrapping_add(rel as u16);
            self.pc = addr
        }
    }

    fn compare(&mut self, mode: &AddressingMode, compare_val: u8) {
        let addr = self.consume_address(&mode);
        let val = self.mem_read(addr);
        self.status.update_flag(compare_val >= val, StatusFlag::CARRY);

        self.set_zero_negative_flags(compare_val.wrapping_sub(val));
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.consume_address(&mode);
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
        let addr = self.consume_address(&mode);
        let val = self.mem_read(addr);

        self.add_to_accumulator(val);
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.consume_address(&mode);
        let val = self.mem_read(addr) as i8;

        self.add_to_accumulator(val.wrapping_neg().wrapping_add(1) as u8);
    }

    pub fn reset(&mut self) {
        self.registers = Registers::new();
        self.status = Status::new();
        self.pc = self.mem_read_u16(0xFFFC);
    }

    pub fn load_program(&mut self, program: Vec<u8>) {
        self.memory[0x8000 .. (0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
        self.reset();
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
        let code = self.next_u8();
        let opcode = self.opcodes
            .get(&code)
            .expect(&*format!("Unknown instruction found for: 0x{:x}", code));

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

            // Inc/Dec operations
            OpCodeType::INC => self.inc(&opcode.mode),
            OpCodeType::INX | OpCodeType::INY => self.in_(&opcode.opcode_type),
            OpCodeType::DEC => self.dec(&opcode.mode),
            OpCodeType::DEX | OpCodeType::DEY => self.de_(&opcode.opcode_type),

            // Branch operations
            OpCodeType::BCC => self.branch(!self.status.is_flag_set(StatusFlag::CARRY)),
            OpCodeType::BCS => self.branch(self.status.is_flag_set(StatusFlag::CARRY)),
            OpCodeType::BEQ => self.branch(self.status.is_flag_set(StatusFlag::ZERO)),
            OpCodeType::BNE => self.branch(!self.status.is_flag_set(StatusFlag::ZERO)),
            OpCodeType::BMI => self.branch(self.status.is_flag_set(StatusFlag::NEGATIVE)),
            OpCodeType::BPL => self.branch(!self.status.is_flag_set(StatusFlag::NEGATIVE)),
            OpCodeType::BVC => self.branch(!self.status.is_flag_set(StatusFlag::OVERFLOW)),
            OpCodeType::BVS => self.branch(self.status.is_flag_set(StatusFlag::OVERFLOW)),

            // Compare operations
            OpCodeType::CMP => self.compare(&opcode.mode, self.registers.a),
            OpCodeType::CPX => self.compare(&opcode.mode, self.registers.x),
            OpCodeType::CPY => self.compare(&opcode.mode, self.registers.y),

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

            // Push/pull operations
            OpCodeType::PHA => self.stack_push(self.registers.a),
            OpCodeType::PHP => {
                let mut flags = self.status.clone();
                flags.set_flag(StatusFlag::BREAK);
                self.stack_push(flags.val)
            },
            OpCodeType::PLA => self.registers.a = self.stack_pop(),
            OpCodeType::PLP => {
                self.status.val = self.stack_pop();
                self.status.clear_flag(StatusFlag::BREAK);
            },

            // Jump operations
            OpCodeType::JMP => {
                let addr = self.consume_address(&opcode.mode);
                self.pc = addr
            }
            OpCodeType::JSR => {
                let addr = self.consume_address(&opcode.mode);
                self.stack_push_u16(self.pc.wrapping_sub(1));
                self.pc = addr
            }

            // Return operations
            OpCodeType::RTI => {
                self.status.val = self.stack_pop();
                self.pc = self.stack_pop_u16();
            }

            OpCodeType::RTS => {
                self.pc = self.stack_pop_u16().wrapping_add(1);
            }

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
            OpCodeType::NOP => {
                // Do nothing
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_mem_read() {
        let mut cpu = CPU::new();

        cpu.memory[0x1234] = 0xAB;
        assert_eq!(cpu.mem_read(0x1234), 0xAB);
    }

    #[test]
    fn test_mem_write() {
        let mut cpu = CPU::new();

        cpu.mem_write(0x1234, 0xCD);
        assert_eq!(cpu.memory[0x1234], 0xCD);
    }

    #[test]
    fn test_set_zero_negative_flags() {
        let mut cpu = CPU::new();

        cpu.set_zero_negative_flags(0);
        assert!(cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));

        cpu.set_zero_negative_flags(128);
        assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(cpu.status.is_flag_set(StatusFlag::NEGATIVE));
    }

    #[test]
    fn test_stack_push_pop() {
        let mut cpu = CPU::new();

        cpu.stack_push(123);
        assert_eq!(cpu.stack_pop(), 123);
    }

    #[test]
    fn test_stack_push_pop_u16() {
        let mut cpu = CPU::new();

        cpu.stack_push_u16(0xFFAA);
        assert_eq!(cpu.stack_pop_u16(), 0xFFAA);
    }

    #[test]
    fn test_consume_address_immediate() {
        let mut cpu = CPU::new();
        cpu.pc = 0x400;
        let addr = cpu.consume_address(&AddressingMode::Immediate);
        assert_eq!(addr, 0x400);
        assert_eq!(cpu.pc, 0x401);
    }

    #[test]
    fn test_consume_address_zero_page() {
        let mut cpu = CPU::new();
        cpu.pc = 0x400;
        cpu.memory[0x400] = 0x10;
        let addr = cpu.consume_address(&AddressingMode::ZeroPage);

        assert_eq!(addr, 0x10);
        assert_eq!(cpu.pc, 0x401);
    }

    #[test]
    fn test_consume_address_zero_page_x() {
        let mut cpu = CPU::new();
        cpu.pc = 0x400;
        cpu.memory[0x400] = 0x10;
        cpu.registers.x = 0x05;

        let addr = cpu.consume_address(&AddressingMode::ZeroPageX);
        assert_eq!(addr, 0x15);
        assert_eq!(cpu.pc, 0x401);
    }

    #[test]
    fn test_consume_address_zero_page_y() {
        let mut cpu = CPU::new();
        cpu.pc = 0x400;
        cpu.memory[0x400] = 0x10;
        cpu.registers.y = 0x07;

        let addr = cpu.consume_address(&AddressingMode::ZeroPageY);
        assert_eq!(addr, 0x17);
        assert_eq!(cpu.pc, 0x401);
    }

    #[test]
    fn test_consume_address_absolute() {
        let mut cpu = CPU::new();
        cpu.pc = 0x400;
        cpu.memory[0x400] = 0x10;
        cpu.memory[0x401] = 0x2a;

        let addr = cpu.consume_address(&AddressingMode::Absolute);
        assert_eq!(addr, 0x2a10);
        assert_eq!(cpu.pc, 0x402);
    }

    #[test]
    fn test_consume_address_absolute_x() {
        let mut cpu = CPU::new();
        cpu.pc = 0x400;
        cpu.memory[0x400] = 0x10;
        cpu.memory[0x401] = 0x2a;
        cpu.registers.x = 0x05;

        let addr = cpu.consume_address(&AddressingMode::AbsoluteX);
        assert_eq!(addr, 0x2a15);
        assert_eq!(cpu.pc, 0x402);
    }

    #[test]
    fn test_consume_address_absolute_y() {
        let mut cpu = CPU::new();
        cpu.pc = 0x400;
        cpu.memory[0x400] = 0x10;
        cpu.memory[0x401] = 0x2a;
        cpu.registers.y = 0x07;

        let addr = cpu.consume_address(&AddressingMode::AbsoluteY);
        assert_eq!(addr, 0x2a17);
        assert_eq!(cpu.pc, 0x402);
    }

    #[test]
    fn test_consume_address_indirect() {
        let mut cpu = CPU::new();
        cpu.pc = 0x400;
        cpu.mem_write_u16(0x400, 0x102a);
        cpu.mem_write_u16(0x102a, 0xf11a);

        let addr = cpu.consume_address(&AddressingMode::Indirect);
        assert_eq!(addr, 0xf11a);
        assert_eq!(cpu.pc, 0x402);
    }

    #[test]
    fn test_consume_address_indexed_indirect() {
        let mut cpu = CPU::new();
        cpu.pc = 0x400;
        cpu.registers.x = 0x11;
        cpu.memory[0x400] = 0x2a;
        cpu.mem_write_u16(0x2a + 0x11, 0xfaff);

        let addr = cpu.consume_address(&AddressingMode::IndexedIndirect);
        assert_eq!(addr, 0xfaff);
        assert_eq!(cpu.pc, 0x401);
    }

    #[test]
    fn test_consume_address_indirect_indexed() {
        let mut cpu = CPU::new();
        cpu.pc = 0x400;
        cpu.registers.y = 0x11;
        cpu.memory[0x400] = 0x2a;
        cpu.mem_write_u16(0x2a, 0xfaff);

        let addr = cpu.consume_address(&AddressingMode::IndirectIndexed);
        assert_eq!(addr, 0xfaff + 0x11);
        assert_eq!(cpu.pc, 0x401);
    }

    #[test]
    fn test_ld_() {
        let test_cases = vec![
            &OpCodeType::LDA,
            &OpCodeType::LDX,
            &OpCodeType::LDY,
        ];


        for typ in test_cases {
            let mut cpu = CPU::new();
            cpu.load_program(vec![0x05, 0x00]);
            cpu.ld_(&AddressingMode::Immediate, typ);

            assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
            assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));

            match typ {
                OpCodeType::LDA => assert_eq!(cpu.registers.a, 0x05),
                OpCodeType::LDX => assert_eq!(cpu.registers.x, 0x05),
                OpCodeType::LDY => assert_eq!(cpu.registers.y, 0x05),
                _ => panic!("Unknown opcode")
            }
        }
    }

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

    #[test]
    #[should_panic]
    fn test_ld_incorrect_opcode() {
        let mut cpu = CPU::new();
        cpu.load_program(vec![0x05, 0x00]);
        cpu.ld_(&AddressingMode::Immediate, &OpCodeType::ORA);
    }

    #[test]
    fn test_st() {
        let mut cpu = CPU::new();
        cpu.pc = 0xab;
        cpu.memory[0xab] = 0x12;
        cpu.st_(0xaa, &AddressingMode::Immediate);
        assert_eq!(cpu.memory[0xab], 0xaa);
    }

    #[test]
    fn test_inc() {
        let mut cpu = CPU::new();
        cpu.pc = 0xab;
        cpu.memory[0xab] = 0x12;
        cpu.inc(&AddressingMode::Immediate);
        assert_eq!(cpu.memory[0xab], 0x13);
        assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));

        cpu.pc = 0xab;
        cpu.memory[0xab] = 0xff;
        cpu.inc(&AddressingMode::Immediate);
        assert_eq!(cpu.memory[0xab], 0);
        assert!(cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));
    }

    #[test]
    fn test_in_() {
        let test_cases = vec![
            &OpCodeType::INX,
            &OpCodeType::INY,
        ];


        for typ in test_cases {
            let mut cpu = CPU::new();
            cpu.registers.x = 0x05;
            cpu.registers.y = 0x05;
            cpu.in_(typ);

            assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
            assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));

            match typ {
                OpCodeType::INX => assert_eq!(cpu.registers.x, 0x06),
                OpCodeType::INY => assert_eq!(cpu.registers.y, 0x06),
                _ => panic!("Unknown opcode")
            }
        }
    }

    #[test]
    fn test_dec() {
        let mut cpu = CPU::new();
        cpu.pc = 0xab;
        cpu.memory[0xab] = 0x12;
        cpu.dec(&AddressingMode::Immediate);
        assert_eq!(cpu.memory[0xab], 0x11);
        assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));

        cpu.pc = 0xab;
        cpu.memory[0xab] = 0x01;
        cpu.dec(&AddressingMode::Immediate);
        assert_eq!(cpu.memory[0xab], 0x00);
        assert!(cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));
    }

    #[test]
    fn test_de_() {
        let test_cases = vec![
            &OpCodeType::DEX,
            &OpCodeType::DEY,
        ];


        for typ in test_cases {
            let mut cpu = CPU::new();
            cpu.registers.x = 0x05;
            cpu.registers.y = 0x05;
            cpu.de_(typ);

            assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
            assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));

            match typ {
                OpCodeType::DEX => assert_eq!(cpu.registers.x, 0x04),
                OpCodeType::DEY => assert_eq!(cpu.registers.y, 0x04),
                _ => panic!("Unknown opcode")
            }
        }
    }

    #[test]
    fn test_ora() {
        let mut cpu = CPU::new();
        cpu.registers.a = 0b1010_1010;
        cpu.memory[0xab] = 0b0101_0101;
        cpu.pc = 0xab;

        cpu.ora(&AddressingMode::Immediate);
        assert_eq!(cpu.registers.a, 0b1111_1111);
        assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(cpu.status.is_flag_set(StatusFlag::NEGATIVE));
    }

    #[test]
    fn test_and() {
        let mut cpu = CPU::new();
        cpu.registers.a = 0b1010_1010;
        cpu.memory[0xab] = 0b0101_0101;
        cpu.pc = 0xab;

        cpu.and(&AddressingMode::Immediate);
        assert_eq!(cpu.registers.a, 0b0000_0000);
        assert!(cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));
    }

    #[test]
    fn test_eor() {
        let mut cpu = CPU::new();
        cpu.registers.a = 0b1010_1010;
        cpu.memory[0xab] = 0b1101_1010;
        cpu.pc = 0xab;

        cpu.eor(&AddressingMode::Immediate);
        assert_eq!(cpu.registers.a, 0b0111_0000);
        assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));
    }

    #[test]
    fn test_shift_operation() {
        let mut cpu = CPU::new();
        cpu.registers.a = 0x0a;

        cpu.shift_operation(&AddressingMode::Accumulator, |val: u8, _: &Status| -> (u8, bool) {
            assert_eq!(val, 0x0a);

            (0x1a, true)
        });

        assert_eq!(cpu.registers.a, 0x1a);

        assert!(cpu.status.is_flag_set(StatusFlag::CARRY));
        assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));

        cpu.memory[0xab] = 0x0a;
        cpu.pc = 0xab;

        cpu.shift_operation(&AddressingMode::Immediate, |val: u8, _: &Status| -> (u8, bool) {
            assert_eq!(val, 0x0a);

            (0x00, false)
        });

        assert_eq!(cpu.memory[0xab], 0x00);
        assert!(!cpu.status.is_flag_set(StatusFlag::CARRY));
        assert!(cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));
    }

    #[test]
    fn test_branch() {
        let mut cpu = CPU::new();
        cpu.memory[0x400] = 0x20;
        cpu.pc = 0x400;
        cpu.branch(true);
        assert_eq!(cpu.pc, 0x421);

        cpu.pc = 0x400;
        cpu.branch(false);
        assert_eq!(cpu.pc, 0x401);
    }

    #[test]
    fn test_compare() {
        let mut cpu = CPU::new();
        cpu.memory[0x200] = 0x80;
        cpu.pc = 0x200;

        cpu.compare(&AddressingMode::Immediate, 0x80);
        assert!(cpu.status.is_flag_set(StatusFlag::CARRY));
        assert!(cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));

        cpu.pc = 0x200;
        cpu.compare(&AddressingMode::Immediate, 0x70);
        assert!(!cpu.status.is_flag_set(StatusFlag::CARRY));
        assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(cpu.status.is_flag_set(StatusFlag::NEGATIVE));

        cpu.pc = 0x200;
        cpu.compare(&AddressingMode::Immediate, 0x90);
        assert!(cpu.status.is_flag_set(StatusFlag::CARRY));
        assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));
    }

    #[test]
    fn test_bit() {
        let mut cpu = CPU::new();
        cpu.registers.a = 0b1001_0000;
        cpu.memory[0x200] = 0b1101_0001;
        cpu.pc = 0x200;
        cpu.bit(&AddressingMode::Immediate);
        assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(cpu.status.is_flag_set(StatusFlag::OVERFLOW));
        assert!(cpu.status.is_flag_set(StatusFlag::NEGATIVE));

        cpu.registers.a = 0b1001_0000;
        cpu.memory[0x200] = 0b0010_0101;
        cpu.pc = 0x200;
        cpu.bit(&AddressingMode::Immediate);
        assert!(cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::OVERFLOW));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));
    }

    #[test]
    fn test_add_to_accumulator_without_carry() {
        let mut cpu = CPU::new();
        cpu.registers.a = 0x20;
        cpu.add_to_accumulator(0x10);
        assert_eq!(cpu.registers.a, 0x30);
        assert!(!cpu.status.is_flag_set(StatusFlag::CARRY));
        assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));
        assert!(!cpu.status.is_flag_set(StatusFlag::OVERFLOW));
    }

    #[test]
    fn test_add_to_accumulator_with_carry() {
        let mut cpu = CPU::new();
        cpu.registers.a = 0x90;
        cpu.add_to_accumulator(0x90);
        assert_eq!(cpu.registers.a, 0x20);
        assert!(cpu.status.is_flag_set(StatusFlag::CARRY));
        assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));
        assert!(cpu.status.is_flag_set(StatusFlag::OVERFLOW));
    }

    #[test]
    fn test_add_to_accumulator_with_overflow() {
        let mut cpu = CPU::new();
        cpu.registers.a = 0x50;
        cpu.add_to_accumulator(0x50);
        assert_eq!(cpu.registers.a, 0xA0);
        assert!(!cpu.status.is_flag_set(StatusFlag::CARRY));
        assert!(!cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(cpu.status.is_flag_set(StatusFlag::NEGATIVE));
        assert!(cpu.status.is_flag_set(StatusFlag::OVERFLOW));
    }

    #[test]
    fn test_add_to_accumulator_result_zero() {
        let mut cpu = CPU::new();
        cpu.registers.a = 0x80;
        cpu.add_to_accumulator(0x80);
        assert_eq!(cpu.registers.a, 0x0);
        assert!(cpu.status.is_flag_set(StatusFlag::CARRY));
        assert!(cpu.status.is_flag_set(StatusFlag::ZERO));
        assert!(!cpu.status.is_flag_set(StatusFlag::NEGATIVE));
        assert!(cpu.status.is_flag_set(StatusFlag::OVERFLOW));
    }

    #[test]
    fn test_adc() {
        let mut cpu = CPU::new();
        cpu.registers.a = 20;
        cpu.memory[0x200] = 30;
        cpu.pc = 0x200;
        cpu.adc(&AddressingMode::Immediate);

        assert_eq!(cpu.registers.a, 50);
    }

    #[test]
    fn test_adc_with_carry() {
        let mut cpu = CPU::new();
        cpu.registers.a = 130;
        cpu.memory[0x200] = 130;
        cpu.pc = 0x200;

        cpu.status.set_flag(StatusFlag::CARRY);
        cpu.adc(&AddressingMode::Immediate);
        assert_eq!(cpu.registers.a, (260 % 255) as u8);
    }

    #[test]
    fn test_sbc() {
        let mut cpu = CPU::new();
        cpu.registers.a = 100;
        cpu.memory[0x200] = 30;
        cpu.pc = 0x200;

        cpu.sbc(&AddressingMode::Immediate);
        assert_eq!(cpu.registers.a, 71);
    }

    #[test]
    fn test_sbc_with_borrow() {
        let mut cpu = CPU::new();
        cpu.registers.a = 30;
        cpu.memory[0x200] = 100;
        cpu.pc = 0x200;

        cpu.sbc(&AddressingMode::Immediate);
        assert_eq!(cpu.registers.a, (70_i8.wrapping_neg() + 1) as u8);
    }

    #[test]
    fn test_smoke_tests() {
        
    }
}