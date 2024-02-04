use lazy_static::lazy_static;
use crate::cpu::AddressingMode;

pub enum OpCodeType {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
}

pub struct OpCode {
    pub(crate) code: u8,
    pub(crate) opcode_type: OpCodeType,
    pub(crate) len: u8,
    pub(crate) mode: AddressingMode,
}

impl OpCode {
    fn new(code: u8, opcode_type: OpCodeType, len: u8, mode: AddressingMode) -> Self {
        OpCode {
            code,
            opcode_type,
            len,
            mode
        }
    }
}

lazy_static! {
    pub(crate) static ref OPCODES: Vec<OpCode> = vec![
        // TODO: Can get the num bytes from the type?
        OpCode::new(0x29, OpCodeType::AND, 2, AddressingMode::Immediate),
        OpCode::new(0x25, OpCodeType::AND, 2, AddressingMode::ZeroPage),
        OpCode::new(0x35, OpCodeType::AND, 2, AddressingMode::ZeroPageX),
        OpCode::new(0x2D, OpCodeType::AND, 3, AddressingMode::Absolute),
        OpCode::new(0x3D, OpCodeType::AND, 3, AddressingMode::AbsoluteX),
        OpCode::new(0x39, OpCodeType::AND, 3, AddressingMode::AbsoluteY),
        OpCode::new(0x21, OpCodeType::AND, 2, AddressingMode::IndexedIndirect),
        OpCode::new(0x31, OpCodeType::AND, 2, AddressingMode::IndirectIndexed),

        OpCode::new(0x00, OpCodeType::BRK, 1, AddressingMode::Implicit),

        OpCode::new(0xA9, OpCodeType::LDA, 2, AddressingMode::Immediate),
        OpCode::new(0xA5, OpCodeType::LDA, 2, AddressingMode::ZeroPage),
        OpCode::new(0xB5, OpCodeType::LDA, 2, AddressingMode::ZeroPageX),
        OpCode::new(0xAD, OpCodeType::LDA, 3, AddressingMode::Absolute),
        OpCode::new(0xBD, OpCodeType::LDA, 3, AddressingMode::AbsoluteX),
        OpCode::new(0xB9, OpCodeType::LDA, 3, AddressingMode::AbsoluteY),
        OpCode::new(0xA1, OpCodeType::LDA, 2, AddressingMode::IndexedIndirect),
        OpCode::new(0xB9, OpCodeType::LDA, 2, AddressingMode::IndirectIndexed),

        OpCode::new(0xA2, OpCodeType::LDX, 2, AddressingMode::Immediate),
        OpCode::new(0xA6, OpCodeType::LDX, 2, AddressingMode::ZeroPage),
        OpCode::new(0xB6, OpCodeType::LDX, 2, AddressingMode::ZeroPageY),
        OpCode::new(0xAE, OpCodeType::LDX, 3, AddressingMode::Absolute),
        OpCode::new(0xBE, OpCodeType::LDX, 3, AddressingMode::AbsoluteY),

        OpCode::new(0xA0, OpCodeType::LDY, 2, AddressingMode::Immediate),
        OpCode::new(0xA4, OpCodeType::LDY, 2, AddressingMode::ZeroPage),
        OpCode::new(0xB4, OpCodeType::LDY, 2, AddressingMode::ZeroPageX),
        OpCode::new(0xAC, OpCodeType::LDY, 3, AddressingMode::Absolute),
        OpCode::new(0xBC, OpCodeType::LDY, 3, AddressingMode::AbsoluteX),

        OpCode::new(0xEA, OpCodeType::NOP, 1, AddressingMode::Implicit),

        OpCode::new(0x09, OpCodeType::ORA, 2, AddressingMode::Immediate),
        OpCode::new(0x05, OpCodeType::ORA, 2, AddressingMode::ZeroPage),
        OpCode::new(0x15, OpCodeType::ORA, 2, AddressingMode::ZeroPageX),
        OpCode::new(0x0D, OpCodeType::ORA, 3, AddressingMode::Absolute),
        OpCode::new(0x1D, OpCodeType::ORA, 3, AddressingMode::AbsoluteX),
        OpCode::new(0x19, OpCodeType::ORA, 3, AddressingMode::AbsoluteY),
        OpCode::new(0x01, OpCodeType::ORA, 2, AddressingMode::IndexedIndirect),
        OpCode::new(0x11, OpCodeType::ORA, 2, AddressingMode::IndirectIndexed),

        OpCode::new(0x85, OpCodeType::STA, 2, AddressingMode::ZeroPage),
        OpCode::new(0x95, OpCodeType::STA, 2, AddressingMode::ZeroPageX),
        OpCode::new(0x8D, OpCodeType::STA, 3, AddressingMode::Absolute),
        OpCode::new(0x9D, OpCodeType::STA, 3, AddressingMode::AbsoluteX),
        OpCode::new(0x99, OpCodeType::STA, 3, AddressingMode::AbsoluteY),
        OpCode::new(0x81, OpCodeType::STA, 2, AddressingMode::IndexedIndirect),
        OpCode::new(0x91, OpCodeType::STA, 2, AddressingMode::IndirectIndexed),

        OpCode::new(0x86, OpCodeType::STX, 2, AddressingMode::ZeroPage),
        OpCode::new(0x96, OpCodeType::STX, 2, AddressingMode::ZeroPageY),
        OpCode::new(0x8E, OpCodeType::STX, 3, AddressingMode::Absolute),

        OpCode::new(0x84, OpCodeType::STY, 2, AddressingMode::ZeroPage),
        OpCode::new(0x94, OpCodeType::STY, 2, AddressingMode::ZeroPageY),
        OpCode::new(0x8C, OpCodeType::STY, 3, AddressingMode::Absolute),

        OpCode::new(0xAA, OpCodeType::TAX, 1, AddressingMode::Implicit),
        OpCode::new(0xA8, OpCodeType::TAY, 1, AddressingMode::Implicit),
        OpCode::new(0xBA, OpCodeType::TSX, 1, AddressingMode::Implicit),
        OpCode::new(0x8A, OpCodeType::TXA, 1, AddressingMode::Implicit),
        OpCode::new(0x9A, OpCodeType::TXS, 1, AddressingMode::Implicit),
        OpCode::new(0x98, OpCodeType::TYA, 1, AddressingMode::Implicit),
    ];
}

let r = match opcode_type {
OpCodeType::STA => { self.registers.a }
OpCodeType::STX => { self.registers.x }
OpCodeType::STY => { self.registers.y }
_ => panic!("Incorrect OpCode")
};