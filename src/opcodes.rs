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
    pub(crate) mode: AddressingMode,
}

impl OpCode {
    // TODO: If we decide we want the number of bytes, we can get it from here
    fn new(code: u8, opcode_type: OpCodeType, mode: AddressingMode) -> Self {
        OpCode {
            code,
            opcode_type,
            mode
        }
    }
}

lazy_static! {
    pub(crate) static ref OPCODES: Vec<OpCode> = vec![
       OpCode::new(0x69, OpCodeType::ADC, AddressingMode::Immediate),
       OpCode::new(0x65, OpCodeType::ADC, AddressingMode::ZeroPage),
       OpCode::new(0x75, OpCodeType::ADC, AddressingMode::ZeroPageX),
       OpCode::new(0x6D, OpCodeType::ADC, AddressingMode::Absolute),
       OpCode::new(0x7D, OpCodeType::ADC, AddressingMode::AbsoluteX),
       OpCode::new(0x79, OpCodeType::ADC, AddressingMode::AbsoluteY),
       OpCode::new(0x61, OpCodeType::ADC, AddressingMode::IndirectIndexed),
       OpCode::new(0x71, OpCodeType::ADC, AddressingMode::IndexedIndirect),

       OpCode::new(0x29, OpCodeType::AND, AddressingMode::Immediate),
       OpCode::new(0x25, OpCodeType::AND, AddressingMode::ZeroPage),
       OpCode::new(0x35, OpCodeType::AND, AddressingMode::ZeroPageX),
       OpCode::new(0x2D, OpCodeType::AND, AddressingMode::Absolute),
       OpCode::new(0x3D, OpCodeType::AND, AddressingMode::AbsoluteX),
       OpCode::new(0x39, OpCodeType::AND, AddressingMode::AbsoluteY),
       OpCode::new(0x21, OpCodeType::AND, AddressingMode::IndexedIndirect),
       OpCode::new(0x31, OpCodeType::AND, AddressingMode::IndirectIndexed),

       OpCode::new(0x0A, OpCodeType::ASL, AddressingMode::Accumulator),
       OpCode::new(0x06, OpCodeType::ASL, AddressingMode::ZeroPage),
       OpCode::new(0x16, OpCodeType::ASL, AddressingMode::ZeroPageX),
       OpCode::new(0x0E, OpCodeType::ASL, AddressingMode::Absolute),
       OpCode::new(0x1E, OpCodeType::ASL, AddressingMode::AbsoluteX),

       OpCode::new(0x90, OpCodeType::BCC, AddressingMode::Relative),
       OpCode::new(0xB0, OpCodeType::BCS, AddressingMode::Relative),
       OpCode::new(0xF0, OpCodeType::BEQ, AddressingMode::Relative),

       OpCode::new(0x24, OpCodeType::BIT, AddressingMode::ZeroPage),
       OpCode::new(0x2C, OpCodeType::BIT, AddressingMode::Absolute),

       OpCode::new(0x30, OpCodeType::BMI, AddressingMode::Relative),
       OpCode::new(0xD0, OpCodeType::BNE, AddressingMode::Relative),
       OpCode::new(0xD0, OpCodeType::BPL, AddressingMode::Relative),

       OpCode::new(0x00, OpCodeType::BRK, AddressingMode::Implicit),

       OpCode::new(0x50, OpCodeType::BVC, AddressingMode::Relative),
       OpCode::new(0x70, OpCodeType::BVS, AddressingMode::Relative),

       OpCode::new(0x18, OpCodeType::CLC, AddressingMode::Implicit),
       OpCode::new(0xD8, OpCodeType::CLD, AddressingMode::Implicit),
       OpCode::new(0x58, OpCodeType::CLI, AddressingMode::Implicit),
       OpCode::new(0xB8, OpCodeType::CLV, AddressingMode::Implicit),

       OpCode::new(0xC9, OpCodeType::CMP, AddressingMode::Immediate),
       OpCode::new(0xC5, OpCodeType::CMP, AddressingMode::ZeroPage),
       OpCode::new(0xD5, OpCodeType::CMP, AddressingMode::ZeroPageX),
       OpCode::new(0xCD, OpCodeType::CMP, AddressingMode::Absolute),
       OpCode::new(0xDD, OpCodeType::CMP, AddressingMode::AbsoluteX),
       OpCode::new(0xD9, OpCodeType::CMP, AddressingMode::AbsoluteY),
       OpCode::new(0xC1, OpCodeType::CMP, AddressingMode::IndexedIndirect),
       OpCode::new(0xD1, OpCodeType::CMP, AddressingMode::IndirectIndexed),

       OpCode::new(0xE0, OpCodeType::CPX, AddressingMode::Immediate),
       OpCode::new(0xE4, OpCodeType::CPX, AddressingMode::ZeroPage),
       OpCode::new(0xEC, OpCodeType::CPX, AddressingMode::Absolute),

       OpCode::new(0xC0, OpCodeType::CPY, AddressingMode::Immediate),
       OpCode::new(0xC4, OpCodeType::CPY, AddressingMode::ZeroPage),
       OpCode::new(0xCC, OpCodeType::CPY, AddressingMode::Absolute),

       OpCode::new(0xC6, OpCodeType::DEC, AddressingMode::ZeroPage),
       OpCode::new(0xD6, OpCodeType::DEC, AddressingMode::ZeroPageX),
       OpCode::new(0xCE, OpCodeType::DEC, AddressingMode::Absolute),
       OpCode::new(0xDE, OpCodeType::DEC, AddressingMode::AbsoluteX),

       OpCode::new(0xCA, OpCodeType::DEX, AddressingMode::Implicit),
       OpCode::new(0x88, OpCodeType::DEY, AddressingMode::Implicit),

       OpCode::new(0x49, OpCodeType::EOR, AddressingMode::Immediate),
       OpCode::new(0x45, OpCodeType::EOR, AddressingMode::ZeroPage),
       OpCode::new(0x55, OpCodeType::EOR, AddressingMode::ZeroPageX),
       OpCode::new(0x4D, OpCodeType::EOR, AddressingMode::Absolute),
       OpCode::new(0x5D, OpCodeType::EOR, AddressingMode::AbsoluteX),
       OpCode::new(0x59, OpCodeType::EOR, AddressingMode::AbsoluteY),
       OpCode::new(0x41, OpCodeType::EOR, AddressingMode::IndexedIndirect),
       OpCode::new(0x51, OpCodeType::EOR, AddressingMode::IndirectIndexed),

       OpCode::new(0xE6, OpCodeType::INC, AddressingMode::ZeroPage),
       OpCode::new(0xF6, OpCodeType::INC, AddressingMode::ZeroPageX),
       OpCode::new(0xEE, OpCodeType::INC, AddressingMode::Absolute),
       OpCode::new(0xFE, OpCodeType::INC, AddressingMode::AbsoluteX),

       OpCode::new(0xE8, OpCodeType::INX, AddressingMode::Implicit),
       OpCode::new(0xC8, OpCodeType::INY, AddressingMode::Implicit),

       OpCode::new(0xE8, OpCodeType::JMP, AddressingMode::Absolute),
       OpCode::new(0xE8, OpCodeType::JMP, AddressingMode::Indirect),

       OpCode::new(0xE8, OpCodeType::JSR, AddressingMode::Absolute),

       OpCode::new(0xA9, OpCodeType::LDA, AddressingMode::Immediate),
       OpCode::new(0xA5, OpCodeType::LDA, AddressingMode::ZeroPage),
       OpCode::new(0xB5, OpCodeType::LDA, AddressingMode::ZeroPageX),
       OpCode::new(0xAD, OpCodeType::LDA, AddressingMode::Absolute),
       OpCode::new(0xBD, OpCodeType::LDA, AddressingMode::AbsoluteX),
       OpCode::new(0xB9, OpCodeType::LDA, AddressingMode::AbsoluteY),
       OpCode::new(0xA1, OpCodeType::LDA, AddressingMode::IndexedIndirect),
       OpCode::new(0xB9, OpCodeType::LDA, AddressingMode::IndirectIndexed),

       OpCode::new(0xA2, OpCodeType::LDX, AddressingMode::Immediate),
       OpCode::new(0xA6, OpCodeType::LDX, AddressingMode::ZeroPage),
       OpCode::new(0xB6, OpCodeType::LDX, AddressingMode::ZeroPageY),
       OpCode::new(0xAE, OpCodeType::LDX, AddressingMode::Absolute),
       OpCode::new(0xBE, OpCodeType::LDX, AddressingMode::AbsoluteY),

       OpCode::new(0xA0, OpCodeType::LDY, AddressingMode::Immediate),
       OpCode::new(0xA4, OpCodeType::LDY, AddressingMode::ZeroPage),
       OpCode::new(0xB4, OpCodeType::LDY, AddressingMode::ZeroPageX),
       OpCode::new(0xAC, OpCodeType::LDY, AddressingMode::Absolute),
       OpCode::new(0xBC, OpCodeType::LDY, AddressingMode::AbsoluteX),

       OpCode::new(0x4A, OpCodeType::LSR, AddressingMode::Accumulator),
       OpCode::new(0x46, OpCodeType::LSR, AddressingMode::ZeroPage),
       OpCode::new(0x56, OpCodeType::LSR, AddressingMode::ZeroPageX),
       OpCode::new(0x4E, OpCodeType::LSR, AddressingMode::Absolute),
       OpCode::new(0x5E, OpCodeType::LSR, AddressingMode::AbsoluteX),

       OpCode::new(0xEA, OpCodeType::NOP, AddressingMode::Implicit),

       OpCode::new(0x09, OpCodeType::ORA, AddressingMode::Immediate),
       OpCode::new(0x05, OpCodeType::ORA, AddressingMode::ZeroPage),
       OpCode::new(0x15, OpCodeType::ORA, AddressingMode::ZeroPageX),
       OpCode::new(0x0D, OpCodeType::ORA, AddressingMode::Absolute),
       OpCode::new(0x1D, OpCodeType::ORA, AddressingMode::AbsoluteX),
       OpCode::new(0x19, OpCodeType::ORA, AddressingMode::AbsoluteY),
       OpCode::new(0x01, OpCodeType::ORA, AddressingMode::IndexedIndirect),
       OpCode::new(0x11, OpCodeType::ORA, AddressingMode::IndirectIndexed),

       OpCode::new(0x48, OpCodeType::PHA, AddressingMode::Implicit),
       OpCode::new(0x08, OpCodeType::PHP, AddressingMode::Implicit),
       OpCode::new(0x68, OpCodeType::PLA, AddressingMode::Implicit),
       OpCode::new(0x28, OpCodeType::PLP, AddressingMode::Implicit),

       OpCode::new(0x2A, OpCodeType::ROL, AddressingMode::Accumulator),
       OpCode::new(0x26, OpCodeType::ROL, AddressingMode::ZeroPage),
       OpCode::new(0x36, OpCodeType::ROL, AddressingMode::ZeroPageX),
       OpCode::new(0x2E, OpCodeType::ROL, AddressingMode::Absolute),
       OpCode::new(0x3E, OpCodeType::ROL, AddressingMode::AbsoluteX),

       OpCode::new(0x6A, OpCodeType::ROR, AddressingMode::Accumulator),
       OpCode::new(0x66, OpCodeType::ROR, AddressingMode::ZeroPage),
       OpCode::new(0x76, OpCodeType::ROR, AddressingMode::ZeroPageX),
       OpCode::new(0x6E, OpCodeType::ROR, AddressingMode::Absolute),
       OpCode::new(0x7E, OpCodeType::ROR, AddressingMode::AbsoluteX),

       OpCode::new(0x40, OpCodeType::RTI, AddressingMode::Implicit),
       OpCode::new(0x60, OpCodeType::RTS, AddressingMode::Implicit),

       OpCode::new(0xE9, OpCodeType::SBC, AddressingMode::Immediate),
       OpCode::new(0xE5, OpCodeType::SBC, AddressingMode::ZeroPage),
       OpCode::new(0xF5, OpCodeType::SBC, AddressingMode::ZeroPageX),
       OpCode::new(0xED, OpCodeType::SBC, AddressingMode::Absolute),
       OpCode::new(0xFD, OpCodeType::SBC, AddressingMode::AbsoluteX),
       OpCode::new(0xF9, OpCodeType::SBC, AddressingMode::AbsoluteY),
       OpCode::new(0xE1, OpCodeType::SBC, AddressingMode::IndirectIndexed),
       OpCode::new(0xF1, OpCodeType::SBC, AddressingMode::IndexedIndirect),

       OpCode::new(0x38, OpCodeType::SEC, AddressingMode::Implicit),
       OpCode::new(0xF8, OpCodeType::SED, AddressingMode::Implicit),
       OpCode::new(0x78, OpCodeType::SEI, AddressingMode::Implicit),

       OpCode::new(0x85, OpCodeType::STA, AddressingMode::ZeroPage),
       OpCode::new(0x95, OpCodeType::STA, AddressingMode::ZeroPageX),
       OpCode::new(0x8D, OpCodeType::STA, AddressingMode::Absolute),
       OpCode::new(0x9D, OpCodeType::STA, AddressingMode::AbsoluteX),
       OpCode::new(0x99, OpCodeType::STA, AddressingMode::AbsoluteY),
       OpCode::new(0x81, OpCodeType::STA, AddressingMode::IndexedIndirect),
       OpCode::new(0x91, OpCodeType::STA, AddressingMode::IndirectIndexed),

       OpCode::new(0x86, OpCodeType::STX, AddressingMode::ZeroPage),
       OpCode::new(0x96, OpCodeType::STX, AddressingMode::ZeroPageY),
       OpCode::new(0x8E, OpCodeType::STX, AddressingMode::Absolute),

       OpCode::new(0x84, OpCodeType::STY, AddressingMode::ZeroPage),
       OpCode::new(0x94, OpCodeType::STY, AddressingMode::ZeroPageY),
       OpCode::new(0x8C, OpCodeType::STY, AddressingMode::Absolute),

       OpCode::new(0xAA, OpCodeType::TAX, AddressingMode::Implicit),
       OpCode::new(0xA8, OpCodeType::TAY, AddressingMode::Implicit),
       OpCode::new(0xBA, OpCodeType::TSX, AddressingMode::Implicit),
       OpCode::new(0x8A, OpCodeType::TXA, AddressingMode::Implicit),
       OpCode::new(0x9A, OpCodeType::TXS, AddressingMode::Implicit),
       OpCode::new(0x98, OpCodeType::TYA, AddressingMode::Implicit),
    ];
}
