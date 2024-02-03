use lazy_static::lazy_static;
use crate::cpu::AddressingMode;

pub enum OpCodeType {
    BRK,
    LDA,
    LDX,
    LDY,
    TAX,
    TSX,
    TAY,
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
        OpCode::new(0x00, OpCodeType::BRK, 1, AddressingMode::None),
        OpCode::new(0xA9, OpCodeType::LDA, 2, AddressingMode::Immediate),
        OpCode::new(0xA2, OpCodeType::LDX, 2, AddressingMode::Immediate),
        OpCode::new(0xA0, OpCodeType::LDY, 2, AddressingMode::Immediate),
        OpCode::new(0xAA, OpCodeType::TAX, 1, AddressingMode::Immediate),
        OpCode::new(0xBA, OpCodeType::TSX, 1, AddressingMode::Immediate),
        OpCode::new(0xA8, OpCodeType::TAY, 1, AddressingMode::Immediate),
        OpCode::new(0x8A, OpCodeType::TXA, 1, AddressingMode::Immediate),
        OpCode::new(0x9A, OpCodeType::TXS, 1, AddressingMode::Immediate),
        OpCode::new(0x98, OpCodeType::TYA, 1, AddressingMode::Immediate),
    ];
}

