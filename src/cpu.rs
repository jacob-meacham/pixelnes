enum OpCode {
    LDA,
    BRK,
}

impl OpCode {
    fn from_byte(byte: u8) -> Option<OpCode> {
        match byte {
            0x00 => Some(OpCode::BRK),
            0xA9 => Some(OpCode::LDA),
            _ => None
        }
    }
}

struct Registers {
    a: u8,
    x: u8,
}

impl Registers {
    pub fn new() -> Self {
        Registers {
            a: 0,
            x: 0,
        }
    }
}

pub struct CPU {
    registers: Registers,
    status: u8,
    pc: u16,
    halt_requested: bool
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            registers: Registers::new(),
            status: 0,
            pc: 0,
            halt_requested: false
        }
    }

    pub fn run(&mut self, program: Vec<u8>) {
        self.pc = 0;

        loop {
            if self.halt_requested {
                break
            }
            self.step(&program);
        }
    }

    fn step(&mut self, program: &Vec<u8>) {
        let opcode = program[self.pc as usize];

        let next_pc = if let Some(opcode) = OpCode::from_byte(opcode) {
            self.execute(opcode, program)
        } else {
            panic!("Unknown instruction found for: 0x{:x}", opcode);
        };

        self.pc = next_pc;
    }

    fn execute(&mut self, opcode: OpCode, program: &Vec<u8>) -> u16 {
        return match opcode {
            OpCode::LDA => {
                let param = program[(self.pc + 1) as usize];
                self.registers.a = param;

                if self.registers.a == 0 {
                    self.status = self.status | 0b0000_0010;
                } else {
                    self.status = self.status & 0b1111_1101;
                }

                if self.registers.a & 0b1000_0000 != 0 {
                    self.status = self.status | 0b1000_0000;
                } else {
                    self.status = self.status & 0b0111_1111;
                }

                self.pc + 2
            }
            OpCode::BRK => {
                self.halt_requested = true;
                self.pc
            }
        }

    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.run(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.registers.a, 0x05);
        assert_eq!(cpu.status & 0b0000_0010, 0b00);
        assert_eq!(cpu.status & 0b1000_0000, 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.run(vec![0xa9, 0x00, 0x00]);
        assert_eq!(cpu.status & 0b0000_0010, 0b10);
    }
}