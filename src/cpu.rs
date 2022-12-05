use crate::instruction::{
    is_word_target, BitPosition, Indirect, Instruction, InstructionTarget, JumpTest, LoadType,
};
use crate::mmu::MMU;
use crate::registers::{RegisterTarget, Registers, WordRegisterTarget};

pub struct CPU {
    program_counter: u16,
    stack_pointer: u16,

    // Registers
    pub registers: Registers,

    // MMU
    pub mmu: MMU,
}

impl CPU {
    pub fn new(mmu: MMU) -> Self {
        Self {
            program_counter: 0x0,
            stack_pointer: 0x00,
            registers: Registers::new(),
            mmu,
        }
    }

    fn read_current_byte(&mut self) -> u8 {
        self.mmu.read(self.program_counter)
    }

    fn read_next_byte(&mut self) -> u8 {
        self.mmu.read(self.program_counter + 1)
    }

    fn read_next_word(&mut self) -> u16 {
        // Treat next byte as least significant bits and next byte + 1 as most significant
        let msb = (self.mmu.read(self.program_counter + 2) as u16) << 8;
        let lsb = self.read_next_byte() as u16;
        msb | lsb
    }

    pub fn step(&mut self) {
        let byte = self.read_current_byte();
        let is_prefixed = byte == 0xCB;
        let instruction_byte = if is_prefixed {
            self.read_next_byte()
        } else {
            byte
        };
        //println!("Byte: 0x{:02x}", instruction_byte);
        let instruction = Instruction::from_byte(instruction_byte, is_prefixed).unwrap();
        println!("Instruction {:?}", instruction);
        let next_program_counter = self.execute(instruction);
        self.program_counter = next_program_counter;
    }

    fn get_instruction_target_byte(&mut self, target: InstructionTarget) -> u8 {
        match target {
            InstructionTarget::A => self.registers.get(RegisterTarget::A),
            InstructionTarget::B => self.registers.get(RegisterTarget::B),
            InstructionTarget::C => self.registers.get(RegisterTarget::C),
            InstructionTarget::D => self.registers.get(RegisterTarget::D),
            InstructionTarget::E => self.registers.get(RegisterTarget::E),
            InstructionTarget::H => self.registers.get(RegisterTarget::H),
            InstructionTarget::L => self.registers.get(RegisterTarget::L),
            InstructionTarget::D8 => self.read_next_byte(),
            InstructionTarget::HLI => self
                .mmu
                .read(self.registers.get_word(WordRegisterTarget::HL)),
            _ => todo!(),
        }
    }

    fn set_instruction_target_byte(&mut self, target: InstructionTarget, value: u8) {
        //println!("Set {:?} to  0x{:02x}", target, value);
        match target {
            InstructionTarget::A => self.registers.set(RegisterTarget::A, value),
            InstructionTarget::B => self.registers.set(RegisterTarget::B, value),
            InstructionTarget::C => self.registers.set(RegisterTarget::C, value),
            InstructionTarget::D => self.registers.set(RegisterTarget::D, value),
            InstructionTarget::E => self.registers.set(RegisterTarget::E, value),
            InstructionTarget::H => self.registers.set(RegisterTarget::H, value),
            InstructionTarget::L => self.registers.set(RegisterTarget::L, value),
            InstructionTarget::HLI => self
                .mmu
                .write(self.registers.get_word(WordRegisterTarget::HL), value),
            _ => todo!(),
        }
    }

    fn get_instruction_target_word(&mut self, target: InstructionTarget) -> u16 {
        match target {
            InstructionTarget::AF => self.registers.get_word(WordRegisterTarget::AF),
            InstructionTarget::BC => self.registers.get_word(WordRegisterTarget::BC),
            InstructionTarget::DE => self.registers.get_word(WordRegisterTarget::DE),
            InstructionTarget::HL => self.registers.get_word(WordRegisterTarget::HL),
            InstructionTarget::SP => self.stack_pointer,
            _ => todo!(),
        }
    }

    fn set_instruction_target_word(&mut self, target: InstructionTarget, value: u16) {
        //println!("Set {:?} to 0x{:02x}", target, value);
        match target {
            InstructionTarget::BC => self.registers.set_word(WordRegisterTarget::BC, value),
            InstructionTarget::DE => self.registers.set_word(WordRegisterTarget::DE, value),
            InstructionTarget::HL => self.registers.set_word(WordRegisterTarget::HL, value),
            InstructionTarget::SP => self.stack_pointer = value,
            _ => todo!(),
        }
    }

    fn push(&mut self, value: u16) {
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
        self.mmu.write(self.stack_pointer, (value >> 8) as u8);

        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
        self.mmu.write(self.stack_pointer, value as u8);
    }

    fn pop(&mut self) -> u16 {
        let lsb = self.mmu.read(self.stack_pointer) as u16;
        self.stack_pointer = self.stack_pointer.wrapping_add(1);

        let msb = (self.mmu.read(self.stack_pointer) as u16) << 8;
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        msb | lsb
    }

    fn execute(&mut self, instruction: Instruction) -> u16 {
        match instruction {
            Instruction::NOP => {
                // no-op
                self.program_counter.wrapping_add(1)
            }
            Instruction::ADD(target) => {
                let value1 = self.get_instruction_target_byte(target);
                let value2 = self.registers.get(RegisterTarget::A);
                let value = value1.wrapping_add(value2);
                self.registers.set(RegisterTarget::A, value);
                self.registers.zero = value == 0;
                self.registers.sub = false;
                // TODO: Half-carry
                match target {
                    InstructionTarget::D8 => self.program_counter.wrapping_add(2),
                    _ => self.program_counter.wrapping_add(1),
                }
            }
            Instruction::SUB(target) => {
                let value1 = self.get_instruction_target_byte(target);
                let value2 = self.registers.get(RegisterTarget::A);
                let value = value1.wrapping_sub(value2);
                self.registers.set(RegisterTarget::A, value);
                self.registers.zero = value == 0;
                self.registers.sub = true;
                // TODO: Half-carry
                match target {
                    InstructionTarget::D8 => self.program_counter.wrapping_add(2),
                    _ => self.program_counter.wrapping_add(1),
                }
            }
            Instruction::INC(target) => {
                if is_word_target(target) {
                    let current_value = self.get_instruction_target_word(target);
                    let value = current_value.wrapping_add(1);
                    self.set_instruction_target_word(target, value);
                    self.registers.zero = value == 0;
                } else {
                    let current_value = self.get_instruction_target_byte(target);
                    let value = current_value.wrapping_add(1);
                    self.set_instruction_target_byte(target, value);
                    self.registers.zero = value == 0;
                }
                self.registers.sub = false;
                // TODO: Half-carry
                self.program_counter.wrapping_add(1)
            }
            Instruction::DEC(target) => {
                if is_word_target(target) {
                    let current_value = self.get_instruction_target_word(target);
                    let value = current_value.wrapping_sub(1);
                    self.set_instruction_target_word(target, value);
                    self.registers.zero = value == 0;
                } else {
                    let current_value = self.get_instruction_target_byte(target);
                    let value = current_value.wrapping_sub(1);
                    self.set_instruction_target_byte(target, value);
                    self.registers.zero = value == 0;
                }
                self.registers.sub = true;
                // TODO: Half-carry
                self.program_counter.wrapping_add(1)
            }
            Instruction::LD(load_type) => match load_type {
                LoadType::Byte(target, source) => {
                    let source_value = self.get_instruction_target_byte(source);
                    self.set_instruction_target_byte(target, source_value);
                    match source {
                        InstructionTarget::D8 => self.program_counter.wrapping_add(2),
                        InstructionTarget::HLI => self.program_counter.wrapping_add(1),
                        _ => self.program_counter.wrapping_add(1),
                    }
                }
                LoadType::Word(target) => {
                    let source_value = self.read_next_word();
                    self.set_instruction_target_word(target, source_value);
                    self.program_counter.wrapping_add(3)
                }
                LoadType::AFromIndirect(source) => {
                    let address = match source {
                        Indirect::BCIndirect => {
                            self.get_instruction_target_word(InstructionTarget::BC)
                        }
                        Indirect::DEIndirect => {
                            self.get_instruction_target_word(InstructionTarget::DE)
                        }
                        Indirect::HLIndirectMinus => todo!(),
                        Indirect::HLIndirectPlus => todo!(),
                        Indirect::WordIndirect => self.read_next_word(),
                        Indirect::LastByteIndirect => {
                            let offset = self.get_instruction_target_byte(InstructionTarget::C);
                            0xFF00 + offset as u16
                        }
                    };
                    let source_value = self.mmu.read(address);
                    self.set_instruction_target_byte(InstructionTarget::A, source_value);
                    match source {
                        Indirect::WordIndirect => self.program_counter.wrapping_add(3),
                        _ => self.program_counter.wrapping_add(1),
                    }
                }
                LoadType::IndirectFromA(target) => {
                    let source_value = self.get_instruction_target_byte(InstructionTarget::A);
                    let address = match target {
                        Indirect::BCIndirect => {
                            self.get_instruction_target_word(InstructionTarget::BC)
                        }
                        Indirect::DEIndirect => {
                            self.get_instruction_target_word(InstructionTarget::DE)
                        }
                        Indirect::HLIndirectMinus => {
                            let value = self.get_instruction_target_word(InstructionTarget::HL);
                            self.set_instruction_target_word(
                                InstructionTarget::HL,
                                value.wrapping_sub(1),
                            );
                            value
                        }
                        Indirect::HLIndirectPlus => {
                            let value = self.get_instruction_target_word(InstructionTarget::HL);
                            self.set_instruction_target_word(
                                InstructionTarget::HL,
                                value.wrapping_add(1),
                            );
                            value
                        }
                        Indirect::WordIndirect => self.read_next_word(),
                        Indirect::LastByteIndirect => {
                            let offset = self.get_instruction_target_byte(InstructionTarget::C);
                            0xFF00 + offset as u16
                        }
                    };
                    self.mmu.write(address, source_value);
                    match target {
                        Indirect::WordIndirect => self.program_counter.wrapping_add(3),
                        _ => self.program_counter.wrapping_add(1),
                    }
                }
                LoadType::AFromByteAddress => {
                    let offset = self.read_next_byte();
                    let value = self.mmu.read(0xFF00 + offset as u16);
                    self.set_instruction_target_byte(InstructionTarget::A, value);
                    self.program_counter.wrapping_add(2)
                }
                LoadType::ByteAddressFromA => {
                    let offset = self.read_next_byte();
                    let value = self.get_instruction_target_byte(InstructionTarget::A);
                    self.mmu.write(0xFF00 + offset as u16, value);
                    self.program_counter.wrapping_add(2)
                }
                LoadType::SPFromHL => {
                    self.stack_pointer = self.get_instruction_target_word(InstructionTarget::HL);
                    self.program_counter.wrapping_add(1)
                }
                LoadType::IndirectFromSP => {
                    let address = self.read_next_word();
                    self.mmu.write(address, self.stack_pointer as u8);
                    self.mmu
                        .write(address.wrapping_add(1), (self.stack_pointer >> 8) as u8);
                    self.program_counter.wrapping_add(3)
                }
                _ => {
                    panic!("Failed to load {:?}", load_type)
                }
            },
            Instruction::OR(target) => {
                let value = self.get_instruction_target_byte(target);
                let a = self.get_instruction_target_byte(InstructionTarget::A);
                let result = value | a;
                self.set_instruction_target_byte(InstructionTarget::A, result);
                self.registers.zero = result == 0;
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = false;
                match target {
                    InstructionTarget::D8 => self.program_counter.wrapping_add(2),
                    _ => self.program_counter.wrapping_add(1),
                }
            }
            Instruction::AND(target) => {
                let value = self.get_instruction_target_byte(target);
                let a = self.get_instruction_target_byte(InstructionTarget::A);
                let result = value & a;
                self.set_instruction_target_byte(InstructionTarget::A, result);
                self.registers.zero = result == 0;
                self.registers.sub = false;
                self.registers.half_carry = true;
                self.registers.carry = false;
                match target {
                    InstructionTarget::D8 => self.program_counter.wrapping_add(2),
                    _ => self.program_counter.wrapping_add(1),
                }
            }
            Instruction::XOR(target) => {
                let value = self.get_instruction_target_byte(target);
                let a = self.get_instruction_target_byte(InstructionTarget::A);
                let result = value ^ a;
                self.set_instruction_target_byte(InstructionTarget::A, result);
                self.registers.zero = result == 0;
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = false;
                match target {
                    InstructionTarget::D8 => self.program_counter.wrapping_add(2),
                    _ => self.program_counter.wrapping_add(1),
                }
            }
            Instruction::SET(target, bit_position) => {
                let value = self.get_instruction_target_byte(target);
                // Use bitmask to set specific bit
                let result = value | (1u8 << (bit_position as u8));
                self.set_instruction_target_byte(target, result);
                self.program_counter.wrapping_add(2)
            }
            Instruction::RES(target, bit_position) => {
                let value = self.get_instruction_target_byte(target);
                // Use bitmask to reset specific bit
                let result = value & !(1u8 << (bit_position as u8));
                self.set_instruction_target_byte(target, result);
                self.program_counter.wrapping_add(2)
            }
            Instruction::BIT(target, bit_position) => {
                let value = self.get_instruction_target_byte(target);
                let result = (value >> (bit_position as u8)) & 0b1;
                self.registers.zero = result == 0;
                self.registers.sub = false;
                self.registers.half_carry = true;
                self.program_counter.wrapping_add(2)
            }
            Instruction::PUSH(target) => {
                let value = self.get_instruction_target_word(target);
                self.push(value);
                self.program_counter.wrapping_add(1)
            }
            Instruction::POP(target) => {
                let value = self.pop();
                self.set_instruction_target_word(target, value);
                self.program_counter.wrapping_add(1)
            }
            Instruction::RST(location) => {
                self.push(self.program_counter);
                location.to_hex()
            }
            Instruction::JP(test) => {
                let jump_condition = match test {
                    JumpTest::NotZero => !self.registers.zero,
                    JumpTest::NotCarry => !self.registers.carry,
                    JumpTest::Zero => self.registers.zero,
                    JumpTest::Carry => self.registers.carry,
                    JumpTest::Always => true,
                };
                if jump_condition {
                    self.read_next_word()
                } else {
                    self.program_counter.wrapping_add(3)
                }
            }
            Instruction::JR(test) => {
                let jump_condition = match test {
                    JumpTest::NotZero => !self.registers.zero,
                    JumpTest::NotCarry => !self.registers.carry,
                    JumpTest::Zero => self.registers.zero,
                    JumpTest::Carry => self.registers.carry,
                    JumpTest::Always => true,
                };
                let next_program_counter = self.program_counter.wrapping_add(2);
                if jump_condition {
                    // Handle signed offsets
                    let offset = self.read_next_byte() as i8;
                    return if offset >= 0 {
                        next_program_counter.wrapping_add(offset as u16)
                    } else {
                        next_program_counter.wrapping_sub(offset.abs() as u16)
                    };
                } else {
                    next_program_counter
                }
            }
            Instruction::CP(target) => {
                let a_value = self.get_instruction_target_byte(InstructionTarget::A);
                let target_value = self.get_instruction_target_byte(target);

                self.registers.zero = a_value == target_value;
                self.registers.sub = true;
                // TODO: Half-carry
                self.registers.carry = a_value < target_value;

                match target {
                    InstructionTarget::D8 => self.program_counter.wrapping_add(2),
                    _ => self.program_counter.wrapping_add(1),
                }
            }
            Instruction::CALL(test) => {
                let condition = match test {
                    JumpTest::NotZero => !self.registers.zero,
                    JumpTest::NotCarry => !self.registers.carry,
                    JumpTest::Zero => self.registers.zero,
                    JumpTest::Carry => self.registers.carry,
                    JumpTest::Always => true,
                };
                let next_program_counter = self.program_counter.wrapping_add(3);
                if condition {
                    self.push(next_program_counter);
                    self.read_next_word()
                } else {
                    next_program_counter
                }
            }
            Instruction::RET(test) => {
                let condition = match test {
                    JumpTest::NotZero => !self.registers.zero,
                    JumpTest::NotCarry => !self.registers.carry,
                    JumpTest::Zero => self.registers.zero,
                    JumpTest::Carry => self.registers.carry,
                    JumpTest::Always => true,
                };
                if condition {
                    self.pop()
                } else {
                    self.program_counter.wrapping_add(1)
                }
            }
            Instruction::RL(target) => {
                let value = self.get_instruction_target_byte(target);
                let result = (value << 1) | self.registers.carry as u8;
                self.set_instruction_target_byte(target, result);
                self.registers.zero = result == 0;
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = (value & 0x80) == 0x80;
                self.program_counter.wrapping_add(2)
            }
            Instruction::RLA => {
                let value = self.get_instruction_target_byte(InstructionTarget::A);
                let result = (value << 1) | self.registers.carry as u8;
                self.set_instruction_target_byte(InstructionTarget::A, result);
                self.registers.zero = result == 0;
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = (value & 0x80) == 0x80;
                self.program_counter.wrapping_add(1)
            }
            Instruction::SWAP(target) => {
                let value = self.get_instruction_target_byte(target);
                let result = ((value) << 4) | (value >> 4);
                self.set_instruction_target_byte(target, result);
                self.registers.zero = result == 0;
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = false;
                self.program_counter.wrapping_add(1)
            }
            Instruction::SCF => {
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = true;
                self.program_counter.wrapping_add(1)
            }
            _ => {
                panic!("Failed to execute {:?}", instruction);
            }
        }
    }
}

#[test]
fn test_step() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.skip_boot();
    cpu.mmu.write(0, 0x23); // INC(HL)
    cpu.mmu.write(1, 0xB5); // OR(L)
    cpu.mmu.write(2, 0xCB); // PREFIX
    cpu.mmu.write(3, 0xe8); // SET(B, 5)
    for _ in 0..=3 {
        cpu.step();
    }

    assert_eq!(cpu.registers.h, 0b0);
    assert_eq!(cpu.registers.l, 0b1);
    assert_eq!(cpu.registers.a, 0b1);
    assert_eq!(cpu.registers.b, 0b0010_0000);
}

#[test]
fn test_add() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.skip_boot();
    cpu.mmu.write(0, 0x3C); //INC A
    cpu.mmu.write(1, 0x04); // INC B
    cpu.mmu.write(2, 0x80); // ADD A,B
    for _ in 0..=2 {
        cpu.step();
    }

    assert_eq!(cpu.registers.a, 2);
    assert_eq!(cpu.registers.b, 1);
}

#[test]
fn test_load_byte() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.skip_boot();
    cpu.mmu.write(0, 0x04); // INC B
    cpu.mmu.write(1, 0x50); // LD D, B
    for _ in 0..=1 {
        cpu.step();
    }

    assert_eq!(cpu.registers.d, 1);
    assert_eq!(cpu.registers.b, 1);
}

#[test]
fn test_push_pop() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.skip_boot();
    cpu.registers.b = 0x4;
    cpu.registers.c = 0x89;
    cpu.stack_pointer = 0x10;
    cpu.execute(Instruction::PUSH(InstructionTarget::BC));

    assert_eq!(cpu.mmu.read(0xF), 0x04);
    assert_eq!(cpu.mmu.read(0xE), 0x89);
    assert_eq!(cpu.stack_pointer, 0xE);

    cpu.execute(Instruction::POP(InstructionTarget::DE));

    assert_eq!(cpu.registers.d, 0x04);
    assert_eq!(cpu.registers.e, 0x89);
}

#[test]
fn test_jr() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.skip_boot();
    cpu.program_counter = 0xF8;
    cpu.mmu.write(0xF9, 0x4);
    let next_pc = cpu.execute(Instruction::JR(JumpTest::Always));

    assert_eq!(next_pc, 0xFE);

    cpu.mmu.write(0xF9, 0xFC); // == -4
    let next_pc = cpu.execute(Instruction::JR(JumpTest::Always));
    assert_eq!(next_pc, 0xF6);
}

#[test]
fn test_bit() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.skip_boot();
    cpu.registers.a = 0b1011_0100;

    cpu.execute(Instruction::BIT(InstructionTarget::A, BitPosition::B2));
    assert_eq!(cpu.registers.zero, false);
    assert_eq!(cpu.registers.sub, false);
    assert_eq!(cpu.registers.half_carry, true);
    assert_eq!(cpu.registers.carry, false);

    cpu.execute(Instruction::BIT(InstructionTarget::A, BitPosition::B1));
    assert_eq!(cpu.registers.zero, true);
    assert_eq!(cpu.registers.sub, false);
    assert_eq!(cpu.registers.half_carry, true);
    assert_eq!(cpu.registers.carry, false);
}

#[test]
fn test_swap() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.skip_boot();
    cpu.registers.a = 0b1011_0101;

    cpu.execute(Instruction::SWAP(InstructionTarget::A));

    assert_eq!(cpu.registers.a, 0b0101_1011);
    assert_eq!(cpu.registers.zero, false);
    assert_eq!(cpu.registers.sub, false);
    assert_eq!(cpu.registers.half_carry, false);
    assert_eq!(cpu.registers.carry, false);
}