use crate::instruction::{
    is_word_target, BitPosition, Indirect, Instruction, InstructionTarget, JumpTest, LoadType,
};
use crate::mmu::MMU;
use crate::registers::{RegisterTarget, Registers, WordRegisterTarget};

pub struct CPU {
    program_counter: u16,
    stack_pointer: u16,
    interrupts_enabled: bool,
    halted: bool,

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
            interrupts_enabled: false,
            halted: false,
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

    pub fn step(&mut self) -> u8 {
        let byte = self.read_current_byte();
        let is_prefixed = byte == 0xCB;
        let instruction_byte = if is_prefixed {
            self.read_next_byte()
        } else {
            byte
        };
        //println!("{:?} Byte: 0x{:02x} ({:?})", self.program_counter, instruction_byte, is_prefixed);
        let instruction = Instruction::from_byte(instruction_byte, is_prefixed).unwrap();
        //println!("Instruction {:?}", instruction);
        let (next_program_counter, cycles) = self.execute(instruction);
        self.mmu.step(cycles);

        if !self.halted {
            self.program_counter = next_program_counter;
        }

        if self.interrupts_enabled {
            // TODO: Run interrupt
            if self.mmu.vblank_interrupt_enabled && self.mmu.gpu.vblank_interrupt_flag {
                self.mmu.gpu.vblank_interrupt_flag = false;
                return cycles + self.interrupt(0x40);
            }

            if self.mmu.lcdstat_interrupt_enabled && self.mmu.gpu.lcdstat_interrupt_flag {
                self.mmu.gpu.lcdstat_interrupt_flag = false;
                return cycles + self.interrupt(0x48);
            }
        }

        cycles
    }

    fn interrupt(&mut self, address: u16) -> u8 {
        self.interrupts_enabled = false;
        self.push(self.program_counter);
        self.program_counter = address;
        self.mmu.step(12);
        12
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
            InstructionTarget::AF => self.registers.set_word(WordRegisterTarget::AF, value),
            InstructionTarget::BC => self.registers.set_word(WordRegisterTarget::BC, value),
            InstructionTarget::DE => self.registers.set_word(WordRegisterTarget::DE, value),
            InstructionTarget::HL => self.registers.set_word(WordRegisterTarget::HL, value),
            InstructionTarget::SP => self.stack_pointer = value,
            _ => todo!("Not implemented {:?}", target),
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

    fn execute(&mut self, instruction: Instruction) -> (u16, u8) {
        match instruction {
            Instruction::NOP => {
                // no-op
                (self.program_counter.wrapping_add(1), 1)
            }
            Instruction::HALT => {
                self.halted = true;
                (self.program_counter.wrapping_add(1), 4)
            }
            Instruction::ADD(target) => {
                let value1 = self.get_instruction_target_byte(target) as u32;
                let value2 = self.registers.get(RegisterTarget::A) as u32;
                let value = value1.wrapping_add(value2);
                self.registers.set(RegisterTarget::A, value as u8);
                self.registers.zero = value == 0;
                self.registers.sub = false;
                self.registers.carry = (value & 0x100) == 0x100;
                // TODO: Half-carry
                match target {
                    InstructionTarget::D8 => (self.program_counter.wrapping_add(2), 8),
                    InstructionTarget::HLI => (self.program_counter.wrapping_add(1), 8),
                    _ => (self.program_counter.wrapping_add(1), 4),
                }
            }
            Instruction::ADDHL(target) => {
                let value1 = self.get_instruction_target_word(target) as u32;
                let value2 = self.get_instruction_target_word(InstructionTarget::HL) as u32;
                let value = value1.wrapping_add(value2);
                self.registers.set(RegisterTarget::A, value as u8);
                self.registers.zero = value == 0;
                self.registers.sub = false;
                self.registers.carry = (value & 0x10000) == 0x10000;
                // TODO: Half-carry
                self.set_instruction_target_word(InstructionTarget::HL, value as u16);
                (self.program_counter.wrapping_add(1), 8)
            }
            Instruction::ADDSP => {
                let stack_pointer = self.get_instruction_target_word(InstructionTarget::SP) as i32;
                let value = self.get_instruction_target_byte(InstructionTarget::D8) as i8 as i32;
                let result = stack_pointer.wrapping_add(value);
                self.set_instruction_target_word(InstructionTarget::SP, result as u16);
                self.registers.zero = false;
                self.registers.sub = false;
                // TODO: Carry
                // TODO: Half-carry
                (self.program_counter.wrapping_add(2), 16)
            }
            Instruction::ADC(target) => {
                let value1 = self.get_instruction_target_byte(target) as u32;
                let value2 = self.registers.get(RegisterTarget::A) as u32;
                let value = value1
                    .wrapping_add(value2)
                    .wrapping_add(self.registers.carry as u32);
                self.registers.set(RegisterTarget::A, value as u8);
                self.registers.zero = value == 0;
                self.registers.sub = false;
                self.registers.carry = (value & 0x100) == 0x100;
                // TODO: Half-carry
                match target {
                    InstructionTarget::D8 => (self.program_counter.wrapping_add(2), 8),
                    InstructionTarget::HLI => (self.program_counter.wrapping_add(1), 8),
                    _ => (self.program_counter.wrapping_add(1), 4),
                }
            }
            Instruction::SUB(target) => {
                let value1 = self.get_instruction_target_byte(target) as u32;
                let value2 = self.registers.get(RegisterTarget::A) as u32;
                let value = value1.wrapping_sub(value2);
                self.registers.set(RegisterTarget::A, value as u8);
                self.registers.zero = value == 0;
                self.registers.sub = true;
                self.registers.carry = (value & 0x100) == 0x100;
                // TODO: Half-carry
                match target {
                    InstructionTarget::D8 => (self.program_counter.wrapping_add(2), 8),
                    InstructionTarget::HLI => (self.program_counter.wrapping_add(1), 8),
                    _ => (self.program_counter.wrapping_add(1), 4),
                }
            }
            Instruction::SBC(target) => {
                let value1 = self.get_instruction_target_byte(target) as u32;
                let value2 = self.registers.get(RegisterTarget::A) as u32;
                let value = value1
                    .wrapping_sub(value2)
                    .wrapping_sub(self.registers.carry as u32);
                self.registers.set(RegisterTarget::A, value as u8);
                self.registers.zero = value == 0;
                self.registers.sub = true;
                self.registers.carry = (value & 0x100) == 0x100;
                // TODO: Half-carry
                match target {
                    InstructionTarget::D8 => (self.program_counter.wrapping_add(2), 8),
                    InstructionTarget::HLI => (self.program_counter.wrapping_add(1), 8),
                    _ => (self.program_counter.wrapping_add(1), 4),
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
                let cycles = match target {
                    InstructionTarget::BC
                    | InstructionTarget::DE
                    | InstructionTarget::HL
                    | InstructionTarget::SP => 8,
                    InstructionTarget::HLI => 12,
                    _ => 4,
                };
                (self.program_counter.wrapping_add(1), cycles)
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
                let cycles = match target {
                    InstructionTarget::BC
                    | InstructionTarget::DE
                    | InstructionTarget::HL
                    | InstructionTarget::SP => 8,
                    InstructionTarget::HLI => 12,
                    _ => 4,
                };
                (self.program_counter.wrapping_add(1), cycles)
            }
            Instruction::LD(load_type) => match load_type {
                LoadType::Byte(target, source) => {
                    let source_value = self.get_instruction_target_byte(source);
                    self.set_instruction_target_byte(target, source_value);
                    match source {
                        InstructionTarget::D8 => (self.program_counter.wrapping_add(2), 8),
                        InstructionTarget::HLI => (self.program_counter.wrapping_add(1), 8),
                        _ => (self.program_counter.wrapping_add(1), 4),
                    }
                }
                LoadType::Word(target) => {
                    let source_value = self.read_next_word();
                    self.set_instruction_target_word(target, source_value);
                    (self.program_counter.wrapping_add(3), 12)
                }
                LoadType::AFromIndirect(source) => {
                    let address = match source {
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
                    let source_value = self.mmu.read(address);
                    self.set_instruction_target_byte(InstructionTarget::A, source_value);
                    match source {
                        Indirect::WordIndirect => (self.program_counter.wrapping_add(3), 16),
                        _ => (self.program_counter.wrapping_add(1), 8),
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
                        Indirect::WordIndirect => (self.program_counter.wrapping_add(3), 16),
                        _ => (self.program_counter.wrapping_add(1), 8),
                    }
                }
                LoadType::AFromByteAddress => {
                    let offset = self.read_next_byte();
                    let value = self.mmu.read(0xFF00 + offset as u16);
                    self.set_instruction_target_byte(InstructionTarget::A, value);
                    (self.program_counter.wrapping_add(2), 12)
                }
                LoadType::ByteAddressFromA => {
                    let offset = self.read_next_byte();
                    let value = self.get_instruction_target_byte(InstructionTarget::A);
                    self.mmu.write(0xFF00 + offset as u16, value);
                    (self.program_counter.wrapping_add(2), 12)
                }
                LoadType::SPFromHL => {
                    self.stack_pointer = self.get_instruction_target_word(InstructionTarget::HL);
                    (self.program_counter.wrapping_add(1), 8)
                }
                LoadType::IndirectFromSP => {
                    let address = self.read_next_word();
                    self.mmu.write(address, self.stack_pointer as u8);
                    self.mmu
                        .write(address.wrapping_add(1), (self.stack_pointer >> 8) as u8);
                    (self.program_counter.wrapping_add(3), 20)
                }
                LoadType::HLFromSPN => {
                    let stack_pointer = self.get_instruction_target_word(InstructionTarget::SP);
                    let value = self.get_instruction_target_byte(InstructionTarget::D8) as i8 as i16 as u16;
                    let result = stack_pointer.wrapping_add(value);
                    self.set_instruction_target_word(InstructionTarget::HL, result);
                    self.registers.sub = false;
                    self.registers.zero = false;
                    // TODO: Carry
                    // TODO: Half-carry
                    (self.program_counter.wrapping_add(2), 12)
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
                    InstructionTarget::D8 => (self.program_counter.wrapping_add(2), 8),
                    InstructionTarget::HLI => (self.program_counter.wrapping_add(1), 8),
                    _ => (self.program_counter.wrapping_add(1), 4),
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
                    InstructionTarget::D8 => (self.program_counter.wrapping_add(2), 8),
                    InstructionTarget::HLI => (self.program_counter.wrapping_add(1), 8),
                    _ => (self.program_counter.wrapping_add(1), 4),
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
                    InstructionTarget::D8 => (self.program_counter.wrapping_add(2), 8),
                    InstructionTarget::HLI => (self.program_counter.wrapping_add(1), 8),
                    _ => (self.program_counter.wrapping_add(1), 4),
                }
            }
            Instruction::SET(target, bit_position) => {
                let value = self.get_instruction_target_byte(target);
                // Use bitmask to set specific bit
                let result = value | (1u8 << (bit_position as u8));
                self.set_instruction_target_byte(target, result);
                let cycles = match target {
                    InstructionTarget::HLI => 16,
                    _ => 8,
                };
                (self.program_counter.wrapping_add(2), cycles)
            }
            Instruction::RES(target, bit_position) => {
                let value = self.get_instruction_target_byte(target);
                // Use bitmask to reset specific bit
                let result = value & !(1u8 << (bit_position as u8));
                self.set_instruction_target_byte(target, result);
                self.set_instruction_target_byte(target, result);
                let cycles = match target {
                    InstructionTarget::HLI => 16,
                    _ => 8,
                };
                (self.program_counter.wrapping_add(2), cycles)
            }
            Instruction::BIT(target, bit_position) => {
                let value = self.get_instruction_target_byte(target);
                let result = (value >> (bit_position as u8)) & 0b1;
                self.registers.zero = result == 0;
                self.registers.sub = false;
                self.registers.half_carry = true;
                self.set_instruction_target_byte(target, result);
                let cycles = match target {
                    InstructionTarget::HLI => 16,
                    _ => 8,
                };
                (self.program_counter.wrapping_add(2), cycles)
            }
            Instruction::PUSH(target) => {
                let value = self.get_instruction_target_word(target);
                self.push(value);
                (self.program_counter.wrapping_add(1), 16)
            }
            Instruction::POP(target) => {
                let value = self.pop();
                self.set_instruction_target_word(target, value);
                (self.program_counter.wrapping_add(1), 12)
            }
            Instruction::RST(location) => {
                self.push(self.program_counter);
                (location.to_hex(), 24)
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
                    (self.read_next_word(), 16)
                } else {
                    (self.program_counter.wrapping_add(3), 12)
                }
            }
            Instruction::JPI => (self.get_instruction_target_word(InstructionTarget::HL), 4),
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
                        (next_program_counter.wrapping_add(offset as u16), 16)
                    } else {
                        (next_program_counter.wrapping_sub(offset.abs() as u16), 16)
                    };
                } else {
                    (next_program_counter, 12)
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
                    InstructionTarget::D8 => (self.program_counter.wrapping_add(2), 8),
                    InstructionTarget::HLI => (self.program_counter.wrapping_add(1), 8),
                    _ => (self.program_counter.wrapping_add(1), 4),
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
                    (self.read_next_word(), 24)
                } else {
                    (next_program_counter, 12)
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
                let next_program_counter = if condition {
                    self.pop()
                } else {
                    self.program_counter.wrapping_add(1)
                };
                let cycles = if condition && test == JumpTest::Always {
                    16
                } else if condition {
                    20
                } else {
                    8
                };
                (next_program_counter, cycles)
            }
            Instruction::RETI => {
                self.interrupts_enabled = true;
                (self.pop(), 16)
            }
            Instruction::RL(target) => {
                let value = self.get_instruction_target_byte(target);
                let result = (value << 1) | self.registers.carry as u8;
                self.set_instruction_target_byte(target, result);
                self.registers.zero = result == 0;
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = (value & 0x80) == 0x80;
                let cycles = match target {
                    InstructionTarget::HLI => 16,
                    _ => 8,
                };
                (self.program_counter.wrapping_add(2), cycles)
            }
            Instruction::RLA => {
                let value = self.get_instruction_target_byte(InstructionTarget::A);
                let result = (value << 1) | self.registers.carry as u8;
                self.set_instruction_target_byte(InstructionTarget::A, result);
                self.registers.zero = result == 0;
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = (value & 0x80) == 0x80;
                (self.program_counter.wrapping_add(1), 4)
            }
            Instruction::SRA(target) => {
                let value = self.get_instruction_target_byte(target);
                let result = (value >> 1) | (value & 0x80);
                self.set_instruction_target_byte(target, result);
                self.registers.zero = result == 0;
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = (value & 0x1) == 0x1;
                let cycles = match target {
                    InstructionTarget::HLI => 16,
                    _ => 8,
                };
                (self.program_counter.wrapping_add(2), cycles)
            }
            Instruction::SLA(target) => {
                let value = self.get_instruction_target_byte(target);
                let result = value << 1;
                self.set_instruction_target_byte(target, result);
                self.registers.zero = result == 0;
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = (value & 0x80) == 0x80;
                let cycles = match target {
                    InstructionTarget::HLI => 16,
                    _ => 8,
                };
                (self.program_counter.wrapping_add(2), cycles)
            }
            Instruction::SRL(target) => {
                let value = self.get_instruction_target_byte(target);
                let result = value >> 1;
                self.set_instruction_target_byte(target, result);
                self.registers.zero = result == 0;
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = (value & 0x1) == 0x1;
                let cycles = match target {
                    InstructionTarget::HLI => 16,
                    _ => 8,
                };
                (self.program_counter.wrapping_add(2), cycles)
            }
            Instruction::RRA => {
                let value = self.get_instruction_target_byte(InstructionTarget::A);
                let result = (value >> 1) | ((self.registers.carry as u8) << 7);
                self.set_instruction_target_byte(InstructionTarget::A, result);
                self.registers.zero = false;
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = (value & 0x1) == 0x1;
                (self.program_counter.wrapping_add(1), 4)
            }
            Instruction::RR(target) => {
                let value = self.get_instruction_target_byte(target);
                let result = (value >> 1) | ((self.registers.carry as u8) << 7);
                self.set_instruction_target_byte(target, result);
                self.registers.zero = result == 0;
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = (value & 0x1) == 0x1;
                (self.program_counter.wrapping_add(1), 4)
            }
            Instruction::RLCA => {
                let value = self.get_instruction_target_byte(InstructionTarget::A);
                let carry = value >> 7;
                let result = (value << 1) | carry;
                self.set_instruction_target_byte(InstructionTarget::A, result);
                self.registers.zero = false;
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = carry == 1;
                (self.program_counter.wrapping_add(1), 4)
            }
            Instruction::SWAP(target) => {
                let value = self.get_instruction_target_byte(target);
                let result = ((value) << 4) | (value >> 4);
                self.set_instruction_target_byte(target, result);
                self.registers.zero = result == 0;
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = false;
                let cycles = match target {
                    InstructionTarget::HLI => 16,
                    _ => 8,
                };
                (self.program_counter.wrapping_add(2), cycles)
            }
            Instruction::SCF => {
                self.registers.sub = false;
                self.registers.half_carry = false;
                self.registers.carry = true;
                (self.program_counter.wrapping_add(1), 4)
            }
            Instruction::DI => {
                self.interrupts_enabled = false;
                (self.program_counter.wrapping_add(1), 4)
            }
            Instruction::EI => {
                self.interrupts_enabled = true;
                (self.program_counter.wrapping_add(1), 4)
            }
            Instruction::CPL => {
                let value = self.get_instruction_target_byte(InstructionTarget::A);
                self.set_instruction_target_byte(InstructionTarget::A, !value);
                self.registers.sub = true;
                self.registers.half_carry = true;
                (self.program_counter.wrapping_add(1), 4)
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
    cpu.mmu.finish_boot();
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
    cpu.mmu.finish_boot();
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
    cpu.mmu.finish_boot();
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
    cpu.mmu.finish_boot();
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
    cpu.mmu.finish_boot();
    cpu.program_counter = 0xF8;
    cpu.mmu.write(0xF9, 0x4);
    let (next_pc, _) = cpu.execute(Instruction::JR(JumpTest::Always));

    assert_eq!(next_pc, 0xFE);

    cpu.mmu.write(0xF9, 0xFC); // == -4
    let (next_pc, _) = cpu.execute(Instruction::JR(JumpTest::Always));
    assert_eq!(next_pc, 0xF6);
}

#[test]
fn test_bit() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.finish_boot();
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
    cpu.mmu.finish_boot();
    cpu.registers.a = 0b1011_0101;

    cpu.execute(Instruction::SWAP(InstructionTarget::A));

    assert_eq!(cpu.registers.a, 0b0101_1011);
    assert_eq!(cpu.registers.zero, false);
    assert_eq!(cpu.registers.sub, false);
    assert_eq!(cpu.registers.half_carry, false);
    assert_eq!(cpu.registers.carry, false);
}
