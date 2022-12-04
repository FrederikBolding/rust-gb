use crate::instruction::{is_word_target, Instruction, InstructionTarget, LoadType};
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
            stack_pointer: 0x0,
            registers: Registers::new(),
            mmu,
        }
    }

    fn read_current_byte(&self) -> u8 {
        self.mmu.read(self.program_counter)
    }

    fn read_next_byte(&self) -> u8 {
        self.mmu.read(self.program_counter + 1)
    }

    fn read_next_word(&self) -> u16 {
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
        println!("Byte: 0x{:02x}", instruction_byte);
        let instruction = Instruction::from_byte(instruction_byte, is_prefixed).unwrap();
        println!("Instruction {:?}", instruction);
        self.execute(instruction);
        // TODO: Change this to be dependant on instruction executed
        if is_prefixed {
            self.program_counter += 2;
        } else {
            self.program_counter += 1;
        }
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

    fn execute(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::NOP => {
                // no-op
            }
            Instruction::ADD(target) => {
                let value1 = self.get_instruction_target_byte(target);
                let value2 = self.registers.get(RegisterTarget::A);
                let value = value1.wrapping_add(value2);
                self.registers.set(RegisterTarget::A, value);
            }
            Instruction::SUB(target) => {
                let value1 = self.get_instruction_target_byte(target);
                let value2 = self.registers.get(RegisterTarget::A);
                let value = value1.wrapping_sub(value2);
                self.registers.set(RegisterTarget::A, value);
            }
            Instruction::INC(target) => {
                if is_word_target(target) {
                    let current_value = self.get_instruction_target_word(target);
                    let value = current_value.wrapping_add(1);
                    self.set_instruction_target_word(target, value);
                } else {
                    let current_value = self.get_instruction_target_byte(target);
                    let value = current_value.wrapping_add(1);
                    self.set_instruction_target_byte(target, value);
                }
            }
            Instruction::DEC(target) => {
                if is_word_target(target) {
                    let current_value = self.get_instruction_target_word(target);
                    let value = current_value.wrapping_sub(1);
                    self.set_instruction_target_word(target, value);
                } else {
                    let current_value = self.get_instruction_target_byte(target);
                    let value = current_value.wrapping_sub(1);
                    self.set_instruction_target_byte(target, value);
                }
            }
            Instruction::LD(load_type) => match load_type {
                LoadType::Byte(target, source) => {
                    let source_value = self.get_instruction_target_byte(source);
                    self.set_instruction_target_byte(target, source_value);
                }
                LoadType::Word(target) => {
                    let source_value = self.read_next_word();
                    self.set_instruction_target_word(target, source_value);
                }
                LoadType::SPFromHL => {
                    self.stack_pointer = self.get_instruction_target_word(InstructionTarget::HL);
                }
                _ => {
                    panic!("Failed to load {:?}", load_type)
                }
            },
            Instruction::OR(target) => {
                // TODO: Flags
                let value = self.get_instruction_target_byte(target);
                let a = self.get_instruction_target_byte(InstructionTarget::A);
                let result = value | a;
                self.set_instruction_target_byte(InstructionTarget::A, result);
            }
            Instruction::AND(target) => {
                // TODO: Flags
                let value = self.get_instruction_target_byte(target);
                let a = self.get_instruction_target_byte(InstructionTarget::A);
                let result = value & a;
                self.set_instruction_target_byte(InstructionTarget::A, result);
            }
            Instruction::XOR(target) => {
                // TODO: Flags
                let value = self.get_instruction_target_byte(target);
                let a = self.get_instruction_target_byte(InstructionTarget::A);
                let result = value ^ a;
                self.set_instruction_target_byte(InstructionTarget::A, result);
            }
            Instruction::SET(target, bit_position) => {
                let value = self.get_instruction_target_byte(target);
                // Use bitmask to set specific bit
                let result = value | (1u8 << (bit_position as u8));
                self.set_instruction_target_byte(target, result);
            }
            Instruction::RES(target, bit_position) => {
                let value = self.get_instruction_target_byte(target);
                // Use bitmask to reset specific bit
                let result = value & !(1u8 << (bit_position as u8));
                self.set_instruction_target_byte(target, result);
            }
            Instruction::PUSH(target) => {
                let value = self.get_instruction_target_word(target);
                self.push(value);
            }
            Instruction::POP(target) => {
                let value = self.pop();
                self.set_instruction_target_word(target, value);
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