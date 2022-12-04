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

    fn read_current_byte(&mut self) -> u8 {
        self.mmu.read(self.program_counter)
    }

    fn read_next_byte(&mut self) -> u8 {
        self.mmu.read(self.program_counter + 1)
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
            _ => todo!(),
        }
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
fn test_step_2() {
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
fn test_step_3() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.write(0, 0x04); //INC B
    cpu.mmu.write(1, 0x50); // LD D, B
    for _ in 0..=1 {
        cpu.step();
    }

    assert_eq!(cpu.registers.d, 1);
    assert_eq!(cpu.registers.b, 1);
}
