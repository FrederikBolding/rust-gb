#[derive(Debug)]
pub enum Instruction {
    NOP,
    HALT,
    STOP,
    EI,
    DI,

    LD,

    INC,
    DEC,

    ADD,
    ADC,
    ADDHL,
    ADDSP,
    SUB,
    SBC,

    AND,
    OR,
    XOR,
    CP,

    CCF,
    SCF,
    RRA,
    RLA,
    RRCA,
    RLCA,
    CPL,
    DAA,

    RET,
    PUSH,
    POP,

    JP,
    JR,
    JPI,
    CALL,
    RETI,
    RST,

    SET,
    RES,
    BIT,
    SRL,
    RR,
    RL,
    RRC,
    RLC,
    SRA,
    SLA,
    SWAP,
}

impl Instruction {
    pub fn decode(byte: u8) -> Instruction {
        match byte {
            0x00 => Instruction::NOP, // NOP
            0x01 => Instruction::LD, // LD BC, d16
            0x02 => Instruction::LD, // LD (BC), A
            0x03 => Instruction::INC, // INC BC
            0x10 => Instruction::STOP,
            _ => panic!("0x{:02x} cannot be decoded", byte),
        }
    }
}
