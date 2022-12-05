#[derive(Copy, Clone)]
pub enum RegisterTarget {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
}

#[derive(Copy, Clone)]
pub enum WordRegisterTarget {
    AF,
    BC,
    DE,
    HL,
}

pub struct Registers {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,

    // FLAGS (simulating the F register)
    pub zero: bool,
    // Also called operation
    pub sub: bool,
    pub half_carry: bool,
    pub carry: bool,
}

impl Registers {
    pub fn new() -> Self {
        Self {
            a: 0x0,
            b: 0x0,
            c: 0x0,
            d: 0x0,
            e: 0x0,
            h: 0x0,
            l: 0x0,
            zero: false,
            sub: false,
            half_carry: false,
            carry: false,
        }
    }

    pub fn get(&self, register: RegisterTarget) -> u8 {
        match register {
            RegisterTarget::A => self.a,
            RegisterTarget::B => self.b,
            RegisterTarget::C => self.c,
            RegisterTarget::D => self.d,
            RegisterTarget::E => self.e,
            RegisterTarget::F => {
                // Construct F u8 from flags via bitwise operations
                let mut f = 0x0u8;
                if self.zero {
                    f |= 0x80;
                }
                if self.sub {
                    f |= 0x40;
                }
                if self.half_carry {
                    f |= 0x20;
                }
                if self.carry {
                    f |= 0x10;
                }
                f
            }
            RegisterTarget::H => self.h,
            RegisterTarget::L => self.l,
        }
    }

    pub fn get_word(&self, register: WordRegisterTarget) -> u16 {
        match register {
            WordRegisterTarget::AF => {
                (self.get(RegisterTarget::A) as u16) << 8 | self.get(RegisterTarget::F) as u16
            }
            WordRegisterTarget::BC => {
                (self.get(RegisterTarget::B) as u16) << 8 | self.get(RegisterTarget::C) as u16
            }
            WordRegisterTarget::DE => {
                (self.get(RegisterTarget::D) as u16) << 8 | self.get(RegisterTarget::E) as u16
            }
            WordRegisterTarget::HL => {
                (self.get(RegisterTarget::H) as u16) << 8 | self.get(RegisterTarget::L) as u16
            }
        }
    }

    pub fn set(&mut self, register: RegisterTarget, value: u8) {
        match register {
            RegisterTarget::A => {
                self.a = value;
            }
            RegisterTarget::B => {
                self.b = value;
            }
            RegisterTarget::C => {
                self.c = value;
            }
            RegisterTarget::D => {
                self.d = value;
            }
            RegisterTarget::E => {
                self.e = value;
            }
            RegisterTarget::F => {
                // Set flag bools based on input value bits
                self.zero = value & 0x80 == 0x80;
                self.sub = value & 0x40 == 0x40;
                self.half_carry = value & 0x20 == 0x20;
                self.carry = value & 0x10 == 0x10;
            }
            RegisterTarget::H => {
                self.h = value;
            }
            RegisterTarget::L => {
                self.l = value;
            }
        }
    }

    pub fn set_word(&mut self, register: WordRegisterTarget, value: u16) {
        match register {
            WordRegisterTarget::AF => {
                self.set(RegisterTarget::A, (value >> 8) as u8);
                self.set(RegisterTarget::F, (value) as u8)
            }
            WordRegisterTarget::BC => {
                self.set(RegisterTarget::B, (value >> 8) as u8);
                self.set(RegisterTarget::C, (value) as u8)
            }
            WordRegisterTarget::DE => {
                self.set(RegisterTarget::D, (value >> 8) as u8);
                self.set(RegisterTarget::E, (value) as u8)
            }
            WordRegisterTarget::HL => {
                self.set(RegisterTarget::H, (value >> 8) as u8);
                self.set(RegisterTarget::L, (value) as u8)
            }
        }
    }
}
