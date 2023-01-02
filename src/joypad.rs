use minifb::Key;

pub struct Joypad {
    pub up: bool,
    pub down: bool,
    pub left: bool,
    pub right: bool,
    pub a: bool,
    pub b: bool,
    pub start: bool,
    pub select: bool,
    // Column 0 or 1 determining which bits to expose
    pub column: bool,
    pub interrupt_flag: bool,
}

impl Joypad {
    pub fn new() -> Self {
        Self {
            up: false,
            down: false,
            left: false,
            right: false,
            a: false,
            b: false,
            start: false,
            select: false,
            column: false,
            interrupt_flag: false,
        }
    }

    pub fn on_key_down(&mut self, key: Key) {
        match key {
            Key::Up => self.up = true,
            Key::Down => self.down = true,
            Key::Left => self.left = true,
            Key::Right => self.right = true,
            Key::Z => self.a = true,
            Key::X => self.b = true,
            Key::Enter => self.start = true,
            Key::Backspace => self.select = true,
            _ => {
                return;
            }
        }
        //println!("Key down {:?}", key);
        self.interrupt_flag = true;
    }

    pub fn on_key_up(&mut self, key: Key) {
        match key {
            Key::Up => self.up = false,
            Key::Down => self.down = false,
            Key::Left => self.left = false,
            Key::Right => self.right = false,
            Key::Z => self.a = false,
            Key::X => self.b = false,
            Key::Enter => self.start = false,
            Key::Backspace => self.select = false,
            _ => {
                return;
            }
        }
        //println!("Key up {:?}", key);
    }

    pub fn to_byte(&self) -> u8 {
        let column_bit = if !self.column { 1 << 5 } else { 1 << 4 };

        let bits = match self.column {
            true => {
                (if self.a { 0x00 } else { 0x01 }
                    | if self.b { 0x00 } else { 0x02 }
                    | if self.select { 0x00 } else { 0x04 }
                    | if self.start { 0x00 } else { 0x08 })
            }
            false => {
                (if self.right { 0x00 } else { 0x01 }
                    | if self.left { 0x00 } else { 0x02 }
                    | if self.up { 0x00 } else { 0x04 }
                    | if self.down { 0x00 } else { 0x08 })
            }
        };
        column_bit | bits
    }
}
