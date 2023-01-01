pub struct Timer {
    // Regular timer
    pub enabled: bool,
    counter_clock: u16,
    pub counter: u8,
    pub ratio: u16,
    pub modulo: u8,
    pub interrupt_flag: bool,

    // Divider
    divider_clock: u16,
    pub divider: u8,
}

impl Timer {
    pub fn new() -> Self {
        Self {
            enabled: false,
            counter_clock: 0,
            counter: 0,
            ratio: 1024,
            modulo: 0,
            interrupt_flag: false,
            divider_clock: 0,
            divider: 0,
        }
    }

    pub fn step(&mut self, cycles: u8) {
        self.divider_clock += cycles as u16;
        while self.divider_clock >= 256 {
            self.divider = self.divider.wrapping_add(1);
            self.divider_clock -= 256;
        }

        if !self.enabled {
            return;
        }

        self.counter_clock += cycles as u16;

        while self.counter_clock >= self.ratio {
            if self.counter == 0xFF {
                self.interrupt_flag = true;
                self.counter = self.modulo;
            } else {
                self.counter = self.counter.wrapping_add(1);
            }

            self.counter_clock -= self.ratio;
        }
    }
}
