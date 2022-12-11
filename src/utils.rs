pub trait Digits {
    fn digits(s: usize) -> usize {
        match s {
            0..=9 => 1,
            10..=99 => 2,
            100..=999 => 3,
            1000..=9999 => 4,
            10000..=99999 => 5,
            100000..=999999 => 6,
            1000000..=9999999 => 7,
            10000000..=99999999 => 8,
            100000000..=999999999 => 9,
            _ => 10,
        }
    }
}

impl Digits for usize {}

pub fn remove_seq(name: &str) -> String {
    name.split(':').next().unwrap().to_string()
}

#[macro_export]
macro_rules! seq {
    () => {
        unsafe {
            static mut ID: usize = 0;
            ID += 1;
            ID
        }
    };
}
