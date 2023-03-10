# <img width="32" src="./logo/adelie-logo.svg?sanitize=true"> The Adelie Programming Language

[![Windows](https://github.com/kumavale/Adelie/actions/workflows/windows.yml/badge.svg)](https://github.com/kumavale/Adelie/actions/workflows/windows.yml)
[![Linux](https://github.com/kumavale/Adelie/actions/workflows/linux.yml/badge.svg)](https://github.com/kumavale/Adelie/actions/workflows/linux.yml)
[![license](https://img.shields.io/badge/license-MIT-blue.svg?style=flat)](LICENSE)

Adelie is a .Net-compatible statically typed language that aims to combine the benefits of Rust and C#.

## Goals

- A language that takes advantage of Rust and C#
- GUI application development using designer

## Getting started

Please see [Docs](https://kumavale.github.io/Adelie-book/)

## Sample

### FizzBuzz

```rust
fn main() {
    let mut i: i32 = 1;

    while i <= 100 {
        if i % 15 == 0 {
            println!("FizzBuzz");
        } else if i % 3 == 0 {
            println!("Fizz");
        } else if i % 5 == 0 {
            println!("Buzz");
        } else {
            println!(i);
        }
        i += 1;
    }
}
```

### MessageBox

```rust
#[link(name="System.Windows.Forms.dll", publickeytoken="B7 7A 5C 56 19 34 E0 89")]
extern {
    mod System {
        mod Windows {
            mod Forms {
                enum DialogResult {}
                struct MessageBox {}
                impl MessageBox {
                    fn Show(text: string) -> DialogResult {}
                }
            }
        }
    }
}
fn main() {
    System::Windows::Forms::MessageBox::Show("hello!");
}
```

## LICENSE

MIT
