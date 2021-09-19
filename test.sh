#!/bin/bash

run() {
    input="$1"

    ./target/debug/adelie "$input" > tmp.il &&
    /mnt/c/Windows/Microsoft.NET/Framework/v4.0.30319/ilasm.exe /QUIET tmp.il &&
    ./tmp.exe
}

cargo build
if [ "$?" != "0" ]; then
    # build error
    exit 1
fi

echo

run '
fn one() -> i32 { return 1; 2; 3; }
fn two() -> i32 { 1; return 2; 3; }
fn three() -> i32 { 1; 2; return 3; }
fn ret3() -> i32 { return 3; }
fn plus(a: i32, b: i32) -> i32 { return a+b; }
fn add6(a:i32, b:i32, c:i32, d:i32, e:i32, f:i32) -> i32 { return a+b+c+d+e+f; }
fn fib(x: i32) -> i32 { if x<=1 { return 1; } else { return fib(x-1)+fib(x-2); } }
fn swap(x: &i32, y: &i32) { let tmp:i32=*x; *x=*y; *y=tmp; }
fn loop_return() -> i32 { let x: i32 = 3; loop { return x + 2; } }
fn ASSERT(expect: i32, actual: i32) -> i32 {
    let format: string = "[  {0}  ] expect: {1}, actual: {2}";
    if expect == actual {
        println(format, "OK", expect, actual);
        0
    } else {
        println(format, "NG", expect, actual);
        1
    }
}

fn main() {
    let ngcnt: i32 = 0;

    ngcnt += ASSERT( 0, 0);
    ngcnt += ASSERT(42, 42);
    ngcnt += ASSERT(21, 5+20-4);
    ngcnt += ASSERT(41, 12 + 34 - 5);
    ngcnt += ASSERT(47, 5+6*7);
    ngcnt += ASSERT(15, 5*(9-6));
    ngcnt += ASSERT( 4, (3+5)/2);
    ngcnt += ASSERT(10, -10+20);
    ngcnt += ASSERT(10, - -10);

    ngcnt += ASSERT(0, 0==1);
    ngcnt += ASSERT(1, 42==42);
    ngcnt += ASSERT(1, 0!=1);
    ngcnt += ASSERT(0, 42!=42);

    ngcnt += ASSERT(1, 0<1);
    ngcnt += ASSERT(0, 1<1);
    ngcnt += ASSERT(0, 2<1);
    ngcnt += ASSERT(1, 0<=1);
    ngcnt += ASSERT(1, 1<=1);
    ngcnt += ASSERT(0, 2<=1);

    ngcnt += ASSERT(1, 1>0);
    ngcnt += ASSERT(0, 1>1);
    ngcnt += ASSERT(0, 1>2);
    ngcnt += ASSERT(1, 1>=0);
    ngcnt += ASSERT(1, 1>=1);
    ngcnt += ASSERT(0, 1>=2);

    ngcnt += ASSERT(1, one());
    ngcnt += ASSERT(2, two());
    ngcnt += ASSERT(3, three());

    ngcnt += ASSERT(3, { let a : i32 = 3; a });
    ngcnt += ASSERT(8, { let a: i32=3; let b: i32=5; a+b });
    ngcnt += ASSERT(7, { let a: i32=2; a+=5; a });
    ngcnt += ASSERT(3, { let a: i32=5; a-=2; a });
    ngcnt += ASSERT(6, { let a: i32=3; a*=2; a });
    ngcnt += ASSERT(3, { let a: i32=6; a/=2; a });
    ngcnt += ASSERT(3, { let a: i32=7; a%=4; a });
    ngcnt += ASSERT(9, { let a: i32=2; a+=5+2; a });
    ngcnt += ASSERT(8, { let a: i32=2; a+=3*2; a });
    ngcnt += ASSERT(3, { let a: i32=9; a+=-3*2; a });
    ngcnt += ASSERT(3, { let foo: i32; foo=3; foo });
    ngcnt += ASSERT(8, { let foo_123: i32=3; let bar: i32=5; foo_123+bar });
    ngcnt += ASSERT(1, { let f1: i32=-1; let f2: i32=2; f1+f2 });
    ngcnt += ASSERT(3, { let A: i32=1; let _B: i32=2; let c99: i32=A+_B; c99 });

    ngcnt += ASSERT( 5, { let i: i32=5; let j: i32=0; while i>0 { j+=1; i-=1; } j });
    ngcnt += ASSERT( 1, { while 1==0 { return 0; } 1 });
    ngcnt += ASSERT(55, { let i: i32=0; let j: i32=0; while i<=10 { j=i+j; i+=1; } j });

    ngcnt += ASSERT( 3, ret3());
    ngcnt += ASSERT( 5, ret5());
    ngcnt += ASSERT( 7, plus(3, 4));
    ngcnt += ASSERT( 7, plus(plus(1, 2), 4));
    ngcnt += ASSERT(10, plus(plus(1, plus(2, 3)), 4));
    ngcnt += ASSERT(66, add6(1,2,add6(3,4,5,6,7,8),9,10,11));
    ngcnt += ASSERT(55, fib(9));

    ngcnt += ASSERT(42, if 1==1 { let a:i32=42; if 1==1 { let a:i32=3; } a } else {0});

    ngcnt += ASSERT(-42, -42);
    ngcnt += ASSERT(-43, !42);
    ngcnt += ASSERT(true, !false);
    ngcnt += ASSERT(false, !true);
    ngcnt += ASSERT(true, !!true);

    ngcnt += ASSERT( 0,  0&1);
    ngcnt += ASSERT( 1,  3&1);
    ngcnt += ASSERT( 3,  7&3);
    ngcnt += ASSERT(10, -1&10);
    ngcnt += ASSERT( 1,  0|1);
    ngcnt += ASSERT(19, 16|3);
    ngcnt += ASSERT( 0,  0^0);
    ngcnt += ASSERT( 0, 15^15);

    ngcnt += ASSERT( 2, { let i:i32= 6; i&=3; i });
    ngcnt += ASSERT( 7, { let i:i32= 6; i|=3; i });
    ngcnt += ASSERT(10, { let i:i32=15; i^=5; i });

    ngcnt += ASSERT(3, { let x:i32=3; *&x });
    ngcnt += ASSERT(3, { let x:i32=3; let y:&i32=&x; let z:&&i32=&y; **z });
    ngcnt += ASSERT(3, { let a:i32=3;let b:&i32=&a;let c:&&i32=&b;let d:&&&i32=&c;let e:&&&&i32=&d; ****e });
    ngcnt += ASSERT(5, { let x:i32=3; let y:&i32=&x; *y=5; x });
    ngcnt += ASSERT(3, { let x:i32=3; let y:i32=5; swap(&x, &y); y });
    ngcnt += ASSERT(5, { let x:i32=3; let y:i32=5; swap(&x, &y); x });

    ngcnt += ASSERT(97, { let a:char='\''a'\''; a });

    ngcnt += ASSERT(1, { let a:i32=97; if a as char=='\''a'\'' { 1 } else { 0 } });
    ngcnt += ASSERT(1, { let a:char='\''a'\''; if a as i32==97 { 1 } else { 0 } });
    ngcnt += ASSERT(1, { let a:bool=true; if a as i32==1{ 1 } else { 0 } });
    ngcnt += ASSERT(0, { let a:bool=false; if a as i32==1{ 1 } else { 0 } });

    ngcnt += ASSERT(5, loop_return());

    ngcnt += ASSERT(false, false && true);
    ngcnt += ASSERT(false, true && (1==2) && true);
    ngcnt += ASSERT(false, true && false);
    ngcnt += ASSERT(true, true && true);
    ngcnt += ASSERT(true, true && (1==1));
    ngcnt += ASSERT(true, false || true);
    ngcnt += ASSERT(true, false || (1==2) || true);
    ngcnt += ASSERT(false, false || false);
    ngcnt += ASSERT(false, false || (1==2));

    ngcnt += ASSERT( 1, 1<<0);
    ngcnt += ASSERT( 8, 1<<3);
    ngcnt += ASSERT(10, 5<<1);
    ngcnt += ASSERT( 2, 5>>1);
    ngcnt += ASSERT(-1, -1>>1);
    ngcnt += ASSERT( 1, { let i:i32=1; i<<=0; i });
    ngcnt += ASSERT( 8, { let i:i32=1; i<<=3; i });
    ngcnt += ASSERT(10, { let i:i32=5; i<<=1; i });
    ngcnt += ASSERT( 2, { let i:i32=5; i>>=1; i });
    ngcnt += ASSERT(-1, { let i:i32=-1; i });
    ngcnt += ASSERT(-1, { let i:i32=-1; i>>=1; i });

    ngcnt += ASSERT(2, { let a:i32=1; let a:i32=2; a });

    if ngcnt == 0 {
        println("ok");
    } else {
        println("failed: {0}", ngcnt);
    }
}
fn ret5() -> i32 { return 5; }'

run '
fn a() -> i32 { // return 1;
    return 2;//comment
}
fn b() -> i32 { /* return 1; */ return 2; /**/ }
fn c() { return; }
fn main() {
    if a() == 2 {
        if b() == 2 {
            c();
            println("ok");
            return;
        }
    }
    println("failed");
}'

run 'fn a() { println("ok"); } fn main() { a(); }'
run 'fn main() { if 1==1 { println("ok"); } else { println("failed"); } }'
run 'fn a() -> i32 { 42 } fn main() { if a()==42 { println("ok") } else { println("failed") } }'
run 'fn a() -> bool { true } fn main() { if true { if a() { println("ok"); } if false { println("failed"); } } }'
run 'fn a() -> char { '\''a'\'' } fn main() { if a()=='\''a'\'' { println("ok"); } else { println("failed"); } }'

run '
struct Rectangle {
    width: i32,
    height: i32,
    point: Point,
}
struct Point {
    x: i32,
    y: i32,
}
impl Rectangle {
    fn print_ok() { println("ok"); }
    fn max(_: &self, a: i32, b: i32) -> i32 { if a > b { a } else { b } }
    fn area(this: &self) -> i32 { this.width * this.height }
}
fn main() {
    let rect: Rectangle = Rectangle { 30, 50, Point { 128, 255 } };
    rect.width = 42;
    rect.point.x = 64;
    if rect.width == 42 && rect.height == 50 && rect.point.x == 64 && rect.point.y == 255 {
        println("ok");
    } else {
        println("failed");
    }
    rect.print_ok();
    if rect.max(3, 6) == 6 {
        println("ok");
    } else {
        println("failed");
    }
    if rect.area() == 2100 {
        println("ok");
    } else {
        println("failed");
    }
}'

# clean up
rm -f tmp.il tmp.exe # ttmp.cs mp.dll

