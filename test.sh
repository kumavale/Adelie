#!/bin/bash

OKCNT=0
NGCNT=0

assert() {
    expected="$1"
    input="$2"

    ./target/debug/adelie "$input" > tmp.il &&
    /mnt/c/Windows/Microsoft.NET/Framework/v4.0.30319/ilasm.exe /QUIET tmp.il &&
    ./tmp.exe

    #actual="$?"

    #if [ "$actual" = "$expected" ]; then
    #    echo -e "[  \033[32mOK\033[0m  ] $input => $actual"
    #    OKCNT=$((OKCNT+1))
    #else
    #    echo -e "[  \033[31mNG\033[0m  ] $input => $expected expected, but got $actual"
    #    NGCNT=$((NGCNT+1))
    #fi
}

cargo build
if [ "$?" != "0" ]; then
    # build error
    exit 1
fi

echo

#assert 0  'fn main() { return 0; }'
#assert 42 'fn main() { return 42; }'
#assert 21 'fn main() { return 5+20-4; }'
#assert 41 'fn main() { return 12 + 34 - 5; }'
#assert 47 'fn main() { return 5+6*7; }'
#assert 15 'fn main() { return 5*(9-6); }'
#assert 4  'fn main() { return (3+5)/2; }'
#assert 10 'fn main() { return -10+20; }'
#assert 10 'fn main() { return - -10; }'

#assert 0 'fn main() { return 0==1; }'
#assert 1 'fn main() { return 42==42; }'
#assert 1 'fn main() { return 0!=1; }'
#assert 0 'fn main() { return 42!=42; }'

#assert 1 'fn main() { return 0<1; }'
#assert 0 'fn main() { return 1<1; }'
#assert 0 'fn main() { return 2<1; }'
#assert 1 'fn main() { return 0<=1; }'
#assert 1 'fn main() { return 1<=1; }'
#assert 0 'fn main() { return 2<=1; }'

#assert 1 'fn main() { return 1>0; }'
#assert 0 'fn main() { return 1>1; }'
#assert 0 'fn main() { return 1>2; }'
#assert 1 'fn main() { return 1>=0; }'
#assert 1 'fn main() { return 1>=1; }'
#assert 0 'fn main() { return 1>=2; }'

#assert 1 'fn main() { return 1; 2; 3; }'
#assert 2 'fn main() { 1; return 2; 3; }'
#assert 3 'fn main() { 1; 2; return 3; }'

#assert 3 'fn main() { let a : i32 = 3; return a; }'
#assert 8 'fn main() { let a: i32=3; let b: i32=5; return a+b; }'
#assert 7 'fn main() { let a: i32=2; a+=5; return a; }'
#assert 3 'fn main() { let a: i32=5; a-=2; return a; }'
#assert 6 'fn main() { let a: i32=3; a*=2; return a; }'
#assert 3 'fn main() { let a: i32=6; a/=2; return a; }'
#assert 3 'fn main() { let a: i32=7; a%=4; return a; }'
#assert 9 'fn main() { let a: i32=2; a+=5+2; return a; }'
#assert 8 'fn main() { let a: i32=2; a+=3*2; return a; }'
#assert 3 'fn main() { let a: i32=9; a+=-3*2; return a; }'

#assert 3 'fn main() { let foo: i32; foo=3; return foo; }'
#assert 6 'fn main() { let foo: i32=2*3; return foo; }'
#assert 3 'fn main() { let foo: i32 = 3; return foo; }'
#assert 8 'fn main() { let foo_123: i32=3; let bar: i32=5; return foo_123+bar; }'
#assert 1 'fn main() { let f1: i32=-1; let f2: i32=2; return f1+f2; }'
#assert 3 'fn main() { let A: i32=1; let _B: i32=2; let c99: i32=A+_B; return c99; }'

#assert 1 'fn main() { if 1==1 { return 1; } return 0; }'
#assert 0 'fn main() { if 1==0 { return 1; } return 0; }'
#assert 1 'fn main() { if 1*2==5-3 { return 1; } return 0; }'
#assert 3 'fn main() { if 1==1 { if 2==2 { return 3; } return 4; } }'

#assert 1 'fn main() { if 1==1 { return 1; } else { return 2; } }'
#assert 2 'fn main() { if 1==0 { return 1; } else { if 1==1 { return 2; } } }'
#assert 3 'fn main() { if 1==0 { return 1; } else { if 1==0 { return 2; } else { return 3; } } }'
#assert 4 'fn main() { if 1==1 { let a: i32=4; return a; } else { if 1 { return 2; } } }'

#assert 5 'fn main() { let i: i32=5; let j: i32=0; while i>0 { j+=1; i-=1; } return j; }'
#assert 1 'fn main() { while 1==0 { return 0; } return 1; }'
#assert 55 'fn main() { let i: i32=0; let j: i32=0; while i<=10 { j=i+j; i+=1; } return j; }'

#assert 3 'fn main() { {1; {2;} return 3;} }'

#assert 3 'fn ret3() { return 3; } fn main() { return ret3(); }'
#assert 5 'fn main() { return ret5(); } fn ret5() { return 5; }'
#assert 7 'fn main() { return plus(3, 4); } fn plus(a: i32, b: i32) { return a+b; }'
#assert 66 'fn main() { return add6(1,2,add6(3,4,5,6,7,8),9,10,11); }
#fn add6(a:i32 ,b:i32, c:i32, d:i32, e:i32, f:i32) { return a+b+c+d+e+f; }'
#assert 55 'fn main() { return fib(9); } fn fib(x: i32) { if x<=1 { return 1; } else { return fib(x-1)+fib(x-2); } }'

#assert 0 'fn main() { let s: String = "Hello, World!"; return 0; }'
#assert 0 'fn main() { println("Hello, World!"); return 0; }'
#assert 0 'fn main() { let s: String = "Hello, World!"; println(s); return 0; }'
#assert 0 'fn p(s: String) { println(s); return 0; } fn main() { p("Hello, World!"); return 0; }'

assert 0 '
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
    let format: String = "[  {0}  ] expect: {1}, actual: {2}";
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

    if ngcnt == 0 {
        println("ok");
    } else {
        println("failed: {0}", ngcnt);
    }
}
fn ret5() -> i32 { return 5; }'

assert 0 '
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

assert 0 'fn a() { println("ok"); } fn main() { a(); }'
assert 0 'fn main() { if 1==1 { println("ok"); } else { println("failed"); } }'
assert 0 'fn a() -> i32 { 42 } fn main() { if a()==42 { println("ok") } else { println("failed") } }'
assert 0 'fn a() -> bool { true } fn main() { if true { if a() { println("ok"); } if false { println("failed"); } } }'
assert 0 'fn a() -> char { '\''a'\'' } fn main() { if a()=='\''a'\'' { println("ok"); } else { println("failed"); } }'

#echo
#echo -ne "test result: "
#if [ $NGCNT -eq 0 ]; then
#    echo -ne "\033[32mok\033[0m. "
#else
#    echo -ne "\033[31mfailed\033[0m. "
#fi
#echo "$OKCNT passed; $NGCNT failed;"
#echo

# clean up
rm -f tmp.il tmp.exe # ttmp.cs mp.dll

