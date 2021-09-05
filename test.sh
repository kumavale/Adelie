#!/bin/bash

OKCNT=0
NGCNT=0

assert() {
    expected="$1"
    input="$2"

    ./target/debug/adelie "$input" > tmp.il &&
    /mnt/c/Windows/Microsoft.NET/Framework/v4.0.30319/ilasm.exe /QUIET tmp.il &&
    ./tmp.exe

    actual="$?"

    if [ "$actual" = "$expected" ]; then
        echo -e "[  \033[32mOK\033[0m  ] $input => $actual"
        OKCNT=$((OKCNT+1))
    else
        echo -e "[  \033[31mNG\033[0m  ] $input => $expected expected, but got $actual"
        NGCNT=$((NGCNT+1))
    fi
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

assert 1 'fn main() { return 1; 2; 3; }'
assert 2 'fn main() { 1; return 2; 3; }'
assert 3 'fn main() { 1; 2; return 3; }'

assert 3 'fn main() { let a : i32 = 3; return a; }'
assert 8 'fn main() { let a: i32=3; let b: i32=5; return a+b; }'
assert 7 'fn main() { let a: i32=2; a+=5; return a; }'
assert 3 'fn main() { let a: i32=5; a-=2; return a; }'
assert 6 'fn main() { let a: i32=3; a*=2; return a; }'
assert 3 'fn main() { let a: i32=6; a/=2; return a; }'
assert 3 'fn main() { let a: i32=7; a%=4; return a; }'
assert 9 'fn main() { let a: i32=2; a+=5+2; return a; }'
assert 8 'fn main() { let a: i32=2; a+=3*2; return a; }'
assert 3 'fn main() { let a: i32=9; a+=-3*2; return a; }'

assert 3 'fn main() { let foo: i32; foo=3; return foo; }'
assert 6 'fn main() { let foo: i32=2*3; return foo; }'
assert 3 'fn main() { let foo: i32 = 3; return foo; }'
assert 8 'fn main() { let foo_123: i32=3; let bar: i32=5; return foo_123+bar; }'
assert 1 'fn main() { let f1: i32=-1; let f2: i32=2; return f1+f2; }'
assert 3 'fn main() { let A: i32=1; let _B: i32=2; let c99: i32=A+_B; return c99; }'

assert 1 'fn main() { if 1==1 { return 1; } return 0; }'
assert 0 'fn main() { if 1==0 { return 1; } return 0; }'
assert 1 'fn main() { if 1*2==5-3 { return 1; } return 0; }'
assert 3 'fn main() { if 1==1 { if 2==2 { return 3; } return 4; } }'

assert 1 'fn main() { if 1==1 { return 1; } else { return 2; } }'
assert 2 'fn main() { if 1==0 { return 1; } else { if 1==1 { return 2; } } }'
assert 3 'fn main() { if 1==0 { return 1; } else { if 1==0 { return 2; } else { return 3; } } }'
assert 4 'fn main() { if 1==1 { let a: i32=4; return a; } else { if 1 { return 2; } } }'

assert 5 'fn main() { let i: i32=5; let j: i32=0; while i>0 { j+=1; i-=1; } return j; }'
assert 1 'fn main() { while 1==0 { return 0; } return 1; }'
assert 55 'fn main() { let i: i32=0; let j: i32=0; while i<=10 { j=i+j; i+=1; } return j; }'

assert 3 'fn main() { {1; {2;} return 3;} }'

assert 3 'fn ret3() { return 3; } fn main() { return ret3(); }'
assert 5 'fn main() { return ret5(); } fn ret5() { return 5; }'
assert 7 'fn main() { return plus(3, 4); } fn plus(a: i32, b: i32) { return a+b; }'
assert 66 'fn main() { return add6(1,2,add6(3,4,5,6,7,8),9,10,11); }
fn add6(a:i32 ,b:i32, c:i32, d:i32, e:i32, f:i32) { return a+b+c+d+e+f; }'
assert 55 'fn main() { return fib(9); } fn fib(x: i32) { if x<=1 { return 1; } else { return fib(x-1)+fib(x-2); } }'

assert 0 'fn main() { let s: String = "Hello, World!"; return 0; }'
assert 0 'fn main() { println("Hello, World!"); return 0; }'
assert 0 'fn main() { let s: String = "Hello, World!"; println(s); return 0; }'
assert 0 'fn p(s: String) { println(s); return 0; } fn main() { p("Hello, World!"); return 0; }'

assert 0 '
fn ASSERT(expect: i32, actual: i32) {
    if expect == actual {
        print("[  OK  ] expect: ");
        printi32_test(expect);
        print(", actual: ");
        printi32_test(actual);
        println("");
        return 0;
    } else {
        print("[  NG  ] expect: ");
        printi32_test(expect);
        print(", actual: ");
        printi32_test(actual);
        println("");
        return 1;
    }
}

fn main() {
    let okcnt: i32 = 0;
    let ngcnt: i32 = 0;

    ASSERT( 0, 0);
    ASSERT(42, 42);
    ASSERT(21, 5+20-4);
    ASSERT(41, 12 + 34 - 5);
    ASSERT(47, 5+6*7);
    ASSERT(15, 5*(9-6));
    ASSERT( 4, (3+5)/2);
    ASSERT(10, -10+20);
    ASSERT(10, - -10);

    ASSERT(0, 0==1);
    ASSERT(1, 42==42);
    ASSERT(1, 0!=1);
    ASSERT(0, 42!=42);

    ASSERT(1, 0<1);
    ASSERT(0, 1<1);
    ASSERT(0, 2<1);
    ASSERT(1, 0<=1);
    ASSERT(1, 1<=1);
    ASSERT(0, 2<=1);

    ASSERT(1, 1>0);
    ASSERT(0, 1>1);
    ASSERT(0, 1>2);
    ASSERT(1, 1>=0);
    ASSERT(1, 1>=1);
    ASSERT(0, 1>=2);

    print("test result: ");
    if ngcnt == 0 {
        print("ok. ");
    } else {
        print("failed. ");
    }
    printi32_test(okcnt);
    print(" passed; ");
    printi32_test(ngcnt);
    print(" failed;");

    return 0;
}
'
echo
echo -ne "test result: "
if [ $NGCNT -eq 0 ]; then
    echo -ne "\033[32mok\033[0m. "
else
    echo -ne "\033[31mfailed\033[0m. "
fi
echo "$OKCNT passed; $NGCNT failed;"
echo

# clean up
rm -f tmp.il tmp.exe # ttmp.cs mp.dll

