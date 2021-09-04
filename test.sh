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

assert 0  'return 0;'
assert 42 'return 42;'
assert 21 'return 5+20-4;'
assert 41 'return 12 + 34 - 5;'
assert 47 'return 5+6*7;'
assert 15 'return 5*(9-6);'
assert 4  'return (3+5)/2;'
assert 10 'return -10+20;'
assert 10 'return - -10;'

assert 0 'return 0==1;'
assert 1 'return 42==42;'
assert 1 'return 0!=1;'
assert 0 'return 42!=42;'

assert 1 'return 0<1;'
assert 0 'return 1<1;'
assert 0 'return 2<1;'
assert 1 'return 0<=1;'
assert 1 'return 1<=1;'
assert 0 'return 2<=1;'

assert 1 'return 1>0;'
assert 0 'return 1>1;'
assert 0 'return 1>2;'
assert 1 'return 1>=0;'
assert 1 'return 1>=1;'
assert 0 'return 1>=2;'

assert 1 'return 1; 2; 3;'
assert 2 '1; return 2; 3;'
assert 3 '1; 2; return 3;'

assert 3 'a = 3; return a;'
assert 8 'a=3; b=5; return a+b;'
assert 7 'a=2; a+=5; return a;'
assert 3 'a=5; a-=2; return a;'
assert 6 'a=3; a*=2; return a;'
assert 3 'a=6; a/=2; return a;'
assert 3 'a=7; a%=4; return a;'
assert 9 'a=2; a+=5+2; return a;'
assert 8 'a=2; a+=3*2; return a;'
assert 3 'a=9; a+=-3*2; return a;'

assert 3 'foo = 3; return foo;'
assert 8 'foo_123=3; bar=5; return foo_123+bar;'
assert 1 'f1=-1; f2=2; return f1+f2;'
assert 3 'A=1; _B=2; c99=a+b; return c99;'

#assert 1 'if(1) return 1; return 0;'
#assert 0 'if(0) return 1; return 0;'
#assert 1 'if(1*2==5-3) return 1; return 0;'
#assert 3 'if(1) if(2) return 3; return 4;'
#
#assert 1 'if(1) return 1; else return 2;'
#assert 2 'if(0) return 1; else if(1) return 2;'
#assert 3 'if(0) return 1; else if(0) return 2; else return 3;'
#
#assert 4 'i=5; j=0; while(i=i-1) j=j+1; return j;'
#assert 1 'while(0) return 0; return 1;'
#assert 55 'i=0; j=0; while(i<=10) {j=i+j; i=i+1;} return j;'
#
#assert 10 'j=0; for(i=0; i<5; i=i+1) j=j+i; return j;'
#assert 1 'for(;;) return 1; return 0;'
#
#assert 3 '{1; {2;} return 3;}'
#
#assert 3 'return ret3();'
#assert 5 'return ret5();'

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
rm tmp.il tmp.exe

