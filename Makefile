ifeq ($(OS), Windows_NT)
	RUN=
	ILASM=C:/Windows/Microsoft.NET/Framework/v4.0.30319/ilasm.exe
else
	ifeq ($(shell uname), Linux)
		RUN=mono
		ILASM=ilasm
	else
	endif
endif
TEST_SRCS=$(wildcard tests/*.ad)
TESTS=$(TEST_SRCS:.ad=.exe)

all: clippy build build-std

build:
	cargo build

build-std:
	$(ILASM) /OUTPUT:tests/adelie_std.dll -DLL library/std.il

check:
	cargo check

clippy:
	cargo clippy

tests/%.exe: build build-std
	cargo run tests/$*.ad

test: $(TESTS)
	@for i in $^; do echo; echo $$i; $(RUN) ./$$i || exit 1; done

%.ad: build build-std
	cargo run $*.ad && $(RUN) $*.exe

clean:
	cargo clean
	rm -rf tmp* $(TESTS) tests/*.il tests/*.exe example/*.il example/*.exe

.PHONY: build build-std check clippy test clean
