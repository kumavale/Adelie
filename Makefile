ILASM=/mnt/c/Windows/Microsoft.NET/Framework/v4.0.30319/ilasm.exe
FLAGS=/QUIET

TEST_SRCS=$(wildcard test/*.ad)
TESTS=$(TEST_SRCS:.ad=.exe)

all: clippy build

build:
	cargo build

clippy:
	cargo clippy

test/%.exe: build test/%.ad
	./target/debug/adelie test/$*.ad > test/$*.il
	$(ILASM) $(FLAGS) /OUTPUT=$@ test/$*.il

test: $(TESTS)
	for i in $^; do echo; echo $$i; ./$$i || exit 1; done

clean:
	cargo clean
	rm -rf tmp* $(TESTS) test/*.il test/*.exe

.PHONY: build clippy test clean
