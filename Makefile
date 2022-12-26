ILASM=/mnt/c/Windows/Microsoft.NET/Framework/v4.0.30319/ilasm.exe
FLAGS=/QUIET

TEST_SRCS=$(wildcard test/*.ad)
TESTS=$(TEST_SRCS:.ad=.exe)

all: clippy build build-std

build:
	cargo build

build-std:
	$(ILASM) /OUTPUT=test/adelie_std.dll /QUIET /DLL library/std.il

check:
	cargo check

clippy:
	cargo clippy

test/%.exe: build build-std
	./target/debug/adelie test/$*.ad
	$(ILASM) $(FLAGS) /OUTPUT=$@ test/$*.il

test: $(TESTS)
	for i in $^; do echo; echo $$i; ./$$i || exit 1; done

%.ad: build build-std
	@if [ -f "$@" ]; then \
		./target/debug/adelie $*.ad && \
		$(ILASM) $(FLAGS) /OUTPUT=$*.exe $*.il && \
		$*.exe; \
	else \
		echo \`$@\` is not found.; \
	fi

clean:
	cargo clean
	rm -rf tmp* $(TESTS) test/*.il test/*.exe example/*.il example/*.exe

.PHONY: build build-std check clippy test clean
