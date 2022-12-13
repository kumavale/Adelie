ILASM=/mnt/c/Windows/Microsoft.NET/Framework/v4.0.30319/ilasm.exe
FLAGS=/QUIET

TEST_SRCS=$(wildcard test/*.ad)
TESTS=$(TEST_SRCS:.ad=.exe)

all: clippy build

build:
	cargo build

check:
	cargo check

clippy:
	cargo clippy

test/%.exe: build test/%.ad
	./target/debug/adelie test/$*.ad > test/$*.il
	$(ILASM) $(FLAGS) /OUTPUT=$@ test/$*.il

test: $(TESTS)
	for i in $^; do echo; echo $$i; ./$$i || exit 1; done

%.ad: build
	@if [ -f "test/$@" ]; then \
		./target/debug/adelie test/$*.ad > test/$*.il && \
		$(ILASM) $(FLAGS) /OUTPUT=test/$*.exe test/$*.il && \
		test/$*.exe; \
	elif [ -f "example/$@" ]; then\
		./target/debug/adelie example/$*.ad > example/$*.il && \
		$(ILASM) $(FLAGS) /OUTPUT=example/$*.exe example/$*.il && \
		example/$*.exe; \
	else \
		echo \`$@\` is not found.; \
	fi

clean:
	cargo clean
	rm -rf tmp* $(TESTS) test/*.il test/*.exe example/*.il example/*.exe

.PHONY: build check clippy test clean
