all: clippy build

build:
	@cargo build

clippy:
	@cargo clippy

test: build test.sh
	@./test.sh
	@rm -f tmp.il tmp.exe

clean:
	@cargo clean
	@rm -f tmp.il tmp.exe
