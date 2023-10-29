
top: build-all

run: _build run-key-emit-loop

units = $(patsubst src/%.asm, %, $(wildcard src/*.asm))
ssds = $(patsubst %, _build/%.ssd, $(units))

build-all: _build $(ssds)

run-%: _build/%.ssd
	b-em $<

_build/%.ssd: src/%.asm Makefile
	@ echo Building $<
	@ beebasm -i $< -do $@ -boot Code || rm $@

_build: ; @mkdir -p $@
