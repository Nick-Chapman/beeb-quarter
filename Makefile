
top: build-all

run: _build run-kernel

quarter = ../quarter-forth

units = $(patsubst src/%.asm, %, $(wildcard src/*.asm))
ssds = $(patsubst %, _build/%.ssd, $(units))

build-all: _build $(ssds)

run-%: _build/%.ssd
	b-em -sp9 $< # run emulator at 10x speed

_build/%.ssd: src/%.asm Makefile $(wildcard $(quarter)/f/*)
	@ echo Building $<
	@ beebasm -w -i $< -do $@ -boot Code || rm $@

_build: ; @mkdir -p $@
