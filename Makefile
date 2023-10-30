
top: build-all

run: _build run-kernel

units = $(patsubst src/%.asm, %, $(wildcard src/*.asm))
ssds = $(patsubst %, _build/%.ssd, $(units))

build-all: _build $(ssds)

run-%: _build/%.ssd
	b-em $<

_build/%.ssd: src/%.asm Makefile play.q ../quarter-forth/f/quarter.q
	@ echo Building $<
	@ beebasm -w -i $< -do $@ -boot Code || rm $@

_build: ; @mkdir -p $@
