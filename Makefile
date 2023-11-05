
top: build-full

run: run-full

kernel = src/kernel.asm
quarter = ../quarter-forth

build-%: _build/%.ssd
	@ echo -n

run-%: _build _build/%.ssd
	b-em -sp9 _build/$*.ssd

.PRECIOUS:_build/%.ssd
_build/%.ssd: _build _build/%.f $(kernel) Makefile
	@ echo Building $(kernel)
	@ beebasm -S FORTH=_build/$*.f -w -i $(kernel) -do $@ -boot Code || rm $@

.PRECIOUS:_build/%.f
_build/%.f : %.list $(wildcard f/*) $(wildcard $(quarter)/f/*) Makefile
	@ echo Combining Forth files: $<
	@ bash -c 'cat $< | sed s/#.*// | xargs cat > $@' || rm -f $@

_build: ; @mkdir -p $@
