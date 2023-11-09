
default = bbc

top: build-$(default)
run: run-$(default)

kernel = src/kernel.asm
quarter = ../quarter-forth

build-%: _build/%.ssd
	@ echo -n

run-%: _build _build/%.ssd
	b-em -sp9 _build/$*.ssd

.PRECIOUS:_build/%.ssd
_build/%.ssd: _build _build/%.f $(kernel)
	@ echo Building $(kernel)
	@ beebasm -S FORTH=_build/$*.f -w -i $(kernel) -do $@ -boot Code || rm $@

forth = $(wildcard $(quarter)/*.f) $(wildcard $(quarter)/f/*)

.PRECIOUS:_build/%.f
_build/%.f : $(quarter)/%.list $(forth)
	@ echo Combining Forth files: $<
	@ bash -c '(cd $(quarter); cat $< | sed s/#.*// | xargs cat) > $@' || rm -f $@

_build: ; @mkdir -p $@
