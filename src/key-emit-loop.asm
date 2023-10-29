
screenStart = &3000

osrdch = &ffe0
oswrch = &ffe3

guard screenStart
org &1100

.start:
    jmp main
.main:
.loop:
    jsr osrdch
    jsr oswrch
    jmp loop

.end:
print "bytes left: ", screenStart-*
SAVE "Code", start, end
