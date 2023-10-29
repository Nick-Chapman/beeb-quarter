
macro copy16i I,V
    lda #LO(I) : sta V
    lda #HI(I) : sta V+1
endmacro

;; TODO: allow naming as inc16
macro xinc16 V
    inc V
    bne done
    inc V+1
.done:
endmacro

osrdch = &ffe0
oswrch = &ffe3

screenStart = &3000

guard &74
org &70
.embedded_ptr SKIP 2

guard screenStart
org &2000 ;; 1100 NO, 1200 OK?

.start:
    jmp main

.cls:
    lda #22 : jsr oswrch
    lda #7 : jsr oswrch
    rts

.main: {
    jsr cls
    copy16i embedded, embedded_ptr
.loop:
    jsr key
    jsr emit
    jmp loop }

.emit: {
    cmp #10
    bne ok
    clc : adc #3
.ok
    jmp oswrch }

.key {
    jmp (indirection)
.indirection
    equw initial
.initial:
    ldy #0
    lda (embedded_ptr),y
    beq switch
    xinc16 embedded_ptr
	rts
.switch
    copy16i interactive, indirection
    ;; fallthrough
.interactive
    jmp osrdch }

.embedded:
    incbin "message.txt"
    equs 13, "Please start typing now...", 13, 0

.end:
print "bytes left: ", screenStart-*
SAVE "Code", start, end
