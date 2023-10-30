
macro puts message
    copy16i msg, msgPtr
    jmp after
.msg: equs message, 13, 0
.after:
    jsr print_message
endmacro

macro stop message
    puts message
    jmp spin
endmacro

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

macro pushA ; hi-byte then lo-byte
    dex
	sta 0, x
endmacro

macro popA ; lo-byte then hi-byte
	lda 0, x
    inx
endmacro

macro commaHereY ; Y needs to be set
    sta (herePtr),y
    xinc16 herePtr
endmacro

osrdch = &ffe0
oswrch = &ffe3

screenStart = &3000

guard &10
org &0

.temp skip 2
.herePtr skip 2
.embeddedPtr skip 2
.msgPtr skip 2
.tempDispatch skip 1 ;; TODO: kill

guard screenStart
org &2000 ;; 1100 NO, 1200 OK?

.start:
    jmp main

.cls:
    lda #22 : jsr oswrch
    lda #7 : .jsr jsr oswrch
    .rts rts

.spin:
    jmp spin

.print_message: {
    tya : pha
    ldy #0
.loop
    lda (msgPtr),y
    beq done
    jsr oswrch
    iny
    bne loop
.done:
    pla : tay
    rts }

.main: {
    ldx #0
    jsr cls
    copy16i embedded, embeddedPtr
    copy16i here_start, herePtr
.loop:
    jsr _key
    jsr _dispatch
    jsr _execute
    jmp loop }

._nop:
    rts

._cr:
    lda #13
    jmp oswrch

._dup:
    dex : dex
    lda 2, x
    sta 0, x
    lda 3, x
    sta 1, x
    rts

._emit: {
	popA
    cmp #10
    bne ok
    clc : adc #3
.ok:
    jsr oswrch
	popA
    rts
    }

._key: {
    lda #0
    pushA
    jsr indirect
    pushA
    rts
.indirect:
    jmp (indirection)
.indirection:
    equw initial
.initial:
    ldy #0
    lda (embeddedPtr),y
    beq switch
    xinc16 embeddedPtr
	rts
.switch:
    copy16i interactive, indirection
.interactive:
    jmp osrdch
    }

._jump:
    pla
    pla
    jmp _execute

._execute: {
    popA
    sta indirection
    popA
    sta indirection+1
    jmp (indirection)
.indirection
    equw 0 }

._compile_comma:
    lda jsr ; TODO: hardcode literal
    ldy #0
    commaHereY
    popA
    commaHereY
    popA
    commaHereY
    rts

._comma:
    ldy #0
    popA
    commaHereY
    popA
    commaHereY
    rts

._write_ret:
    lda rts ; TODO hardcode literal
    ldy #0
    sta (herePtr),y
    xinc16 herePtr
    rts

._here:
    lda herePtr+1
    pushA
    lda herePtr
    pushA
    rts

._lit:
    pla
    sta temp
    pla
    sta temp+1
    xinc16 temp
    ldy #1
    lda (temp),y
    pushA
    ldy #0
    lda (temp),y
    pushA
    xinc16 temp
    lda temp+1
    pha
    lda temp
    pha
    rts

._set_dispatch_table:
    jsr _key
    popA
    asl a
    tay
    popA
    jsr _here
    popA
    sta dispatch_table,y
    popA
    sta dispatch_table+1,y
	rts

._dispatch: {
    popA
    sta tempDispatch
    asl a
    tay
    popA
    lda dispatch_table+1,y
    pushA
    lda dispatch_table,y
    pushA
	ora 1, x
    beq unset
    rts
.unset
    lda #'(' : jsr oswrch
    lda tempDispatch : jsr oswrch
    lda #')' : jsr oswrch
    stop "unset"
    }

U = 0
.dispatch_table:

equw U ; 0
equw U ; 1
equw U ; 2
equw U ; 3
equw U ; 4
equw U ; 5
equw U ; 6
equw U ; 7
equw U ; 8
equw U ; 9
equw _nop ; a
equw U ; b
equw U ; c
equw _nop ; d
equw U ; e
equw U ; f

equw U ; 10
equw U ; 11
equw U ; 12
equw U ; 13
equw U ; 14
equw U ; 15
equw U ; 16
equw U ; 17
equw U ; 18
equw U ; 19
equw U ; 1a
equw U ; 1b
equw U ; 1c
equw U ; 1d
equw U ; 1e
equw U ; 1f

equw _nop ; 20 space
equw U ; 21 !
equw U ; 22 "
equw U ; 23 #
equw U ; 24 $
equw U ; 25 %
equw U ; 26 &
equw U ; 27 '
equw U ; 28 (
equw U ; 29 )
equw U ; 2a *
equw U ; 2b +
equw _comma ; 2c ,
equw U ; 2d -
equw _emit ; 2e .
equw U ; 2f /

equw U ; 30 0
equw U ; 31 1
equw U ; 32 2
equw U ; 33 3
equw U ; 34 4
equw U ; 35 5
equw U ; 36 6
equw U ; 37 7
equw U ; 38 8
equw U ; 39 9
equw _set_dispatch_table ; 3a :
equw _write_ret ; 3b ;
equw U ; 3c <
equw U ; 3d =
equw _compile_comma ; 3e >
equw _dispatch ; 3f ?

equw U ; 40 @
equw U ; 41 A
equw U ; 42 B
equw U ; 43 C
equw _dup ; 44 D
equw U ; 45 E
equw U ; 46 F
equw U ; 47 G
equw U ; 48 H
equw U ; 49 I
equw _jump ; 4a J
equw U ; 4b K
equw _lit ; 4c L
equw _cr ; 4d M
equw U ; 4e N
equw U ; 4f O

equw U ; 50 P
equw U ; 51 Q
equw U ; 52 R
equw U ; 53 S
equw U ; 54 T
equw U ; 55 U
equw _execute ; 56 V
equw U ; 57 W
equw U ; 58 X
equw U ; 59 Y
equw U ; 5a Z
equw U ; 5b [
equw U ; 5c \
equw U ; 5d ]
equw _key ; 5e ^
equw U ; 5f _

equw U ; 60 `
equw U ; 61 a
equw U ; 62 b
equw U ; 63 c
equw U ; 64 d
equw U ; 65 e
equw U ; 66 f
equw U ; 67 g
equw U ; 68 h
equw U ; 69 i
equw U ; 6a j
equw U ; 6b k
equw U ; 6c l
equw U ; 6d m
equw U ; 6e n
equw U ; 6f o

equw U ; 70 p
equw U ; 71 q
equw U ; 72 r
equw U ; 73 s
equw U ; 74 t
equw U ; 75 u
equw U ; 76 v
equw U ; 77 w
equw U ; 78 x
equw U ; 79 y
equw U ; 7a z
equw U ; 7b {
equw U ; 7c |
equw U ; 7d }
equw U ; 7e ~
equw U ; 7f DEL

.dispatch_table_end
assert ((dispatch_table_end - dispatch_table) = 256)

.embedded:
    incbin "play.q"
    equb 0

.here_start:

.end:
print "bytes left: ", screenStart-*
save "Code", start, end
