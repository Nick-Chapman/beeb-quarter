
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

guard &10
org &0
.embedded_ptr SKIP 2
.tempDispatch SKIP 1 ;; TODO: kill

guard screenStart
org &2000 ;; 1100 NO, 1200 OK?

.start:
    jmp main

.cls:
    lda #22 : jsr oswrch
    lda #7 : jsr oswrch
    rts

.spin:
    jmp spin

.main: {
    ldx #0
    jsr cls
    copy16i embedded, embedded_ptr
.loop:
    lda #'^' ; key
    jsr dispatchA
    ;lda #'D' ; dup
    ;jsr dispatchA
    ;lda #'.' ; emit
    ;jsr dispatchA
    lda #'.' ; emit
    jsr dispatchA
    jmp loop }

macro pushA
    dex
	sta 0, x
endmacro

macro popA
	lda 0, x
    inx
endmacro

.dup:
    dex : dex
    lda 2, x
    sta 0, x
    lda 3, x
    sta 1, x
    rts

.emit: {
	popA
	popA
    cmp #10
    bne ok
    clc : adc #3
.ok:
    jsr oswrch
    rts
    }

.key: {
    jsr indirect
    pushA
    pushA
    rts
.indirect:
    jmp (indirection)
.indirection:
    equw initial
.initial:
    ldy #0
    lda (embedded_ptr),y
    beq switch
    xinc16 embedded_ptr
	rts
.switch:
    copy16i interactive, indirection
.interactive:
    jmp osrdch
    }

.dispatchA: {
    sta tempDispatch ;; TODO: kill when we have a param stack
    asl a
    tay
    ;; TODO: macroize
    lda dispatch_table,y
    sta indirection
    lda dispatch_table+1,y
    sta indirection+1
    jmp (indirection)
.indirection
    equw 0
    }

.error_unset_dispatch_table_entry:
    lda tempDispatch : jsr oswrch
    lda #'!' : jsr oswrch : lda #'D' : jsr oswrch
    jmp spin

U = error_unset_dispatch_table_entry
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
equw U ; a
equw U ; b
equw U ; c
equw U ; d
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

equw U ; 20 space
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
equw U ; 2c ,
equw U ; 2d -
equw emit ; 2e .
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
equw U ; 3a :
equw U ; 3b ;
equw U ; 3c <
equw U ; 3d =
equw U ; 3e >
equw U ; 3f ?

equw U ; 40 @
equw U ; 41 A
equw U ; 42 B
equw U ; 43 C
equw dup ; 44 D
equw U ; 45 E
equw U ; 46 F
equw U ; 47 G
equw U ; 48 H
equw U ; 49 I
equw U ; 4a J
equw U ; 4b K
equw U ; 4c L
equw U ; 4d M
equw U ; 4e N
equw U ; 4f O

equw U ; 50 P
equw U ; 51 Q
equw U ; 52 R
equw U ; 53 S
equw U ; 54 T
equw U ; 55 U
equw U ; 56 V
equw U ; 57 W
equw U ; 58 X
equw U ; 59 Y
equw U ; 5a Z
equw U ; 5b [
equw U ; 5c \
equw U ; 5d ]
equw key ; 5e ^
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
ASSERT ((dispatch_table_end - dispatch_table) = 256)

.embedded:
    incbin "message.txt"
    equs 13, "Please start typing now...", 13, 0

.end:
print "bytes left: ", screenStart-*
SAVE "Code", start, end
