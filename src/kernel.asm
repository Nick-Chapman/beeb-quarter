
jsrOpCode = &20
rtsOpCode = &60

osrdch = &ffe0
osasci = &ffe3
oswrch = &ffee

kernelStart = &1900 ;; 1100 NO, 1200 OK
screenStart = &3000

macro puts message
    copy16i msg, msgPtr
    jmp after
.msg: equs message, 13, 0
.after:
    jsr print_message
endmacro

macro stop message
    jsr _cr
    puts message
    jmp spin
endmacro

macro copy16i I,V
    lda #LO(I) : sta V
    lda #HI(I) : sta V+1
endmacro

macro incWord V
    inc V
    bne done
    inc V+1
.done:
endmacro

;; TODO: add PS overflow check
PS = &90 ; Was 0. Bad interaction with osasci

guard &10
org &0

.herePtr skip 2
.temp skip 2 ; used by _lit
.embeddedPtr skip 2
.msgPtr skip 2

guard screenStart
org kernelStart

.start:
    jmp main

.cls:
    lda #22 : jsr oswrch
    lda #7 : jsr oswrch
    rts

.spin:
    jmp spin

.writeChar: { ; converting asci 10-->13
    cmp #10
    bne ok
    lda #13
.ok:
    jmp osasci ; has special handling for 13 -> NL
    }

.print_message: { ; just for dev; used by stop/puts
    tya : pha
    ldy #0
.loop
    lda (msgPtr),y
    beq done
    jsr osasci
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
    jmp osasci

macro pushA ; hi-byte then lo-byte
    dex
	sta PS, x
endmacro

macro popA ; lo-byte then hi-byte
	lda PS, x
    inx
endmacro

._drop:
    stop "dup"
    rts

._dup:
    dex : dex
    lda PS+2, x
    sta PS+0, x
    lda PS+3, x
    sta PS+1, x
    rts

._over:
    stop "over"
    rts

._swap:
    ldy PS+0, x
    lda PS+2, x
    sty PS+2, x
    sta PS+0, x
    ldy PS+1, x
    lda PS+3, x
    sty PS+3, x
    sta PS+1, x
    rts

._plus: ;; PH PL + QH QL
    clc
    lda PS+2, x ; PL
    adc PS+0, x ; QL
    sta PS+2, x
    lda PS+3, x ; PH
    adc PS+1, x ; QH
    sta PS+3, x
    inx : inx
    rts

._minus: ;; PH PL - QH QL
    sec
    lda PS+2, x ; PL
    sbc PS+0, x ; QL
    sta PS+2, x
    lda PS+3, x ; PH
    sbc PS+1, x ; QH
    sta PS+3, x
    inx : inx
    rts

._less_than:
    stop "less_than"
    rts

._equal: { ;; PH PL = QH QL
    lda PS+2, x ; PL
    cmp PS+0, x ; QL
    bne diff
    lda PS+3, x ; PH
    cmp PS+1, x ; QH
    bne diff
.same:
    lda #&ff ; true
    jmp store
.diff:
    lda #0 ; false
.store:
    sta PS+2, x
    sta PS+3, x
    inx : inx
    rts }

._fetch: ; ( addr -- value )
	lda PS+0, x ; lo-addr
    sta temp
	lda PS+1, x ; hi-addr
    sta temp+1
    ldy #0
    lda (temp),y
	sta PS+0, x ; lo-value
    ldy #1
    lda (temp),y
	sta PS+1, x ; hi-value
    rts

._c_fetch:
    stop "c_fetch"
    rts

._store: ; ( value addr -- )
    popA ;lo-addr
    sta temp
    popA ;hi-addr
    sta temp+1
    popA ;lo-value
    ldy #0
    sta (temp),y
    popA ;hi-value
    ldy #1
    sta (temp),y
    rts

._emit:
	popA ;lo
    jsr writeChar
	popA ;hi
    rts

._key:
    lda #0
    pushA ;hi
    jsr key_indirect
    pushA ;lo
    jsr writeChar ; echo
    rts

.key_indirect: {
    jmp (indirection) ; TODO: prefer SMC
.indirection:
    equw initial
.initial:
    ldy #0
    lda (embeddedPtr),y
    beq switch
    incWord embeddedPtr
	rts
.switch:
    copy16i interactive, indirection
.interactive:
    jmp osrdch
    }

._exit:
    pla
    pla
    rts

._jump:
    pla
    pla
    jmp _execute

._execute: {
    popA ;lo
    sta mod+1
    popA ;hi
    sta mod+2
    .mod jmp 0
    }

macro commaHereY ; Y needs to be set
    sta (herePtr),y
    incWord herePtr
endmacro

._write_ret:
    lda #rtsOpCode
    ldy #0
    commaHereY
    rts

._compile_comma:
    lda #jsrOpCode
    ldy #0
    commaHereY
    popA ;lo
    commaHereY
    popA ;hi
    commaHereY
    rts

._comma:
    jsr _c_comma
    commaHereY ; the only extra thing
    rts

._c_comma:
    ldy #0
    popA ;lo
    commaHereY
    popA ;hi
    rts

._lit:
    pla
    sta temp
    pla
    sta temp+1
    incWord temp
    ldy #1
    lda (temp),y
    pushA ;hi
    ldy #0
    lda (temp),y
    pushA ;lo
    incWord temp
    lda temp+1
    pha
    lda temp
    pha
    rts

._branch0:
    popA
    inx
    ora PS-1, x
    bne untaken

.taken:
    pla
    sta temp
    pla
    sta temp+1
	;; TODO: branch the correct distance! instead of hacking to be 5 !
    ;; TODO TODO TODO !!!
    incWord temp
    incWord temp
    incWord temp
    incWord temp
    incWord temp
    lda temp+1
    pha
    lda temp
    pha
    rts
    rts

.untaken:
    pla
    sta temp
    pla
    sta temp+1
    incWord temp
    incWord temp
    lda temp+1
    pha
    lda temp
    pha
    rts

._set_dispatch_table:
    jsr _key
    popA ;lo
    asl a
    tay
    popA ;hi
    jsr _here_pointer
    popA ;lo
    sta dispatch_table,y
    popA ;hi
    sta dispatch_table+1,y
	rts

._dispatch: {
    popA ;lo
    sta mod+1
    asl a
    tay
    popA ;hi
    lda dispatch_table+1,y
    pushA ;hi
    lda dispatch_table,y
    pushA ;lo
	ora PS+1, x
    beq unset
    rts
.unset:
    jsr _cr
    .mod lda #0 : jsr oswrch
    lda #'?' : jsr oswrch
    jmp spin
    }

._zero:
    lda #0
    pushA
    pushA
    rts

._one:
    lda #0
    pushA ;hi
    lda #1
    pushA ;lo
    rts

._here_pointer:
    lda herePtr+1
    pushA ;hi
    lda herePtr
    pushA ;lo
    rts

._entry:
    ;; TODO: create the dictinary entry
    popA
    popA
    rts

._hidden_query:
    stop "hidden_query"
    rts

._xt_next:
    stop "xt_next"
    rts

._xt_name:
    stop "xt_name"
    rts

._latest:
    stop "latest"
    rts

._crash_only_during_startup:
    stop "crash_only_during_startup"
    rts

print "kernel size (sans dispatch table): ", *-start

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
equw _store ; 21 !
equw U ; 22 "
equw U ; 23 #
equw U ; 24 $
equw U ; 25 %
equw U ; 26 &
equw U ; 27 '
equw U ; 28 (
equw U ; 29 )
equw U ; 2a *
equw _plus ; 2b +
equw _comma ; 2c ,
equw _minus ; 2d -
equw _emit ; 2e .
equw U ; 2f /

equw _zero ; 30 0
equw _one ; 31 1
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
equw _less_than ; 3c <
equw _equal ; 3d =
equw _compile_comma ; 3e >
equw _dispatch ; 3f ?

equw _fetch ; 40 @
equw _crash_only_during_startup ; 41 A
equw _branch0 ; 42 B
equw _c_fetch ; 43 C
equw _dup ; 44 D
equw _entry ; 45 E
equw U ; 46 F
equw _xt_next ; 47 G
equw _here_pointer ; 48 H
equw U ; 49 I
equw _jump ; 4a J
equw U ; 4b K
equw _lit ; 4c L
equw _cr ; 4d M
equw _xt_name ; 4e N
equw _over ; 4f O

equw _drop ; 50 P
equw U ; 51 Q
equw U ; 52 R
equw U ; 53 S
equw U ; 54 T
equw U ; 55 U
equw _execute ; 56 V
equw _swap ; 57 W
equw _exit ; 58 X
equw _hidden_query ; 59 Y
equw _latest ; 5a Z
equw U ; 5b [
equw U ; 5c \
equw U ; 5d ]
equw _key ; 5e ^
equw U ; 5f _

equw _c_comma ; 60 `
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

;print "kernel size: ", *-start
;print "bytes left (after kernel): ", screenStart-*

.here_start:

.embedded:
    ;incbin "play.q"
    incbin "../quarter-forth/f/quarter.q"
    equb 0

;print "embedded size: ", *-embedded
;print "bytes left (after embedded): ", screenStart-*

.end:

save "Code", start, end
