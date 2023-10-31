mode = 1

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
.msg: equs message, 0
.after:
    jsr print_message
endmacro

macro newline ;; no clobber
    pha : lda #13 : jsr osasci : pla ; TODO: call os newline direct. avoid clober A
endmacro

macro stop message
    jsr _cr
    puts message : newline
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

PS = &90 ; Was 0. Bad interaction with osasci

guard &10
org &0

.temp skip 2 ; used by _lit & elsewhere
.embeddedPtr skip 2
.msgPtr skip 2

guard screenStart
org kernelStart

.start:
    jmp main

.hereVar skip 2 ; TODO: make this zero page again. avoid copy into temp

.cls:
    lda #22 : jsr oswrch
    lda #mode : jsr oswrch
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

.printHexA: { ;; clobbers A
    sta mod1+1
    sta mod2+1
    txa : pha ; save X (killing A)
    lda #',' : jsr osasci
    .mod1 lda #&33 ; fixme
    and #&f0 : lsr a : lsr a : lsr a : lsr a : tax
    lda digits,x
    jsr osasci
    .mod2 lda #&44 ; fixme
    and #&f : tax
    lda digits,x
    jsr osasci
    pla : tax ; restore X (killing A)
    rts
.digits EQUS "0123456789abcdef" }

.debug_comma: ;; clobbers A
    ;; show where here is & value in A
    pha
    newline
    lda hereVar+1 : jsr printHexA
    lda hereVar : jsr printHexA
    lda #'=' : jsr osasci
    pla
	jsr printHexA
    rts

macro commaHereY ; takes value in A; Y needs to be set (to 0)
    pha
    lda hereVar : sta temp ; TODO avoid copy when hereVar in zero page again
    lda hereVar+1 : sta temp+1
    pla
    sta (temp),y
    ;;jsr debug_comma
    incWord hereVar
endmacro

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
    copy16i here_start, hereVar
.loop:
    jsr _key
    jsr _dispatch
    jsr _execute
    jmp loop }

._nop:
    rts

macro pushA ; hi-byte then lo-byte
    dex
	sta PS, x
endmacro

macro popA ; lo-byte then hi-byte
	lda PS, x
    inx
endmacro

._drop:
    inx : inx
    rts

._zero:
    lda #0
    pushA
    pushA
    rts

;;; Sadly we can't redifine symbols in beebasm
;;; so we have to explicitly pass the previous def to setup the linked list
macro defword NAME,PREV
.name: equs NAME, 0
equw name, PREV : equb 0
endmacro

def0 = 0

defword "1", def0 : def1=*
._one:
    lda #0
    pushA ;hi
    lda #1
    pushA ;lo
    rts

defword "dup", def1 : def2=*
._dup:
    dex : dex
    lda PS+2, x
    sta PS+0, x
    lda PS+3, x
    sta PS+1, x
    rts

defword "+", def2 : def3=*
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

defword "emit", def3 : def4=*
._emit:
	popA ;lo
    jsr writeChar
	popA ;hi
    rts

defword "cr", def4 : def5=*
._cr:
    lda #13
    jmp osasci

last = def5


._over:
    dex : dex
    lda PS+4, x
    sta PS+0, x
    lda PS+5, x
    sta PS+1, x
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

._minus: ;; PH PL - QH QL

    ;newline : puts "minus(pre): "
    ;lda PS+3,x : jsr printHexA
    ;lda PS+2,x : jsr printHexA
    ;lda PS+1,x : jsr printHexA
    ;lda PS+0,x : jsr printHexA

    sec
    lda PS+2, x ; PL
    sbc PS+0, x ; QL
    sta PS+2, x
    lda PS+3, x ; PH
    sbc PS+1, x ; QH
    sta PS+3, x
    inx : inx

    ;newline : puts "minus(post): "
    ;lda PS+1,x : jsr printHexA
    ;lda PS+0,x : jsr printHexA
    ;newline

    rts

._less_than: {  ;; PH PL < QH QL

    ;newline : puts "less(pre): "
    ;lda PS+3,x : jsr printHexA
    ;lda PS+2,x : jsr printHexA
    ;lda PS+1,x : jsr printHexA
    ;lda PS+0,x : jsr printHexA

    ;; compare high order bytes first
    ;; cmp is like subtract. do: P - Q
    ;; check carry flag
    ;; if it (remains) set we didn't borrow
    ;; if it gets cleared we borrowed (so Q>P)

    ;newline : puts "test hi"
    lda PS+3, x ; PH
    cmp PS+1, x ; QH
	bcc less
    bne notLess
    ;; drop here only if hi-bytes are equal

    beq equal ; should not be needed
    stop "impossible"
.equal:
    ;newline : puts "test lo"

    lda PS+2, x ; PL
    cmp PS+0, x ; QL
    bcc less

.notLess:
    ;newline : puts "NOT-LESS"
    lda #0 ; false
    jmp store

.less:
    ;newline : puts "LESS"
    lda #&ff ; true
.store:
    sta PS+2, x
    sta PS+3, x
    inx : inx

    ;newline : puts "less(post): "
    ;lda PS+1,x : jsr printHexA
    ;lda PS+0,x : jsr printHexA
    ;newline

    rts
    }

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

    ;newline : puts "fetch(pre): "
    ;lda PS+1,x : jsr printHexA
    ;lda PS+0,x : jsr printHexA

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

    ;newline : puts "fetch(post): "
    ;lda PS+1,x : jsr printHexA
    ;lda PS+0,x : jsr printHexA
    ;newline

    rts

._c_fetch:

    ;newline : puts "c_fetch(pre): "
    ;lda PS+1,x : jsr printHexA
    ;lda PS+0,x : jsr printHexA

	lda PS+0, x ; lo-addr
    sta temp
	lda PS+1, x ; hi-addr
    sta temp+1
    ldy #0
    lda (temp),y
	sta PS+0, x ; lo-value
	sty PS+1, x ; hi-value (Y conveniently contains 0) -- This store is the only diff from fetch

    ;newline : puts "c_fetch(post): "
    ;lda PS+1,x : jsr printHexA
    ;lda PS+0,x : jsr printHexA
    ;newline

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

._key:
    lda #0
    pushA ;hi
    jsr key_indirect
    pushA ;lo
    ;;jsr writeChar ; echo : TODO: move echo into key_indirect
    rts

print "_key: &", STR$~(_key)

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

print "_exit: &", STR$~(_exit)

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

    ldy #2
    lda (temp),y
    pushA ;hi
    ldy #1
    lda (temp),y
    pushA ;lo

    incWord temp
    incWord temp

    lda temp+1
    pha
    lda temp
    pha
    rts

print "_lit: &", STR$~(_lit)

._branch0: {
    pla
    sta temp
    pla
    sta temp+1

    popA
    inx : ora PS-1, x
    bne untaken

.taken:
    ;newline : puts "taken"
    ldy #1 : lda (temp),y
	jmp bump
.untaken:
    ;newline : puts "untaken"
    lda #2

.bump:
    ;pha : newline : puts "dist: " : pla : pha : jsr printHexA : newline : pla

    clc
    adc temp
    sta temp
    lda #0 ;; TODO: should be hi-byte of dest for long branches. Use SMC !
    adc temp+1
    sta temp+1
    ;; TODO: avoid saving back into temp. just push directly to return-stack
    lda temp+1
    pha
    lda temp
    pha
    rts
    }

._set_dispatch_table:
    ;newline : puts "set_dispatch_table" : newline
    jsr _key ; TODO use key_indirect, avoid popping from PS
    popA ;lo
    asl a
    sta mod+1
    popA ;hi

    jsr _here_pointer
    jsr _fetch ; clobbers Y ; TODO: inline, avoiding PS manip

    .mod ldy #&33
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


._here_pointer:
    lda #HI(hereVar)
    pushA ;hi
    lda #LO(hereVar)
    pushA ;lo
    ;newline : puts "here_pointer(post): "
    ;lda PS+1,x : jsr printHexA
    ;lda PS+0,x : jsr printHexA
    ;newline
    rts

._entry:
    ;; TODO: create the dictinary entry
    popA
    popA
    rts

._hidden_query:
    ;; TODO: support hidden flag & hiding
    ;; Agghh.. need to pop the arg before pushing the answer! - DONE
    popA
    popA
    lda #0
    pushA
    pushA
    rts

._immediate_query:
    stop "immediate_query"
    rts

._xt_next:
    ;newline : puts "xt_next(pre): "
    ;lda PS+1,x : jsr printHexA
    ;lda PS+0,x : jsr printHexA

	lda PS+0, x ; lo-addr
    sta temp
	lda PS+1, x ; hi-addr
    sta temp+1

    dec temp+1 ; back 256 bytes ; to allow negative acess
    ldy #253 ; -3
    lda (temp),y
	sta PS+0, x ; lo-value
    ldy #254 ; -2
    lda (temp),y
	sta PS+1, x ; hi-value

    ;newline : puts "xt_next(post): "
    ;lda PS+1,x : jsr printHexA
    ;lda PS+0,x : jsr printHexA

    rts

._xt_name:
    ;newline : puts "xt_name(pre): "
    ;lda PS+1,x : jsr printHexA
    ;lda PS+0,x : jsr printHexA

	lda PS+0, x ; lo-addr
    sta temp
	lda PS+1, x ; hi-addr
    sta temp+1

    ;newline : puts "temp: "
    ;lda temp+1 : jsr printHexA
    ;lda temp : jsr printHexA

    ;; backup temp some bytes to allow peeking
    ;; sec
    ;; lda temp
    ;; sbc #8
    ;; sta temp
    ;; lda temp+1
    ;; sbc #0
    ;; sta temp+1

    ;; newline : puts "*temp: "
    ;; ldy #0 : lda (temp),y : jsr printHexA
    ;; ldy #1 : lda (temp),y : jsr printHexA
    ;; ldy #2 : lda (temp),y : jsr printHexA
    ;; ldy #3 : lda (temp),y : jsr printHexA
    ;; ldy #4 : lda (temp),y : jsr printHexA
    ;; ldy #5 : lda (temp),y : jsr printHexA
    ;; ldy #6 : lda (temp),y : jsr printHexA
    ;; ldy #7 : lda (temp),y : jsr printHexA
    ;; ldy #8 : lda (temp),y : jsr printHexA

    ;;dec temp+1 ; back 256 bytes
    ;; to allow negative acess
    ;; newline : puts "*temp: "
    ;; ldy #248 : lda (temp),y : jsr printHexA
    ;; ldy #249 : lda (temp),y : jsr printHexA
    ;; ldy #250 : lda (temp),y : jsr printHexA ; null
    ;; ldy #251 : lda (temp),y : jsr printHexA
    ;; ldy #252 : lda (temp),y : jsr printHexA
    ;; ldy #253 : lda (temp),y : jsr printHexA
    ;; ldy #254 : lda (temp),y : jsr printHexA
    ;; ldy #255 : lda (temp),y : jsr printHexA

    dec temp+1 ; back 256 bytes ; to allow negative acess
    ldy #251 ; -5
    lda (temp),y
	sta PS+0, x ; lo-value
    ldy #252 ; -4
    lda (temp),y
	sta PS+1, x ; hi-value

    ;newline : puts "xt_name(post): "
    ;lda PS+1,x : jsr printHexA
    ;lda PS+0,x : jsr printHexA

    rts


._latest:
    ;; TODO: need var for latest, so new entries can be created
    lda #HI(last)
    pushA
    lda #LO(last)
    pushA
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

equw _nop ; 20 space
equw _store ; 21 !
equw U ; 22 "
equw U ; 23 #
equw U ; 24 $
equw U ; 25 %
equw _key ; 26 & ;; TEMP, make & be _key, because that's what I get when press ^ on my keyboard
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
equw _immediate_query ; 49 I
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

print "kernel size: ", *-start
print "bytes left (after kernel): ", screenStart-*

.here_start:
print "here_start: &", STR$~(here_start)

.embedded:
    ;incbin "play.q"
    incbin "../quarter-forth/f/quarter.q"
    incbin "../quarter-forth/f/forth.f-prefix"
    equb 0

print "embedded size: ", *-embedded

.end:

print "end (after embedded): &", STR$~(end)

save "Code", start, end
