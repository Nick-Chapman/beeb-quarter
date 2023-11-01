mode = 0

ImmediateFlag = &40
HiddenFlag = &80

jsrOpCode = &20
rtsOpCode = &60

osrdch = &ffe0
osasci = &ffe3
oswrch = &ffee

kernelStart = &1900 ;; 1100 NO, 1200 OK
screenStart = &3000

PS = &90 ; Was 0. Bad interaction with osasci

guard &10
org &0

.hereVar skip 2
.temp skip 2 ; used by _lit & elsewhere
.embeddedPtr skip 2
.msgPtr skip 2

;guard screenStart
org kernelStart

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

macro commaHere ; takes value in A; Y needs to be set to offset
    sta (hereVar), y
    ;;jsr debug_comma
endmacro

macro commaHereBump ; takes value in A
	ldy #0
    commaHere
    incWord hereVar
endmacro

macro pushA ; hi-byte then lo-byte
    dex
	sta PS, x
endmacro

macro popA ; lo-byte then hi-byte
	lda PS, x
    inx
endmacro

macro PsTopToTemp
    lda PS+0, x ; lo-addr
    sta temp
	lda PS+1, x ; hi-addr
    sta temp+1
endmacro

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
    puts "stop:"
    puts message : newline
    jmp spin
endmacro

;;; Sadly we can't redifine symbols in beebasm
;;; so we have to explicitly pass the previous def to setup the linked list
macro defword NAME,PREV
.name: equs NAME, 0
equw name, PREV : equb 0
endmacro

;macro defwordImmediate NAME,PREV
;.name: equs NAME, 0
;equw name, PREV : equb ImmediateFlag
;endmacro

.zeroName: equs 0
macro xdefword NAME,PREV ; NAME ignored. def wont be found
equw zeroName, PREV : equb 0
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.start:
    jmp main

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

._nop:
    rts

.raw_key: {
    jsr indirect
    pha : jsr writeChar : pla ;; echo
    rts
.indirect:
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

d0 = 0

._set_dispatch_table:
    jsr _key ; TODO use raw_key, avoid popping from PS
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

xdefword "dispatch"                      , d0:d1=*
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

xdefword "reset"                         , d1:d2=*
xdefword "bye"                           , d2:d3=*
xdefword "crash"                         , d3:d4=*
xdefword "startup-is-complete"           , d4:d5=*

xdefword "crash-only-during-startup"     , d5:d6=*
._crash_only_during_startup:
    stop "crash_only_during_startup"
    rts

xdefword "sp"                            , d6:d7=*
xdefword "sp0"                           , d7:d8=*
xdefword "rsp"                           , d8:d9=*
xdefword "rsp0"                          , d9:d10=*
xdefword "as-num"                        , d10:d11=*

defword "dup"                           , d11:d12=*
	;; ( x -- x x )
._dup:
    dex : dex
    lda PS+2, x
    sta PS+0, x
    lda PS+3, x
    sta PS+1, x
    rts

defword "swap"                          , d12:d13=*
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

defword "drop"                          , d13:d14=*
._drop:
    inx : inx
    rts

xdefword "over"                          , d14:d15=*
._over:
    dex : dex
    lda PS+4, x
    sta PS+0, x
    lda PS+5, x
    sta PS+1, x
    rts

xdefword ">r"                            , d15:d16=*
xdefword "r>"                            , d16:d17=*

defword "0"                             , d17:d18=*
._zero:
    lda #0
    pushA
    pushA
    rts

defword "1"                             , d18:d19=*
	;; ( -- num )
._one:
    lda #0
    pushA ;hi
    lda #1
    pushA ;lo
    rts

defword "xor"                           , d19:d20=*
    stop "xor"
    rts

xdefword "/2"                            , d20:d21=*

defword "+"                             , d21:d22=*
    ;; ( P Q -- P+Q )
._plus:
    clc
    lda PS+2, x ; PL
    adc PS+0, x ; QL
    sta PS+2, x
    lda PS+3, x ; PH
    adc PS+1, x ; QH
    sta PS+3, x
    inx : inx
    rts

defword "-"                             , d22:d23=*
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

xdefword "*"                             , d23:d24=*
xdefword "/mod"                          , d24:d25=*

defword "<"                             , d25:d26=*
._less_than: {  ;; PH PL < QH QL
    lda PS+3, x ; PH
    cmp PS+1, x ; QH
	bcc less
    bne notLess
    lda PS+2, x ; PL
    cmp PS+0, x ; QL
    bcc less
.notLess:
    lda #0 ; false
    jmp store
.less:
    lda #&ff ; true
.store:
    sta PS+2, x
    sta PS+3, x
    inx : inx
    rts
    }

xdefword "="                             , d26:d27=*
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

defword "@"                             , d27:d28=*
    ;; ( addr -- value )
._fetch:
    PsTopToTemp
    ldy #0
    lda (temp),y
	sta PS+0, x ; lo-value
    ldy #1
    lda (temp),y
	sta PS+1, x ; hi-value
    rts

defword "!"                             , d28:d29=*
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

defword "c@"                            , d29:d30=*
._c_fetch:
    PsTopToTemp
    ldy #0
    lda (temp),y
	sta PS+0, x ; lo-value
	sty PS+1, x ; hi-value (Y conveniently contains 0) -- This store is the only diff from fetch
    rts

xdefword "c!"                            , d30:d31=*

defword "here-pointer"                  , d31:d32=*
    ;; ( -- a )
._here_pointer:
    lda #HI(hereVar)
    pushA
    lda #LO(hereVar)
    pushA
    rts

defword ","                             , d32:d33=*
    ;; ( x -- )
._comma:
    jsr _c_comma
    commaHereBump ; the only extra thing
    rts

defword "c,"                            , d33:d34=*
    ;; ( char -- )
._c_comma:
    popA
    commaHereBump
    popA
    rts

defword "lit"                           , d34:d35=*
	;; ( -- x )
._lit:
    ;; TODO: macroize the next 4 steps
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
    ;; TODO: macroize the next 4 steps
    lda temp+1
    pha
    lda temp
    pha
    rts

xdefword "execute"                       , d35:d36=*
._execute: {
    popA ;lo
    sta mod+1
    popA ;hi
    sta mod+2
    .mod jmp 0
    }

defword "jump"                          , d36:d37=*
	;; ( xt -- )
._jump:
    pla
    pla
    jmp _execute

defword "exit"                          , d37:d38=*
._exit:
    pla
    pla
    rts

defword "0branch"                       , d38:d39=*
._branch0: {
    pla
    sta temp
    pla
    sta temp+1
    popA
    inx : ora PS-1, x
    bne untaken
.taken:
    ldy #1 : lda (temp),y
	jmp bump
.untaken:
    lda #2
.bump:
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

defword "branch"                        , d39:d40=*
	stop "branch"
    rts

xdefword "ret,"                          , d40:d41=*
._write_ret:
    lda #rtsOpCode
    commaHereBump
    rts

defword "compile,"                      , d41:d42=*
    ;; ( xt -- )
._compile_comma:
    lda #jsrOpCode
    commaHereBump
    popA
    commaHereBump
    popA
    commaHereBump
    rts

xdefword "xt->name"                      , d42:d43=*
._xt_name:
    PsTopToTemp
    dec temp+1
    ldy #251 ; -5
    lda (temp),y
	sta PS+0, x ; lo-value
    ldy #252 ; -4
    lda (temp),y
	sta PS+1, x ; hi-value
    rts

xdefword "xt->next"                      , d43:d44=*
._xt_next:
    PsTopToTemp
    dec temp+1
    ldy #253 ; -3
    lda (temp),y
	sta PS+0, x ; lo-value
    ldy #254 ; -2
    lda (temp),y
	sta PS+1, x ; hi-value
    rts

xdefword "immediate?"                    , d44:d45=*
    ;; ( xt -- bool )
._immediate_query: {
    PsTopToTemp
    dec temp+1 ; back 256 bytes ; to allow negative acess
    ldy #255 ; -1
    lda (temp),y
    and #ImmediateFlag
    ;; y is conveniently &ff(true). incrementing will make it false
    bne yes
    iny
.yes:
    ; Y is 00/ff, which if double stored, is false/true
    sty PS+0, x ; lo
    sty PS+1, x ; hi
    rts
    }

xdefword "hidden?"                       , d45:d46=*
    ;; ( xt -- bool ) ;; TODO: support
._hidden_query:
    popA
    popA
    lda #0
    pushA
    pushA
    rts

defword "immediate^"                    , d46:d47=*
    ;; ( xt -- )
    PsTopToTemp
    dec temp+1 ; back 256 bytes ; to allow negative acess
    ldy #255 ; -1
    lda (temp),y
    eor #ImmediateFlag
    sta (temp),y
    rts

xdefword "hidden^"                       , d47:d48=*

xdefword "entry,"                        , d48:d49=*
._entry_comma: ;; ( name -- )
    popA ;lo-name
    commaHereBump
    popA ;hi-name
    commaHereBump
    lda latestVar ;lo-prev
    commaHereBump
    lda latestVar+1 ;hi-prev
    commaHereBump
    lda #0 ; byte for immediate/hidden flags
    commaHereBump
    ;; Set latest to here (after it has been advanced over the entry)
    lda hereVar
    sta latestVar
    lda hereVar+1
    sta latestVar+1
    rts

defword "latest"                        , d49:d50=*
    ;; ( -- a )
._latest:
    lda latestVar+1
    pushA
    lda latestVar
    pushA
    rts

defword "key"                           , d50:d51=*
._key:
    lda #0
    pushA ;hi
    jsr raw_key
    pushA ;lo
    rts

xdefword "set-key"                       , d51:d52=*
xdefword "get-key"                       , d52:d53=*
xdefword "echo-enabled"                  , d53:d54=*
xdefword "echo-off"                      , d54:d55=*
xdefword "echo-on"                       , d55:d56=*

defword "emit"                          , d56:d57=*
    ;; ( char -- )
._emit:
	popA
    jsr writeChar
	popA
    rts

defword "cr"                            , d57:d58=*
._cr:
    ;; ( -- )
    lda #13
    jmp osasci

xdefword "cls"                           , d58:d59=*

last = d59
.latestVar equw last

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
equw _entry_comma ; 45 E
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
;print "bytes left (after kernel): ", screenStart-*

.here_start:
print "here_start: &", STR$~(here_start)

.embedded:
    incbin "../quarter-forth/f/quarter.q"
    incbin "../quarter-forth/f/forth.f"
    equb 0

print "embedded size: ", *-embedded

.end:

print "end (after embedded): &", STR$~(end)

save "Code", start, end
