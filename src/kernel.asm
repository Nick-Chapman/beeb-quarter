Echo = TRUE
Mode = 7 ;; Can't use Mode0. We have reached embedded text in the screen area.

ImmediateFlag = &40
HiddenFlag = &80

jsrOpCode = &20
rtsOpCode = &60

osrdch = &ffe0
osasci = &ffe3
osnewl = &ffe7
oswrch = &ffee

kernelStart = &1900 ;; 1100 NO, 1200 OK
screenStart = &3000

guard &10
org &0

.hereVar skip 2
.temp skip 2 ; used by _lit & elsewhere
.embeddedPtr skip 2
.msgPtr skip 2

;guard screenStart
org kernelStart

.start:
    jmp main

;;; just for dev/debug
.printHexA: { ;; clobbers A
    sta mod1+1
    sta mod2+1
    txa : pha ; save X (killing A)
    lda #',' : jsr osasci
    .mod1 lda #&33 ; SMC
    and #&f0 : lsr a : lsr a : lsr a : lsr a : tax
    lda digits,x
    jsr osasci
    .mod2 lda #&44 ; SMC
    and #&f : tax
    lda digits,x
    jsr osasci
    pla : tax ; restore X (killing A)
    rts
.digits EQUS "0123456789abcdef" }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parameter Stack (PS)

;;; The parameter stack lives in zero-page growing downwards from &90.
;;; The X register indexes the stack top.
;;; For byte access we use indexed addressing: "PS+N, x".
;;; Each stack entry is 16-bits (2 bytes).
;;; The hi-byte is always deeper in the stack than the lo-byte, so:
;;; When pushing (pushA): push hi; push lo.
;;; When popping (popA): pop lo; pop hi.

PS = &90

macro pushA
    dex
	sta PS, x
endmacro

macro popA
	lda PS, x
    inx
endmacro

macro PsTopToTemp
    lda PS+0, x
    sta temp
	lda PS+1, x
    sta temp+1
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Return Stack (RS)

;;; The return stack uses the 6502 hardware stack, which lives in page-1
;;; To push/pop to the return-stack, we use: jsr/rts, or: pha/pla
;;; uppercase Push/Pop indicates action on 2 bytes

macro PopRsTemp
    pla
    sta temp
    pla
    sta temp+1
endmacro

macro PushRsTemp
    lda temp+1
    pha
    lda temp
    pha
endmacro

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

macro puts message
    copy16i msg, msgPtr
    jmp after
.msg: equs message, 0
.after:
    jsr print_message
endmacro

macro newline ;; no clobber A
    pha : jsr osnewl : pla
endmacro

macro stop message
    jsr osnewl
    puts "stop:"
    puts message : newline
    jmp spin
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    lda #Mode : jsr oswrch
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
    rts
    }

._nop:
    rts

.readChar: {
    jsr indirect
    IF Echo : pha : jsr writeChar : pla : ENDIF
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
;;; Quarter/Forth kernel primitives (leading "_")

macro defword NAME,PREV
.name: equs NAME, 0
equw name, PREV : equb 0
endmacro

.zeroName: equs 0
macro xdefword NAME,PREV ; NAME ignored. def wont be found ;; TODO: kill when unused
equw zeroName, PREV : equb 0
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

d0 = 0

._set_dispatch:
    jsr readChar
    asl a
    tay
    lda hereVar
    sta dispatch_table,y
    lda hereVar+1
    sta dispatch_table+1,y
	rts

xdefword "dispatch"                      , d0:d1=*
._dispatch: {
    popA
    sta mod+1 ; for unset error
    asl a
    tay
    inx ;pop/ignore hi-byte
    lda dispatch_table+1,y
    pushA
    lda dispatch_table,y
    pushA
	ora PS+1, x
    beq unset
    rts
.unset:
    jsr osnewl
    .mod lda #&33 : jsr oswrch ; SMC
    lda #'?' : jsr oswrch
    jmp spin
    }

xdefword "reset"                         , d1:d2=*
xdefword "bye"                           , d2:d3=*

defword "crash"                         , d3:d4=*
    stop "crash"
    rts

xdefword "startup-is-complete"           , d4:d5=*

xdefword "crash-only-during-startup"     , d5:d6=*
._crash_startup:
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
	;; ( x y -- y x )
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
	;; ( x -- )
._drop:
    inx : inx
    rts

defword "over"                          , d14:d15=*
	;; ( x y -- x y x )
._over:
    dex : dex
    lda PS+4, x
    sta PS+0, x
    lda PS+5, x
    sta PS+1, x
    rts

defword ">r"                            , d15:d16=*
	stop ">r"
    rts

defword "r>"                            , d16:d17=*
    stop "r>"
    rts

defword "0"                             , d17:d18=*
	;; ( -- num )
._zero:
    lda #0
    pushA
    pushA
    rts

defword "1"                             , d18:d19=*
	;; ( -- num )
._one:
    lda #0
    pushA
    lda #1
    pushA
    rts

defword "xor"                           , d19:d20=*
    lda PS+2, x
    eor PS+0, x
    sta PS+2, x
    lda PS+3, x
    eor PS+1, x
    sta PS+3, x
    inx : inx
    rts

xdefword "/2"                            , d20:d21=*

defword "+"                             , d21:d22=*
	;; ( numP numQ -- num )
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
	;; ( numP numQ -- num )
._minus:
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
defword "/mod"                          , d24:d25=*
    stop "/mod"
    rts

defword "<"                             , d25:d26=*
	;; ( numP numQ -- bool )
._less_than: {
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

defword "="                             , d26:d27=*
	;; ( numP numQ -- bool )
._equal: {
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
    ;; ( value addr -- )
._store:
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

defword "c!"                            , d30:d31=*
    stop "c!"
    rts

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
    popA
    commaHereBump
    popA
    commaHereBump
    rts

defword "c,"                            , d33:d34=*
    ;; ( char -- )
._c_comma:
    popA
    commaHereBump
	inx
    rts

defword "lit"                           , d34:d35=*
	;; ( -- x )
._lit:
    PopRsTemp
    ldy #2
    lda (temp),y
    pushA
    ldy #1
    lda (temp),y
    pushA
    incWord temp
    incWord temp
    PushRsTemp
    rts

defword "execute"                       , d35:d36=*
	;; ( xt -- )
._execute: {
    popA
    sta mod+1
    popA
    sta mod+2
    .mod jmp 0 ; SMC
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
    ;; ( num -- )
._branch0: {
    PopRsTemp
    popA
    inx : ora PS-1, x
    bne untaken
.taken:
    ldy #2 : lda (temp),y : sta mod+1
    ldy #1 : lda (temp),y
	jmp bump
.untaken:
    lda #0 : sta mod+1
    lda #2
.bump:
    clc
    adc temp
    sta temp
    .mod lda #&33 ; SMC
    adc temp+1
    sta temp+1
    ;; TODO: avoid saving back into temp. just push directly to return-stack
	PushRsTemp
    rts
    }

defword "branch"                        , d39:d40=*
    ;; ( -- )
    {
    PopRsTemp
    ldy #2 : lda (temp),y : sta mod+1
    ldy #1 : lda (temp),y
    clc
    adc temp
    sta temp
    .mod lda #&33 ; SMC
    adc temp+1
    sta temp+1
    ;; TODO: avoid saving back into temp. just push directly to return-stack
	PushRsTemp
    rts
    }

defword "ret,"                          , d40:d41=*
    ;; ( -- )
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

defword "xt->name"                      , d42:d43=*
    ;; ( xt -- string )
._xt_name:
    PsTopToTemp
    dec temp+1
    ldy #251 ; -5
    lda (temp),y
	sta PS+0, x
    ldy #252 ; -4
    lda (temp),y
	sta PS+1, x
    rts

defword "xt->next"                      , d43:d44=*
    ;; ( xt -- xt )
._xt_next:
    PsTopToTemp
    dec temp+1
    ldy #253 ; -3
    lda (temp),y
	sta PS+0, x
    ldy #254 ; -2
    lda (temp),y
	sta PS+1, x
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

defword "hidden?"                       , d45:d46=*
    ;; ( xt -- bool )
._hidden_query:
    popA
    popA
    lda #0 ;; TODO: support properly (look at the flag!)
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

defword "hidden^"                       , d47:d48=*
    ;; ( xt -- )
    stop "hidden^"
    rts

defword "entry,"                        , d48:d49=*
._entry_comma: ;; ( string -- )
    popA
    commaHereBump
    popA
    commaHereBump
    lda latestVar
    commaHereBump
    lda latestVar+1
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
    ;; ( -- xt )
._latest:
    lda latestVar+1
    pushA
    lda latestVar
    pushA
    rts

defword "key"                           , d50:d51=*
    ;; ( -- char )
._key:
    lda #0
    pushA
    jsr readChar
    pushA
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
    inx
    rts

defword "cr"                            , d57:d58=*
._cr:
    ;; ( -- )
    jmp osnewl

xdefword "cls"                           , d58:d59=*

defword "key?"                          , d59:d60=*
    stop "key?"
    rts

last = d60
.latestVar equw last

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

print "kernel size (sans dispatch table): &", STR$~(*-start)

U = 0
.dispatch_table:        ; hex 'char'
equw U                  ; 0
equw U                  ; 1
equw U                  ; 2
equw U                  ; 3
equw U                  ; 4
equw U                  ; 5
equw U                  ; 6
equw U                  ; 7
equw U                  ; 8
equw U                  ; 9
equw _nop               ; a
equw U                  ; b
equw U                  ; c
equw U                  ; d
equw U                  ; e
equw U                  ; f
equw U                  ; 10
equw U                  ; 11
equw U                  ; 12
equw U                  ; 13
equw U                  ; 14
equw U                  ; 15
equw U                  ; 16
equw U                  ; 17
equw U                  ; 18
equw U                  ; 19
equw U                  ; 1a
equw U                  ; 1b
equw U                  ; 1c
equw U                  ; 1d
equw U                  ; 1e
equw U                  ; 1f
equw _nop               ; 20 space
equw _store             ; 21 !
equw U                  ; 22 "
equw U                  ; 23 #
equw U                  ; 24 $
equw U                  ; 25 %
equw _key               ; 26 & (laptop: ^key)
equw U                  ; 27 '
equw U                  ; 28 (
equw U                  ; 29 )
equw U                  ; 2a *
equw _plus              ; 2b +
equw _comma             ; 2c ,
equw _minus             ; 2d -
equw _emit              ; 2e .
equw U                  ; 2f /
equw _zero              ; 30 0
equw _one               ; 31 1
equw U                  ; 32 2
equw U                  ; 33 3
equw U                  ; 34 4
equw U                  ; 35 5
equw U                  ; 36 6
equw U                  ; 37 7
equw U                  ; 38 8
equw U                  ; 39 9
equw _set_dispatch      ; 3a :
equw _write_ret         ; 3b ;
equw _less_than         ; 3c <
equw _equal             ; 3d =
equw _compile_comma     ; 3e >
equw _dispatch          ; 3f ?
equw _fetch             ; 40 @
equw _crash_startup     ; 41 A
equw _branch0           ; 42 B
equw _c_fetch           ; 43 C
equw _dup               ; 44 D
equw _entry_comma       ; 45 E
equw U                  ; 46 F
equw _xt_next           ; 47 G
equw _here_pointer      ; 48 H
equw _immediate_query   ; 49 I
equw _jump              ; 4a J
equw U                  ; 4b K
equw _lit               ; 4c L
equw _cr                ; 4d M
equw _xt_name           ; 4e N
equw _over              ; 4f O
equw _drop              ; 50 P
equw U                  ; 51 Q
equw U                  ; 52 R
equw U                  ; 53 S
equw U                  ; 54 T
equw U                  ; 55 U
equw _execute           ; 56 V
equw _swap              ; 57 W
equw _exit              ; 58 X
equw _hidden_query      ; 59 Y
equw _latest            ; 5a Z
equw U                  ; 5b [
equw U                  ; 5c \
equw U                  ; 5d ]
equw _key               ; 5e ^ (laptop: =key)
equw U                  ; 5f _
equw _c_comma           ; 60 `
equw U                  ; 61 a
equw U                  ; 62 b
equw U                  ; 63 c
equw U                  ; 64 d
equw U                  ; 65 e
equw U                  ; 66 f
equw U                  ; 67 g
equw U                  ; 68 h
equw U                  ; 69 i
equw U                  ; 6a j
equw U                  ; 6b k
equw U                  ; 6c l
equw U                  ; 6d m
equw U                  ; 6e n
equw U                  ; 6f o
equw U                  ; 70 p
equw U                  ; 71 q
equw U                  ; 72 r
equw U                  ; 73 s
equw U                  ; 74 t
equw U                  ; 75 u
equw U                  ; 76 v
equw U                  ; 77 w
equw U                  ; 78 x
equw U                  ; 79 y
equw U                  ; 7a z
equw U                  ; 7b {
equw U                  ; 7c |
equw U                  ; 7d }
equw U                  ; 7e ~
equw U                  ; 7f DEL

.dispatch_table_end
assert ((dispatch_table_end - dispatch_table) = 256)

print "kernel size: &", STR$~(*-start)

.here_start:
print "here_start: &", STR$~(here_start)

.embedded:
    incbin "../quarter-forth/f/quarter.q"
    incbin "../quarter-forth/f/forth.f"
    equb 0

print "embedded size: &", STR$~(*-embedded)

.end:

print "end (after embedded): &", STR$~(end)

save "Code", start, end
