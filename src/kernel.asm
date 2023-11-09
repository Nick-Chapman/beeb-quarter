Echo = FALSE ;; initial state of echo-enabled

ImmediateFlag = &40
HiddenFlag = &80

jsrOpCode = &20
rtsOpCode = &60

osrdch = &ffe0
osasci = &ffe3
osnewl = &ffe7
oswrch = &ffee
osbyte = &fff4

kernelStart = &1200
screenStart = &3000

guard &12
org &0

.embeddedPtr skip 2
.temp skip 2 ; used by _lit & elsewhere
.temp2 skip 2
.temp3wide skip 4

msgPtr = temp

num1 = temp
num2 = temp2
result = temp3wide
remainder = temp3wide ; doesn't need to be wide

org kernelStart

print "start: &", STR$~(*)
.start:
    jmp main


;;; TODO: move multiply/divide routines to their use by forth prims
.multiply: ; num1*num2 -> result
    {
    ;; A holds result+3.
    ;; At each step, num2 is shifted right.
    ;; If the least sig bit is set, we add num1 into result+2/3.
    ;; Then result is shifted rightwards (so subsequent add-in are in effect doubled)
    lda #0 ; result+3
    sta result+2
    ldx #16
.loop:
    lsr num2+1
    ror num2
    bcc shift
    tay
    clc
    lda result+2
    adc num1
    sta result+2
    tya ; result+3
    adc num1+1
.shift:
    ror a
    ror result+2
    ror result+1
    ror result
    dex
    bne loop
    sta result+3
    rts
    }

.divide:  ; num1/num2 -> result(in num1) and remainder
    {
    lda #0
    sta remainder
    sta remainder+1
    ldx #16
.loop:
    asl num1
    rol num1+1
    rol remainder
    rol remainder+1
    lda remainder
    sec
    sbc num2
    tay
    lda remainder+1
    sbc num2+1
    bcc noInc
    sta remainder+1
    sty remainder
    inc num1
.noInc:
    dex
    bne loop
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parameter Stack (PS)

;;; The parameter stack lives in zero-page growing downwards from &90.
;;; The X register indexes the stack top.
;;; For byte access we use indexed addressing: "PS+N, x".
;;; Each stack entry is 16-bits (2 bytes).
;;; The hi-byte is always deeper in the stack than the lo-byte, so:
;;; When pushing (pushA): push hi; push lo.
;;; When popping (popA): pop lo; pop hi.

;;; TODO: detect stack underflow/overflow

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
    sta SMC+1
    lda hereVar : sta temp
    lda hereVar+1 : sta temp+1
    .SMC lda #33
    sta (temp), y
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

macro todo message
    jsr osnewl
    puts "todo:"
    puts message : newline
    jmp spin
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.main: {
    tsx
    stx smc_rsp0+1
    stx smc_bye+1
    ldx #0
    copy16i embedded, embeddedPtr
    copy16i here_start, hereVar
.loop:
    jsr _key
    jsr _dispatch
    jsr _execute
    jmp loop }

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
    ldy echo_enabled
    beq no : pha : jsr writeChar : pla : .no
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
;;print NAME, ": &", STR$~(*) ;; print address of primitive words
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

defword "dispatch"                      , d0:d1=*
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

d2=d1

defword "bye"                           , d2:d3=*
    .smc_bye ldx #33
    txs
    rts

defword "crash"                         , d3:d4=*
._crash:
    newline
    puts "we have crashed!"
    newline
    jmp spin

defword "startup-is-complete"           , d4:d5=*
    lda #1
    sta is_startup_complete
    rts

defword "crash-only-during-startup"     , d5:d6=*
._crash_startup: {
    lda is_startup_complete
    beq _crash
    rts
    }

defword "sp"                            , d6:d7=*
    txa
    clc : adc #PS
    dex : dex
    sta PS+0, x
    lda #0
    sta PS+1, x
    rts

defword "sp0"                           , d7:d8=*
    lda #0
    pushA
    lda #PS
    pushA
    rts

defword "rsp"                           , d8:d9=*
    lda #0
    pushA
    stx SMC_x +1
    tsx
    stx SMC_sp +1
    .SMC_x ldx #&33
    .SMC_sp lda #&44
    pushA
    rts

defword "rsp0"                          , d9:d10=*
    lda #0
    pushA
    .smc_rsp0 lda #33
    pushA
    rts

defword "as-num"                        , d10:d11=*
    rts

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
    ;; ( x -- )
    PopRsTemp
    lda PS+1, x
    pha
    lda PS+0, x
    pha
    inx : inx
    PushRsTemp
    rts

defword "r>"                            , d16:d17=*
    ;; ( -- x )
    PopRsTemp
    dex : dex
    pla
    sta PS+0, x
    pla
    sta PS+1, x
    PushRsTemp
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

defword "/2"                            , d20:d21=*
    ;; ( num -- num )
    lda PS+1, x
    lsr a
    sta PS+1, x
    lda PS, x
    ror a
    sta PS, x
    rts

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

defword "*"                             , d23:d24=*
    jsr _um_star
    jsr _swap
    jsr _drop
    rts

;;; TODO: expose as "um*" -- unsigned multiple with double-width result
._um_star:
    ;; ( p q -- p*q[hi] p*q[lo] )
    lda PS+3, x
    sta num1+1
    lda PS+2, x
    sta num1
    lda PS+1, x
    sta num2+1
    lda PS+0, x
    sta num2
    txa : pha
    jsr multiply
    pla : tax
    lda result+3
    sta PS+3, x
    lda result+2
    sta PS+2, x
    lda result+1
    sta PS+1, x
    lda result+0
    sta PS+0, x
    rts

defword "/mod"                          , d24:d25=*
._div_mod:
    ;; ( p q -- p%q p/q )
    lda PS+3, x
    sta num1+1
    lda PS+2, x
    sta num1
    lda PS+1, x
    sta num2+1
    lda PS+0, x
    sta num2
    txa : pha
    jsr divide
    pla : tax
    lda remainder+1
    sta PS+3, x
    lda remainder
    sta PS+2, x
    lda num1+1
    sta PS+1, x
    lda num1
    sta PS+0, x
    rts

;; signed comparison
defword "<"                             , d25:d26=*
    ;; ( numP numQ -- bool )
._less_than: {
    ;; unsigned 16bit comparison by subtraction like example 4.3 of compare-beyond
    lda PS+2, x ; PL
    cmp PS+0, x ; QL
    lda PS+3, x ; PH
    sbc PS+1, x ; QH

    ;; sign hack (like example 6.1)
    bvc noInvert : eor #$80 : .noInvert
    bpl notLess
    ;;bcs notLess ; unsigned

.less:
    lda #&ff ; true
    jmp store
.notLess:
    lda #0 ; false
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
    popA
    sta temp
    popA
    sta temp+1
    popA
    ldy #0
    sta (temp),y
    popA
    ldy #1
    sta (temp),y
    rts

defword "c@"                            , d29:d30=*
    ;; ( addr -- char )
._c_fetch:
    PsTopToTemp
    ldy #0
    lda (temp),y
    sta PS+0, x ; lo-value
    sty PS+1, x ; hi-value (Y conveniently contains 0) -- This store is the only diff from fetch
    rts

defword "c!"                            , d30:d31=*
    ;; ( char addr -- )
    popA
    sta temp
    popA
    sta temp+1
    popA
    ldy #0
    sta (temp),y
    inx
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

defword "immediate?"                    , d44:d45=*
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
    PsTopToTemp
    dec temp+1 ; back 256 bytes ; to allow negative acess
    ldy #255 ; -1
    lda (temp),y
    and #HiddenFlag
    ;; y is conveniently &ff(true). incrementing will make it false
    bne yes
    iny
.yes:
    ; Y is 00/ff, which if double stored, is false/true
    sty PS+0, x ; lo
    sty PS+1, x ; hi
    rts

defword "immediate^"                    , d46:d47=*
    ;; ( xt -- )
    PsTopToTemp
    dec temp+1 ; back 256 bytes ; to allow negative acess
    ldy #255 ; -1
    lda (temp),y
    eor #ImmediateFlag
    sta (temp),y
    inx : inx
    rts

defword "hidden^"                       , d47:d48=*
    ;; ( xt -- )
    PsTopToTemp
    dec temp+1 ; back 256 bytes ; to allow negative acess
    ldy #255 ; -1
    lda (temp),y
    eor #HiddenFlag
    sta (temp),y
    inx : inx
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
._key:
    ;; ( -- char )
    jmp (key_indirection)

._key0:
    lda #0
    pushA
    jsr readChar
    pushA
    rts

defword "set-key"                       , d51:d52=*
    ;; ( xt -- )
    popA
    sta key_indirection
    popA
    sta key_indirection+1
    rts

defword "get-key"                       , d52:d53=*
    ;; ( -- xt )
    lda key_indirection+1
    pushA
    lda key_indirection
    pushA
    rts

defword "echo-enabled"                  , d53:d54=*
    ;; ( -- addr )
    lda #HI(echo_enabled)
    pushA
    lda #LO(echo_enabled)
    pushA
    rts

defword "echo-off"                      , d54:d55=*
    ;; ( -- )
    lda #0
    sta echo_enabled
    rts

defword "echo-on"                       , d55:d56=*
    ;; ( -- )
    lda #1
    sta echo_enabled
    rts

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

defword "time"                          , d58:d59=*
    ;; ( -- u u )
    lda #0 ;; TODO : real impl
    pushA
    pushA
    pushA
    pushA
    rts

defword "key?"                          , d59:d60=*
    todo "key?"
    rts

;;; make "fx" call to MOS, using "osbyte", passing arguments in A/X/Y
defword "fx"                            , d60:d61=*
    ;; ( a x y -- )
    {
    ldy PS+0, x ; Y
    lda PS+2, x ; X
    sta SMC_newX +1
    lda PS+4, x ; A
    inx : inx : inx : inx : inx : inx
    stx SMC_oldX +1
    .SMC_newX ldx #&44
    jsr osbyte
    .SMC_oldX ldx #&55
    rts
    }

defword "mode"                            , d61:d62=*
    ;; ( n -- )
    lda #22 : jsr oswrch
    lda PS, x : jsr oswrch
    inx
    rts

last = d62

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The restorable heap image starts here
;;; TODO: goal is for the heap to be replaced by an offline compiled image

.heap:

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

;;; variables which contained in the main heap

.hereVar equw here_start
.key_indirection equw _key0
.latestVar equw last
.echo_enabled equw Echo ; controls echo in readChar
.is_startup_complete equb 0 ; controls crash-only-during-startup

.here_start:

print "heap:  &", STR$~(heap)
print "here:  &", STR$~(*)

.embedded:
    incbin FORTH
    equb 0

.end:

print "embed: &", STR$~(*)

save "Code", start, end
