
echo-off
( cls )

cr
.." Welcome to Quarter Forth on the BBC." cr
cr

( mem cr ) ( Number makes no sense for BBC. )
.?stack

caps-off
disable-auto-repeat ( so we type even at 10x emulation speed )
disable-cursor-edit

startup-is-complete

hide startup-is-complete
hide crash-only-during-startup
echo-on
