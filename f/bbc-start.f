
echo-off

cr
.." Welcome to Quarter Forth on the BBC." cr
cr
mem cr
( .." here:&" here .hex cr )
.?stack

caps-off
disable-auto-repeat ( so we type even at 10x emulation speed )
disable-cursor-edit
disable-escape

startup-is-complete

hide startup-is-complete
hide crash-only-during-startup
echo-on
