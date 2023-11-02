
: disable-cursor-edit  4 1 0 fx ;
: enable-cursor-edit   4 0 0 fx ;

(
OSBYTE &CA (202) *FX 202
<NEW VALUE>=(<OLD VALUE> AND Y) EOR X
bit 4-0 if CAPS LOCK is engaged.
)

: caps-on   202  0 239 fx ; ( unset bit 4 )
: caps-off  202 16 239 fx ; ( set bit 4 )
