; vim: ft=nyasm

\include "joy.inc"

\section "ZEROPAGE"

joyPressed:: \res 2
joyReleased:: \res 2
joyHeld:: \res 2

\section "CORE"
\native \index16 \accum8

