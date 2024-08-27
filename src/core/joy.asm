; vim: ft=nyasm

\include "snes.inc"

\section "ZEROPAGE"

joyPressed:: \res 2
joyReleased:: \res 2
joyHeld:: \res 2

\section "CORE"
\native \index16 \accum8

JoyUpdate::
    ; If auto-read is still running, wait
    lda #1
    bit HVBJOY
    beq JoyUpdate

    rts
