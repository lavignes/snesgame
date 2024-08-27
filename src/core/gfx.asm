; vim: ft=nyasm

\include "snes.inc"

\section "ZEROPAGE"

gfxNmiFlags: \res 1

\section "CORE"
\native \index16 \accum8

GfxInit::
    ; Disable Display
    lda #$8F
    sta INIDISP

    ; TODO: clear registers to initial state

    rts

GfxNmi::
    rti

