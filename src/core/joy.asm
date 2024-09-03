; vim: ft=nyasm

\include "asm.inc"
\include "snes.inc"

\section "ZEROPAGE"

joyPressed:: \res 2
joyReleased:: \res 2
joyHeld:: \res 2

\section "CORE"
\native \index16 \accum8

JoyInit::
    lda |`timeShadowNMITIMEN
    ora #NMITIMEN_JOY_AUTOREAD
    sta |`timeShadowNMITIMEN
    sta NMITIMEN

    rts

JoyUpdate::
    ; If auto-read is still running, wait
    lda #1
.Wait:
    bit HVBJOY
    beq .Wait

    SET_A16I16
    ; Compute held by AND with last pressed
    lda JOY1L
    tax
    and <joyPressed
    sta <joyHeld
    ; Compute released by AND with complement of pressed
    txa
    eor #$FFFF
    and <joyPressed
    sta <joyReleased
    ; Compute new pressed by XOR with last pressed
    txa
    eor <joyPressed
    sta <joyPressed
    SET_A8

    rts
