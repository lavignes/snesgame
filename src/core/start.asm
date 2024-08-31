; vim: ft=nyasm

\include "snes.inc"

\section "CORE"
\native \index16 \accum8

StartReset::
    sei
    cld

    ; Set native, index16, and accum8
    clc
    xce
    SET_A8I16

    ; Setup stack
    ldx #$02FF
    txs

    ; DP=$0000
    pea $0000
    pld
    ; DB=$80
    lda #$80
    pha
    plb

    ; Make sure no crazy interrupts happen
    stz NMITIMEN
    stz HDMAEN

    ; Clear LoRAM really inefficiently
    ; We need to at least clear the stack before JSRing
    ldx #|__WRAM0_START__
.ClearLoRAM:
    dex
    stz |$0000,X
    bne .ClearLoRAM

    jsr GfxInit
    jsr MemInit
    jsr JoyInit
    jsr TimeInit

    ; BG Mode 1
    lda #%0000_1_001
    sta BGMODE

    ; Enable Display
    lda #$0F
    sta INIDISP

    bra *
