; vim: ft=nyasm

\include "snes.inc"
\include "color.inc"
\include "joy.inc"

\section "CORE"
\native \index16 \accum8

Start::
    sei
    cld

    ; Set native, index16, and accum8
    clc
    xce
    rep #$10
    sep #$20

    ; Enable FastROM
    lda #1
    sta ROMSEL

    ; Setup stack
    ldx #$02FF
    txs

    pea $0000
    pld     ; DP to $0000
    lda #$80
    pha
    plb     ; DBR = #$80

    ; Disable Display
    lda #$8F
    sta INIDISP

    ; BG Mode 1
    lda #%0000_1_001
    sta BGMODE

    lda <joyHeld

    ; Enable Display
    lda #$0F
    sta INIDISP

    jmp *

StartNmi::
    rti

StartIrq::
    rti
