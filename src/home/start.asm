; vim: ft=nyasm

\include "registers.inc"

\native \index16 \accum8

\section "HOME"
Start::
    sei
    cld

    ; set native, index16, and accum8
    clc
    xce
    rep #$10
    sep #$20

    ldx #$0100
    txs

    pea 0
    pld     ; DP to $0000
    lda #$80
    pha
    plb     ; DBR = $80

    lda #$8F
    sta INIDISP

    jmp *

StartNmi::
    rti

StartIrq::
    rti
