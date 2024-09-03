; vim: ft=nyasm

\include "asm.inc"
\include "snes.inc"
\include "color.inc"

\section "CORE"
\native \index16 \accum8

;; Zero byte in ROM used for zeroing DMAs
startConstZero:: \byte $00

StartReset::
    sei

    ; Set native, index16, and accum8
    clc
    xce
    SET_A8I16

    ; DP=$0000
    pea $0000
    pld
    ; DB=$80
    lda #$80
    pha
    plb

    ; Make sure no crazy IRQs happen
    stz NMITIMEN
    stz HDMAEN

    ; Setup stack
    ldx #$02FF
    txs

    ; Enable FastROM
    lda #ROMSEL_FASTROM
    sta ROMSEL

    ; Clear WRAMs
    ; A Bus
    ldx #`startConstZero
    stx A1T0L
    lda #^startConstZero
    sta A1B0
    ; CONST->WRAM
    lda #(DMAP_PATTERN_0 | DMAP_ABUS_FIXED | DMAP_A2B)
    sta DMAP0
    \loop n, 2
        ; B Bus: Start of WRAMn
        lda #<WMDATA
        sta BBAD0
        stz WMADDL
        stz WMADDM
        lda #n
        sta WMADDH
        ; Length: 64 KiB
        stz DAS0L
        stz DAS0H
        ; Execute
        lda #1
        sta MDMAEN
    \end

    jsr GfxInit
    jsr TimeInit
    jsr JoyInit
    jsr ActInit

    lda #1
    sta BGMODE

    ; Enable NMI
    lda |`timeShadowNMITIMEN
    ora #NMITIMEN_NMI_ENABLE
    sta |`timeShadowNMITIMEN
    sta NMITIMEN

    lda #(OBJSEL_16X16_32X32 | OBJSEL_BANK0_0000 | OBJSEL_BANK1_0000)
    sta OBJSEL
    lda #TM_OBJ
    sta TM

    lda #$81
    sta CGADD
    lda #<COLOR_RGB24 $FF, $00, $00
    sta CGDATA
    lda #>COLOR_RGB24 $FF, $00, $00
    sta CGDATA

    lda #32
    sta |`(gfxOamShadow+OAMOBJ.X)
    sta |`(gfxOamShadow+OAMOBJ.Y)

    lda #(INIDISP_FULLBRIGHT | INIDISP_ENABLE)
    sta INIDISP

.MainLoop:
    ; Wait for VBlank
    bit <gfxNmiFlags
    bmi .MainLoop

    ; TODO

    lda <gfxNmiFlags
    ora #$80
    sta <gfxNmiFlags
    bra .MainLoop

Sprite:
    \byte %0000_0001
    \byte %0000_0000
