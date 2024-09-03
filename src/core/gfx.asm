; vim: ft=nyasm

\include "asm.inc"
\include "snes.inc"

\section "ZEROPAGE"

;; NMI Flags
;;
;; Fields: r.......
;; r - Ready to transfer to VRAM
;;
gfxNmiFlags:: \res 1

\section "LORAM"

gfxOamShadow:: \res 544

\section "CORE"
\native \index16 \accum8

GfxInit::
    lda #(INIDISP_FULLBRIGHT | INIDISP_DISABLE)
    sta INIDISP

    stz OBJSEL
    stz OAMADDL
    stz OAMADDH

    stz BGMODE
    stz MOSAIC
    stz BG1SC
    stz BG2SC
    stz BG3SC
    stz BG4SC
    stz BG12NBA
    stz BG34NBA

    stz SETINI

    ; HScroll=0 VScroll=-1
    lda #<-1
    stz BG1HOFS
    stz BG1HOFS
    sta BG1VOFS
    sta BG1VOFS
    stz BG2HOFS
    stz BG2HOFS
    sta BG2VOFS
    sta BG2VOFS
    stz BG3HOFS
    stz BG3HOFS
    sta BG3VOFS
    sta BG3VOFS
    stz BG4HOFS
    stz BG4HOFS
    sta BG4VOFS
    sta BG4VOFS

    stz W12SEL
    stz W34SEL
    stz WOBJSEL
    stz WH0
    stz WH1
    stz WH2
    stz WH3
    stz WBGLOG
    stz WOBJLOG

    stz TM
    stz TS
    stz TMW
    stz TSW

    ; TODO: magic numbers
    lda #$30
    sta CGWSEL
    stz CGADSUB
    lda #$E0
    sta COLDATA

    jsr GfxVramClear
    jsr GfxCgramClear ; TODO: xfer shadow cgram? Do we need shadow cgram?
    ; Safe to assume WRAM is clear
    jsr GfxOamTransfer

    rts

GfxVramClear:
    ; A Bus
    ldx #`startConstZero
    stx A1T0L
    lda #^startConstZero
    sta A1B0
    ; CONST->VRAM
    lda #(DMAP_PATTERN_01 | DMAP_ABUS_FIXED | DMAP_A2B)
    sta DMAP0
    ; B Bus: Start of VRAM
    lda #<VMDATAL
    sta BBAD0
    ; Increment VMADD every word
    lda #(VMAIN_INCREMENT_HI)
    sta VMAIN
    stz VMADDL
    stz VMADDH
    ; Length: 64 KiB
    stz DAS0L
    stz DAS0H
    ; Execute
    lda #1
    sta MDMAEN
    rts

GfxCgramClear:
    ; A Bus
    ldx #`startConstZero
    stx A1T0L
    lda #^startConstZero
    sta A1B0
    ; CONST->CGRAM
    lda #(DMAP_PATTERN_00 | DMAP_ABUS_FIXED | DMAP_A2B)
    sta DMAP0
    ; B Bus: Start of CGRAM
    lda #<CGDATA
    sta BBAD0
    stz CGADD
    ; Length: 512 Bytes
    ldx #512
    stx DAS0L
    ; Execute
    lda #1
    sta MDMAEN
    rts

GfxOamTransfer:
    ; A Bus
    ldx #`gfxOamShadow
    stx A1T0L
    lda #^gfxOamShadow
    sta A1B0
    ; SHADOW->OAM
    lda #(DMAP_PATTERN_00 | DMAP_ABUS_INCREMENT | DMAP_A2B)
    sta DMAP0
    ; B Bus: Start of OAM
    lda #<OAMDATA
    sta BBAD0
    stz OAMADDL
    stz OAMADDH
    ; Length: 544 Bytes
    ldx #544
    stx DAS0L
    ; Execute
    lda #1
    sta MDMAEN
    rts

GfxNmi::
    phb
    phd

    SET_A8I16
    pha
    xba
    pha

    ; DP=$0000
    pea $0000
    pld
    ; DB=$80
    lda #$80
    pha
    plb

    ; ACK NMI
    bit RDNMI

    ; Can we write VRAM?
    bit <gfxNmiFlags
    bpl .Return

    phx
    phy

    ; Cancel any spurious DMAs
    ; TODO: Debug break if this is happening?
    stz MDMAEN

    ; Do VRAM transfers here
    jsr GfxOamTransfer

    lda <gfxNmiFlags
    eor #$80
    sta <gfxNmiFlags

    ply
    plx

.Return:
    pla
    xba
    pla
    pld
    plb
    rti

