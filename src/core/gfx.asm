; vim: ft=nyasm

\include "snes.inc"

\section "ZEROPAGE"

;; NMI Flags
;;
;; Fields: r.......
;; r - Ready to transfer to VRAM
;;
gfxNmiFlags: \res 1

\section "CORE"
\native \index16 \accum8

GfxInit::
    ; Force vblank, full brightness
    lda #$8F
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

    lda #$30
    sta CGWSEL
    stz CGADSUB
    lda #$E0
    sta COLDATA

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

    ; Do VRAM transfers here

    lda <gfxNmiFlags
    ora #$80
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

