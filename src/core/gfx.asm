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
    ; Disable Display
    lda #$8F
    sta INIDISP

    ; TODO: clear registers to initial state
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

