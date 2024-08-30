; vim: ft=nyasm

\include "snes.inc"

\section "CORE"
\native \index16 \accum8

;; A zeroed word in ROM used as a source for mem-clearing DMAs
memConstZero:: \word $0000

MemInit::
    ldx #|__WRAM0_START__
    stx WMADDL
    lda #^__WRAM0_START__
    sta WMADDH

    ; Fixed address ROM->WRAM DMA Mode
    lda #$08
    sta DMAP0

    ; Destination: WRAM
    lda #<WMDATA
    sta BBAD0

    ; Source: memConstZero
    ldx #|memConstZero
    stx A1T0L
    lda #^memConstZero
    sta A1B0

    ; Clear WRAM0
    ldx #|__WRAM0_SIZE__
    stx DAS0L
    lda #1
    sta MDMAEN

    ; Clear WRAM1
    ldx #|__WRAM1_SIZE__
    stx DAS0L
    lda #1
    sta MDMAEN

    rts
