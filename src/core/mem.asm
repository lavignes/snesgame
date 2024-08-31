; vim: ft=nyasm

\include "snes.inc"

\section "CORE"
\native \index16 \accum8

;; A zeroed byte in ROM used as a source for mem-clearing DMAs
memConstZero: \byte $00

MemInit::
    ; Enable FastROM
    lda #1
    sta ROMSEL

    ; DMA Source: memConstZero
    ldx #|memConstZero
    stx A1T0L
    lda #^memConstZero
    sta A1B0

    ; Fixed address ROM->WRAM DMA Mode
    lda #$08
    sta DMAP0

    ; Destination: WRAM
    lda #<WMDATA
    sta BBAD0

    ; Point to just after LoRAM
    ldx #|__WRAM0_START__
    stx WMADDL
    lda #^__WRAM0_START__
    sta WMADDH

    ; Clear WRAM0
    ldx #|($010000 - |__WRAM0_START__)
    stx DAS0L
    lda #1
    sta MDMAEN

    ; Clear all 64KiB WRAM1
    ldx #0
    stx DAS0L
    lda #1
    sta MDMAEN

    ; Fixed address ROM->VRAM DMA Mode
    lda #$09
    sta DMAP0

    ; Increment VMADD every word
    lda #$80
    sta VMAIN

    ; Destination: VRAM
    lda #<VMDATAL
    sta BBAD0

    ; Point to Start of VRAM
    stz VMADDL
    stz VMADDH

    ; Clear all 64KiB VRAM
    ldx #0
    stx DAS0L
    lda #1
    sta MDMAEN

    ; Fixed address ROM->CGRAM DMA Mode
    lda #$0A
    sta DMAP0

    ; Destination: CGRAM
    lda #<CGDATA
    sta BBAD0

    ; Point to Start of CGRAM
    stz CGADD

    ; Clear all 512 Bytes of CGRAM
    ldx #512
    stx DAS0L
    lda #1
    sta MDMAEN

    ; Fixed address ROM->CGRAM DMA Mode
    lda #$0A
    sta DMAP0

    ; Destination: CGRAM
    lda #<CGDATA
    sta BBAD0

    ; Point to Start of CGRAM
    stz CGADD

    ; Clear all 512 Bytes of CGRAM
    ldx #512
    stx DAS0L
    lda #1
    sta MDMAEN

    rts
