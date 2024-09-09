; vim: ft=nyasm

\include "asm.inc"
\include "snes.inc"

\section "CORE"
\native \index16 \accum8

Lz4Init::
    ; To speed up DMAs, we reserve DMA channel 7 and keep it
    ; pre-configured for use in decompressing to WRAM1

    ; ROM->WRAM
    lda #(DMAP_PATTERN_00 | DMAP_ABUS_INCREMENT | DMAP_A2B)
    sta DMAP7
    ; B Bus: Start of WRAM1
    lda #<WMDATA
    sta BBAD7
    rts

;; Deflate Lz4 frames from AX to WRAM1:Y
Lz4Flate::
    phb
    pha

    ; Prepare the DMA source bank. The bank won't change so this is a good
    ; time to stash it.
    sta A1B0

    ; Make sure we're pointed to WRAM1
    ; TODO: I could probaly use carry to pass this bit and decompress into WRAM0
    sty WMADDL
    lda #1
    sta WMADDH

    ; Move the DB to source bank so we can start reading LZ4 frames there
    plb

; LZ4 format is simple. It's a stream of frames with this format
; [
;   "token" byte,
;   "L" literal length bytes (optional),
;   literal bytes (optional),
;   offset word,
;   "M" match length bytes (optional)
; ]
; The token is a byte that encodes two nibble fields:
;   * The Hi nibble is the literal length (L) 0-15.
;   * The Lo nibble is the match length (M) 0-15.
;
; If L is 15, we keep reading bytes and adding
; them to L. When a byte is < 255 it is the last byte to add to L.
;
; Then follows are L literal bytes. These are DMA'd directly to WRAM.
;
; Afterwards is the offset word. This is used to source literal data
; previously ocurring in the stream. It is subtracted from the current
; stream position for the next DMA source.
; NOTE: If the offset is zero, then this is the final frame no more
; bytes will be present in the stream.
;
; Finally, like L, if M is 15 it will sum the final bytes in the frame to
; form the final 16-bit M. NOTE: Unlike L, we add 4 to M at the end.
; So M will always represent 4 to 19 inclusive. And we always perform
; the DMA of M bytes at the offset to WRAM.

.NextToken:
    lda $0000,X
    inx
    ; Store token copy on stack
    pha
    ; Upper nibble is literal length
    lsr
    lsr
    lsr
    lsr
    cmp #$0F
    bne .LiteralLenInY
    jsr Lz4ReadLenBytes
.LiteralLenInY:
    ; TODO: Annoyingly we cant use stx or sty long addresses
    SET_A16I16
    ; Length: Y
    tya
    beq .SkipLiteral ; Don't DMA zero bytes
    sta @DAS7L
    ; Src: X
    txa
    sta @A1T7L

    ; NOTE: We stay in A16I16 mode here on purpose!
    ;       Its safe to write $0001 to MDMAEN

    ; Copy literal into WRAM1
    lda #MDMAEN_7
    sta @MDMAEN

.SkipLiteral:
    ; We're going to read the offset word next, and subtract it from X
    clc
    txa
    sbc $0000,X
    ; Src: X-offset
    sta @A1T7L
    SET_A8

    ; Restore token
    pla

    ; Exit case, if offset was 0
    ldy $0000,X
    beq .Return

    inx
    inx

    ; Lower nibble is match length
    and #$0F
    tay
    clc
    adc #4 ; Always add 4 to the match length matches must be at least 4 long
    cmp #$13
    bne .MatchLenInY
    jsr Lz4ReadLenBytes
.MatchLenInY:
    SET_A16I16
    ; Length: Y
    tya
    sta @DAS7L
    SET_A8

    lda #MDMAEN_7
    sta @MDMAEN
    bra .NextToken

.Return:
    plb
    rts

; TODO: I think I can make this a lot faster by keeping the length in A16
; and never going out to zero-page. We can also pretty much stay in A16
; the entirety of the LZ4 loop
Lz4ReadLenBytes:
    sty <cnt
.Loop:
    ; Need to keep reading bytes to compute length
    lda $0000,X
    inx
    cmp #$FF
    bne .AddAndReturn

    clc
    adc <cnt+0
    sta <cnt+0
    bcc .NoCarry0
    inc <cnt+1
.NoCarry0:
    bra .Loop

.AddAndReturn:
    clc
    adc <cnt+0
    sta <cnt+0
    bcc .NoCarry1
    inc <cnt+1
.NoCarry1:
    ldy <cnt
    rts
