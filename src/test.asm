; vim: ft=nyasm

\native \index16 \accum8

\section "HEADER"
\byte "SNES GAME            "
\byte $35       ; Fast ExHiROM
\byte $02       ; ROM + SRAM + Battery
\byte 13        ; 2^13 KiB ROM (8 MiB)
\byte 5         ; 2^5 KiB SRAM (32 KiB)
\byte 1         ; NTSC
\byte 0         ; Developer ID
\byte 0         ; ROM Version
\word $FFFF     ; Checksum xor $FFFF
\word 0         ; Checksum

\section "HOME"
Start:
    ; set native, index16, and accum8
    clc
    xce
    rep #$10
    sep #$20

    jmp *

\section "VECTOR_LONGJUMP"
Reset:
    jml @Start

Nmi:
    rti

Irq:
    rti

\section "VECTORS"
; Native Mode Vectors
\word 0, 0          ; (reserved)
\word $FFFF & Reset ; COP
\word $FFFF & Reset ; BRK
\word 0             ; ABORT (unused)
\word $FFFF & Nmi   ; NMI
\word 0             ; (reserved)
\word $FFFF & Irq   ; IRQ

; Emulation Mode Vectors
\word 0, 0          ; (reserved)
\word $FFFF & Reset ; COP
\word 0             ; (reserved)
\word 0             ; ABORT (unused)
\word $FFFF & Nmi   ; SMI
\word $FFFF & Reset ; RESET
\word $FFFF & Irq   ; IRQ

