; vim: ft=nyasm

\native \index16 \accum8

\section "HEADER"
\byte "YO"
\byte "GAME"
\byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
\byte "SNES GAME            "
\byte $35       ; Fast ExHiROM
\byte $02       ; ROM + SRAM + Battery
\byte 13        ; 2^13 KiB ROM (8 MiB)
\byte 5         ; 2^5 KiB SRAM (32 KiB)
\byte 1         ; USA
\byte $33       ; (extended header)
\byte 0         ; ROM Version
\word $0000     ; Checksum Complement
\word $FFFF     ; Checksum

\section "HOME"
Start:
    ; set native, index16, and accum8
    clc
    xce
    rep #$10
    sep #$20
    cld
    sei

    ldx #$000200

    jsr Foo

    jmp *

\section "TEST"
Foo:
    rts

\section "VECTORJUMP"
Reset:
    jml @Start

Nmi:
    rti

Irq:
    rti

\macro WORD
    $FFFF & \1
\end

\section "VECTORS"
; Native Mode Vectors
\word 0, 0          ; (reserved)
\word WORD Reset    ; COP
\word WORD Reset    ; BRK
\word 0             ; ABORT (unused)
\word WORD Nmi      ; NMI
\word 0             ; (reserved)
\word WORD Irq      ; IRQ

; Emulation Mode Vectors
\word 0, 0          ; (reserved)
\word WORD Reset    ; COP
\word 0             ; (reserved)
\word 0             ; ABORT (unused)
\word WORD Nmi      ; SMI
\word WORD Reset    ; RESET
\word WORD Irq      ; IRQ

