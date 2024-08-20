; vim: ft=nyasm

\native

\section "VECTORJUMP"
; Because our vectors are in slow rom, we need to jump to fast rom
Reset:
    jml @Start
Nmi:
    jml @StartNmi
Irq:
    jml @StartIrq

\section "VECTORS"
; Native Mode Vectors
\word 0, 0          ; (reserved)
\word $FFFF&Reset   ; COP
\word $FFFF&Reset   ; BRK
\word 0             ; ABORT (unused)
\word $FFFF&Nmi     ; NMI
\word 0             ; (reserved)
\word $FFFF&Irq     ; IRQ

; Emulation Mode Vectors
\word 0, 0          ; (reserved)
\word $FFFF&Reset   ; COP
\word 0             ; (reserved)
\word 0             ; ABORT (unused)
\word $FFFF&Nmi     ; SMI
\word $FFFF&Reset   ; RESET
\word $FFFF&Irq     ; IRQ

