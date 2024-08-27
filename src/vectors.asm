; vim: ft=nyasm

\native

\section "VECTORJUMP"
; Because our vectors are in slow rom, we need to jump to fast rom
Reset:
    jml @StartReset
Nmi:
    jml @GfxNmi
Irq:
    jml @TimeIrq
Break:
    jml @DbgBrk

\section "VECTORS"
; Native Mode Vectors
\word 0, 0          ; (reserved)
\word $FFFF&Break   ; COP
\word $FFFF&Break   ; BRK
\word 0             ; ABORT (unused)
\word $FFFF&Nmi     ; NMI
\word 0             ; (reserved)
\word $FFFF&Irq     ; IRQ

; Emulation Mode Vectors
\word 0, 0          ; (reserved)
\word $FFFF&Break   ; COP
\word 0             ; (reserved)
\word 0             ; ABORT (unused)
\word $FFFF&Nmi     ; NMI
\word $FFFF&Reset   ; RESET
\word $FFFF&Irq     ; IRQ

