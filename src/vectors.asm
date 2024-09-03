; vim: ft=nyasm

\native

\section "VECTORS"
; Native Mode Vectors
\word 0, 0      ; (reserved)
\word `Break    ; COP
\word `Break    ; BRK
\word 0         ; ABORT (unused)
\word `Nmi      ; NMI
\word 0         ; (reserved)
\word `Irq      ; IRQ

; Emulation Mode Vectors
\word 0, 0      ; (reserved)
\word `Break    ; COP
\word 0         ; (reserved)
\word 0         ; ABORT (unused)
\word `Nmi      ; NMI
\word `Reset    ; RESET
\word `Irq      ; IRQ

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
