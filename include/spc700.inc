; vim: ft=nyasm
\ifndef _SPC700_INC
_SPC700_INC = 1

; TODO: I will probably just make a SPC700 assembler inside nyasm

\macro MOVA_IMM
    \byte $E8, \1
\end

\macro MOVA_IX
    \byte $E6
\end

\macro MOVA_IXPLUS
    \byte $BF
\end

\end ; _SPC700_INC
