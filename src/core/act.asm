; vim: ft=nyasm

\include "act.inc"

\section "LORAM"

actCount: \res 1

actThink:   \res ACT_MAX
actState:   \res ACT_MAX
actXL:      \res ACT_MAX
actXH:      \res ACT_MAX
actYL:      \res ACT_MAX
actYH:      \res ACT_MAX
actZ:       \res ACT_MAX
actSize:    \res ACT_MAX
actTimer0:  \res ACT_MAX
actTimer1:  \res ACT_MAX
actFlag0:   \res ACT_MAX
actFlag1:   \res ACT_MAX

\section "CORE"
\native \index16 \accum8

ActInit::
    rts
