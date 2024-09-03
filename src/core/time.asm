; vim: ft=nyasm

\section "LORAM"

timeShadowNMITIMEN:: \res 1

\section "CORE"
\native \index16 \accum8

TimeInit::
    rts

TimeIrq::
    rti

