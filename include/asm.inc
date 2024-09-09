; vim: ft=nyasm
\ifndef _ASM_INC
_ASM_INC = 1

\macro SET_A8
    \accum8
    sep #$20
\end

\macro SET_I16
    \index16
    rep #$10
\end

\macro SET_A8I16
    SET_A8
    SET_I16
\end

\macro SET_A16I16
    \accum16 \index16
    rep #$30
\end

\end ; _ASM_INC
