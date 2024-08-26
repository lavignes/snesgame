; vim: ft=nyasm
\ifndef _COLOR_INC
_COLOR_INC = 1

\macro COLOR_RGB24TO15
    (((\3 & $F8) << 7) | ((\2 & $F8) << 2) | ((\1 >> 3) & $1F))
\end

COLOR_BLACK = COLOR_RGB24TO15 $00, $00, $00

\end ; _COLOR_INC
