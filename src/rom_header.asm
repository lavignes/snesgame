; vim: ft=nyasm

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

