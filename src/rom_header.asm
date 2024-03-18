; vim: ft=pasm

?section "INES_HEADER"

HDR_PRG_ROM_SIZE    = 2     ; 32k - n * 16k
HDR_CHR_ROM_SIZE    = 0     ; 0k - n * 8k
HDR_MAPPER_ID       = 4     ; TxROM
HDR_SUBMAPPER_ID    = 0
HDR_FOUR_SCREEN     = 1
HDR_MIRRORING       = 0
HDR_BATTERY         = 1
HDR_PRG_NVRAM_SIZE  = 7     ; 8k - 64 * 2^n bytes
HDR_PRG_RAM_SIZE    = 0
HDR_CHR_NVRAM_SIZE  = 0
HDR_CHR_RAM_SIZE    = 7     ; 8k - 64 * 2^n bytes
HDR_REGION          = 0     ; ntsc

?byte "NES", $1A
?byte HDR_PRG_ROM_SIZE
?byte HDR_CHR_ROM_SIZE
?byte ((HDR_MAPPER_ID & $0F) << 4) | (HDR_FOUR_SCREEN << 3) | (HDR_BATTERY << 1) | HDR_MIRRORING
?byte (HDR_MAPPER_ID & $F0) | %1000
?byte (HDR_SUBMAPPER_ID << 4) | ((HDR_MAPPER_ID & $F00) >> 8)
?byte ((HDR_PRG_ROM_SIZE & $F00) >> 4) | ((HDR_CHR_ROM_SIZE & $F00) >> 8)
?byte (HDR_PRG_NVRAM_SIZE << 4) | HDR_PRG_RAM_SIZE
?byte (HDR_CHR_NVRAM_SIZE << 4) | HDR_CHR_RAM_SIZE
?byte HDR_REGION
?byte 0, 0, 0
