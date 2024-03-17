; vim: ft=pasm

@section "INES_HEADER"

HDR_PRG_ROM_BANKS equ 2     ; 2 - 16k banks
HDR_CHR_ROM_BANKS equ 0     ; 0 - 8k banks
HDR_MAPPER_ID equ 30        ; unrom 512
HDR_SUBMAPPER_ID equ 0
HDR_FOUR_SCREEN equ 1
HDR_MIRRORING equ 0
HDR_BATTERY equ 0
HDR_PRG_NVRAM_SIZE equ 0
HDR_PRG_RAM_SIZE equ 0
HDR_CHR_NVRAM_SIZE equ 0
HDR_CHR_RAM_SIZE equ 9      ; 32k - 64 * 2^n bytes
HDR_REGION equ 0            ; ntsc

@byte "NES", $1A
@byte HDR_PRG_ROM_BANKS
@byte HDR_CHR_ROM_BANKS
@byte ((HDR_MAPPER_ID & $0F) << 4) | (HDR_FOUR_SCREEN << 3) | (HDR_BATTERY << 1) | HDR_MIRRORING
@byte (HDR_MAPPER_ID & $F0) | %1000
@byte (HDR_SUBMAPPER_ID << 4) | ((HDR_MAPPER_ID & $F00) >> 8)
@byte ((HDR_PRG_ROM_BANKS & $F00) >> 4) | ((HDR_CHR_ROM_BANKS & $F00) >> 8)
@byte (HDR_PRG_NVRAM_SIZE << 4) | HDR_PRG_RAM_SIZE
@byte (HDR_CHR_NVRAM_SIZE << 4) | HDR_CHR_RAM_SIZE
@byte HDR_REGION
@byte 0, 0, 0
