; vim: ft=pasm

?section "KERNEL"

?export Reset, Nmi, Irq

Nmi:
    rti

Irq:
    rti

MMC3_BANK_SELECT = $8000
MMC3_BANK_DATA   = $8001
MMC3_BASE        = %00000000
MMC3_BANK_PRG_80 = MMC3_BASE | %110
MMC3_BANK_PRG_A0 = MMC3_BASE | %111

Reset:
    ; the registers of mmc3 are in an undefined state at poweron.
    ; this sets up the $8000 and $A000 areas to be bankable.
    lda #MMC3_BANK_PRG_80
    sta MMC3_BANK_SELECT
    lda #0
    sta MMC3_BANK_DATA
    lda #MMC3_BANK_PRG_A0
    sta MMC3_BANK_SELECT
    lda #1
    sta MMC3_BANK_DATA

    jmp Reset

