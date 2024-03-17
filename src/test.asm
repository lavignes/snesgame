; vim: ft=pasm

@SECTION "KERNEL"

Loop:
    jmp Loop

@SECTION "VECTORS"
@WORD IntNmi, IntReset, IntIrq
