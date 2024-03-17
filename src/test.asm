; vim: ft=pasm

@section "KERNEL"

Loop:
    jmp Loop

@section "VECTORS"
@word IntNmi, IntReset, IntIrq
