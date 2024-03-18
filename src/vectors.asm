; vim: ft=pasm

?section "VECTORS"

?word IntNmi, IntReset, IntIrq

?section "KERNEL"

?export IntNmi, IntReset, IntIrq

IntNmi:
IntReset:
IntIrq:
    ?byte ?tag Test, "bank"
    rti

