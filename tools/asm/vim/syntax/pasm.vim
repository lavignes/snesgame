syn clear
syn case ignore

syn match pasmIdentifier "[a-z_\.][a-z0-9_\.]*"
syn match pasmGlobalLabel "^[a-z_][a-z0-9_\.]*"
syn match pasmLocalLabel "^\.[a-z_][a-z0-9_]*"

syn keyword pasmRegister x y s *

syn match pasmOperator display "\%(+\|-\|/\|*\|\^\|\~\|&\||\|!\|>\|<\|%\)=\?"
syn match pasmOperator display "&&\|||\|<<\|>>\|\~>"

syn keyword pasmOpcode adc and asl bcc bcs beq bit bmi bne bpl brk brl bvc bvs
syn keyword pasmOpcode clc cld cli clv cmp cop cpx cpy dec dex dey eor inc inx iny
syn keyword pasmOpcode jmp jsr lda ldx ldy lsr nop ora
syn keyword pasmOpcode pha php pla plp
syn keyword pasmOpcode rol ror rti rts sbc sec sed sei sta stx sty
syn keyword pasmOpcode tax tay tsx txa txs tya

syn keyword pasmOpcodeNative bra jml jsl mvn mvp pea pei per phb phd phk phx phy
syn keyword pasmOpcodeNative plb pld plx ply rep rtl sep stp sty
syn keyword pasmOpcodeNative tcd tcs tdc trb tsb tsc txy tyx wai wdc xba xce

syn keyword pasmDirective \@byte \@word \@section \@export \@pad \@align \@include \@if \@ifdef \@ifndef \@end
syn keyword pasmDirective \@index8 \@index16 \@accum8 \@accum16 \@emulate \@native
syn keyword pasmDirective \@macro \@equ

syn match pasmComment ";.*" contains=pasmTodo
syn keyword pasmTodo  contained todo fixme xxx warning danger note notice bug
syn region pasmString start=+"+ end=+"+

syn match pasmNumber "[0-9]\+"
syn match pasmNumber "\$[0-9a-fA-F]\+"
syn match pasmNumber "%[01]\+"
syn match pasmNumber "'[!-~]"

syn case match

hi def link pasmComment       Comment
hi def link pasmNumber        Number
hi def link pasmString	      String
hi def link pasmIdentifier    Identifier
hi def link pasmOpcode        Keyword
hi def link pasmOpcodeNative  Special
hi def link pasmDirective     PreProc
hi def link pasmGlobalLabel   Function
hi def link pasmLocalLabel    Function
hi def link pasmTodo          Todo

let b:current_syntax = "pasm"
set ts=4
set sw=4
set et
