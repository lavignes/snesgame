syn clear
syn case ignore

syn match nyasmIdentifier "[a-z_\.][a-z0-9_\.]*"
syn match nyasmGlobalLabel "^[a-z_][a-z0-9_\.]*"
syn match nyasmLocalLabel "^\.[a-z_][a-z0-9_]*"

syn keyword nyasmRegister a x y

syn match nyasmOperator display "\%(+\|-\|/\|*\|\^\|\~\|&\||\|!\|>\|<\|%\|=\)=\?"
syn match nyasmOperator display "&&\|||\|<<\|>>\|\~>"

syn keyword nyasmOpcode adc and asl bcc bcs beq bit bmi bne bpl bra brk brl bvc bvs
syn keyword nyasmOpcode clc cld cli clv cmp cop cpx cpy dec dex dey eor inc inx iny
syn keyword nyasmOpcode jml jmp jsl jsr lda ldx ldy lsr mvn mvp nop ora pea pei per
syn keyword nyasmOpcode pha phb phd phk php phx phy pla plb pld plp plx ply
syn keyword nyasmOpcode rep rol ror rti rtl rts sbc sec sed sei sep sta stp stx sty stz
syn keyword nyasmOpcode tax tay tcd tcs tdc trb tsb tsc tsx txa txs txy tya tyx wai wdc xba xce

syn match nyasmDirective "\\byte"
syn match nyasmDirective "\\word"
syn match nyasmDirective "\\long"
syn match nyasmDirective "\\section"
syn match nyasmDirective "\\include"
syn match nyasmDirective "\\if"
syn match nyasmDirective "\\ifdef"
syn match nyasmDirective "\\ifndef"
syn match nyasmDirective "\\end"
syn match nyasmDirective "\\res"
syn match nyasmDirective "\\macro"
syn match nyasmDirective "\\tag"
syn match nyasmDirective "\\len"
syn match nyasmDirective "\\for"
syn match nyasmDirective "\\fail"
syn match nyasmDirective "\\struct"
syn match nyasmDirective "\\[0-9]\+"
syn match nyasmDirective "\\narg"
syn match nyasmDirective "\\shift"
syn match nyasmDirective "\\uniq"
syn match nyasmDirective "\\join"
syn match nyasmDirective "\\term"
syn match nyasmDirective "\\index8"
syn match nyasmDirective "\\index16"
syn match nyasmDirective "\\accum8"
syn match nyasmDirective "\\accum16"
syn match nyasmDirective "\\emulation"
syn match nyasmDirective "\\native"

syn match nyasmComment ";.*" contains=nyasmTodo
syn match nyasmDocComment ";;.*" contains=nyasmTodo
syn keyword nyasmTodo contained todo fixme xxx warning danger note notice bug
syn match nyasmEscape contained "\\."
syn region nyasmString start=+"+ end=+"+ contains=nyasmEscape
syn region nyasmChar start=+'+ end=+'+ contains=nyasmEscape

syn match nyasmNumber "[0-9][0-9_]*"
syn match nyasmNumber "\$[0-9a-fA-F_]\+"
syn match nyasmNumber "%[01_]\+"

syn case match

hi def link nyasmComment      Comment
hi def link nyasmDocComment   SpecialComment
hi def link nyasmNumber       Number
hi def link nyasmString	       String
hi def link nyasmChar         Character
hi def link nyasmIdentifier   Identifier
hi def link nyasmRegister     Identifier
hi def link nyasmOpcode       Keyword
hi def link nyasmEscape       SpecialChar
hi def link nyasmDirective    PreProc
hi def link nyasmGlobalLabel  Function
hi def link nyasmLocalLabel   Function
hi def link nyasmTodo         Todo

let b:current_syntax = "nyasm"
set ts=4
set sw=4
set et

