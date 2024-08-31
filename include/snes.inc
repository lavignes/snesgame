; vim: ft=nyasm
\ifndef _SNES_INC
_SNES_INC = 1

\macro SET_A8
    \accum8
    rep #$10
\end

\macro SET_I16
    \index16
    sep #$20
\end

\macro SET_A8I16
    SET_A8
    SET_I16
\end

\macro SET_A16I16
    \accum16 \index16
    sep #$30
\end

;; LoOAM Table OBJ Structure
;;
;; LoOAM Makes up the first 512 Words of OAM (128 OBJs)
;;
\struct OAMOBJ
    ;; Lo 8 Bits of X (rest in HiOAM)
    .X     1

    ;; Y Pos
    .Y     1

    ;; Lo 8 Bits of Tile (Hi Bit in .Attrs)
    .Tile  1

    ;; OBJ Attributes
    ;;
    ;; Fields: vhppccct
    ;; t   - Hi Bit of Tile
    ;; ccc - Palette
    ;; pp  - Priority
    ;; h   - Horizontal Flip
    ;; v   - Vertical Flip
    ;;
    .Attrs 1
\end

;; Screen Display (W)
;;
;; Fields: f...bbbb
;; bbbb - Brightness
;; f    - Force VBlank
;;
INIDISP = $2100

SETINI = $2133

;; OBJ Size and CHR Address (W)
;;
;; Fields: sssnn.bb
;; bb  - First CHR Bank Address (16KiB VRAM page)
;; nn  - Second CHR Bank Offset (8KiB VRAM page following bb)
;; sss - OBJ Size (0 = 8x8, 16x16)
;;                (1 = 8x8, 32x32)
;;                (2 = 8x8, 64x64)
;;                (3 = 16x16, 32x32)
;;                (4 = 16x16, 64x64)
;;                (5 = 32x32, 64x64)
;;                (6 = 16x32, 32x64)
;;                (7 = 16x32, 32x32)
;;
OBJSEL = $2101

;; OAM Address (W)
;;
;; Fields: p.....h aaaaaaaa
;; aaaaaaaa - OAM Word Address
;; h        - OAM Table Select (0 = LoOAM, 1 = HiOAM)
;; p        - Enable OAM Priority Rotation
;;            TODO: better docs
;;
OAMADDL = $2102
OAMADDH = $2103

;; OAM Data Write (W)
;;
;; NOTE: When writing to LoOAM, OAMADDR is only incremented every 2 writes.
;;
OAMDATA = $2104

;; OAM Data Read (R)
OAMDATAREAD = $2138

;; BG Mode and CHR Size (W)
;;
;; Fields: 4321pmmm
;; mmm - BG Mode
;; p   - Mode 1 BG3 Priority (0 = Normal, 1 = High)
;; 1   - BG1 CHR Size (0 = 8x8, 1 = 16x16)
;; 2   - BG2 CHR Size (0 = 8x8, 1 = 16x16)
;; 3   - BG3 CHR Size (0 = 8x8, 1 = 16x16)
;; 4   - BG4 CHR Size (0 = 8x8, 1 = 16x16)
;;
BGMODE = $2105

;; BG Tilemap Address and Size (W)
;;
;; Fields: aaaaaavh
;; h      - Horizontal Tilemap Count (0 = 1 Tilemap, 1 = 2 Tilemaps)
;; v      - Vertical Tilemap Count (0 = 1 Tilemap, 1 = 2 Tilemaps)
;; aaaaaa - Tilemap VRAM Address (2KiB VRAM page)
;;
BG1SC = $2107
BG2SC = $2108
BG3SC = $2109
BG4SC = $210A

;; BG Name Table (Tilemap) Base Address (W)
;;
;; Fields: hhhhllll
;; llll - BG1/3 Tilemap Base Address (8KiB VRAM Page)
;; hhhh - BG2/4 Tilemap Base Address (8KiB VRAM Page)
;;
BG12NBA = $210B
BG34NBA = $210C

;; BG Horizontal Scroll (W)
;;
;; Fields: ......xx xxxxxxxx
;; xxxxxxxxxx - (Write Twice)
;;
BG1HOFS = $210D
BG2HOFS = $210F
BG3HOFS = $2111
BG4HOFS = $2113

;; BG Vertical Scroll (W)
;;
;; Fields: ......yy yyyyyyyy
;; yyyyyyyyyy - (Write Twice)
;;
BG1VOFS = $210E
BG2VOFS = $2110
BG3VOFS = $2112
BG4VOFS = $2114

;; Screen Pixelation (W)
;;
;; Fields: ssss4321
;; 1    - Enable BG1 mosaic
;; 2    - Enable BG2 mosaic
;; 3    - Enable BG3 mosaic
;; 4    - Enable BG4 mosaic
;; ssss - Pixel Size ($0 = 1x1, ... $F = 16x16)
;;
MOSAIC = $2106

;; Window Settings BG1/3 and BG2/4 (W)
;;
;; Fields: DdCcBbAa
;; a - Invert window 1 for BG1/3
;; A - Enable window 1 for BG1/3
;; b - Invert window 2 for BG1/3
;; B - Enable window 2 for BG1/3
;; c - Invert window 1 for BG2/4
;; C - Enable window 1 for BG2/4
;; d - Invert window 2 for BG2/4
;; D - Enable window 2 for BG2/4
;;
W12SEL = $2123
W34SEL = $2124

;; Window Settings for OBJ and Color Window (W)
;;
;; Fields: DdCcBbAa
;; a - Invert window 1 for OBJ
;; A - Enable window 1 for OBJ
;; b - Invert window 2 for OBJ
;; B - Enable window 2 for OBJ
;; c - Invert window 1 for color
;; C - Enable window 1 for color
;; d - Invert window 2 for color
;; D - Enable window 2 for color
;;
WOBJSEL = $2125

;; Window 1 Left/Right Edge Position (W)
;;
;; Fields: llllllll
;; llllllll - Position
;;
WH0 = $2126
WH1 = $2127

;; Window 2 Left/Right Edge Position (W)
;;
;; Fields: llllllll
;; llllllll - Position
;;
WH2 = $2128
WH3 = $2129

;; Window BG Mask Logic (W)
;;
;; Fields: 44332211
;; 11 - BG1 (0 = OR, 1 = AND, 2 = XOR, 3 = XNOR)
;; 22 - BG2 (0 = OR, 1 = AND, 2 = XOR, 3 = XNOR)
;; 33 - BG3 (0 = OR, 1 = AND, 2 = XOR, 3 = XNOR)
;; 44 - BG4 (0 = OR, 1 = AND, 2 = XOR, 3 = XNOR)
;;
WBGLOG = $212A

;; Window OBK and Color Mask Logic
;;
;; Fields: ....ccoo
;; oo - OBJ (0 = OR, 1 = AND, 2 = XOR, 3 = XNOR)
;; cc - Color Window (0 = OR, 1 = AND, 2 = XOR, 3 = XNOR)
;;
WOBJLOG = $212B

;; Main Screen Enable (W)
;;
;; Fields: ...o4321
;; 1 - BG1
;; 2 - BG2
;; 3 - BG3
;; 4 - BG4
;; o - OBJ
;;
TM = $212C

;; Subscreen Enable (W)
;;
;; Fields: ...o4321
;; 1 - BG1
;; 2 - BG2
;; 3 - BG3
;; 4 - BG4
;; o - OBJ
;;
TS = $212D

;; Main Screen Enable Window (W)
;;
;; Fields: ...o4321
;; 1 - BG1
;; 2 - BG2
;; 3 - BG3
;; 4 - BG4
;; o - OBJ
;;
TMW = $212E

;; Subscreen Enable Window (W)
;;
;; Fields: ...o4321
;; 1 - BG1
;; 2 - BG2
;; 3 - BG3
;; 4 - BG4
;; o - OBJ
;;
TSW = $212F

CGWSEL = $2130

CGADSUB = $2131

COLDATA = $2132

;; VRAM Port Control (W)
;;
;; Fields: m...rrii
;; ii - Increment Amount (0 = 1 Word, 1 = 32 Words, 2/3 = 128 Words)
;; rr - Address Remapping Control (0 = None, 1 = 2bpp, 2 = 4bpp, 3 = 8bpp)
;;
;;      When no remapping is done, VRAM writes ones CHR a time, going through
;;      each bitplane of each row of that CHR. When enabled, VRAM writes will
;;      update the bitplanes of each row 32 of CHRs at a time.
;;      Obviously, this is intended for an 32x32 Tilemap with 8x8 tiles.
;;
;;      Example sequential writes for the top-left 3 pixels in 8x8 tilemap @ 2bpp:
;;      ----------------------------        ----------------------------
;;      |write 1 |write 17|        |        |write 1 |write 3 |write 5 |
;;      |write 2 |        |        |   vs   |write 2 |write 4 |........|
;;      |--------|--------|--------|        |--------|--------|--------|
;;      |write 3 |        |        |        |write 65|        |        |
;;      |........|        |        |        |        |        |        |
;;      ----------------------------        ----------------------------
;;
;; m  - Increment Mode (0 = After access of VMDATAL/VMDATALREAD)
;;                     (1 = After access of VMDATAH/VMDATAHREAD)
;;
VMAIN = $2115

;; VRAM Address (W)
VMADDL = $2116
VMADDH = $2117

;; VRAM Data Write (W)
VMDATAL = $2118
VMDATAH = $2119

;; VRAM Data Read (H)
;;
;; NOTE: Do an initial dummy read if doing a block read
VMDATALREAD = $2139
VMDATAHREAD = $213A

;; CGRAM Address (W)
CGADD = $2121

;; CGRAM Data Write (W)
;;
;; Fields: .bbbbbgg gggrrrrr
;; bbbbbgggggrrrrr - (Write Twice)
;;
CGDATA = $2122

;; CGRAM Data Read (R)
;;
;; Fields: .bbbbbgg gggrrrrr
;; bbbbbgggggrrrrr - (Read Twice)
;;
CGDATAREAD = $213B

;; DMA Enable (W)
;;
MDMAEN = $420B

;; HDMA Enable (W)
;;
;; Fields: 76543210
;; 0 - Channel 0 HMDA Enable
;; 1 - Channel 1 HMDA Enable
;; 2 - Channel 2 HMDA Enable
;; 3 - Channel 3 HMDA Enable
;; 4 - Channel 4 HMDA Enable
;; 5 - Channel 5 HMDA Enable
;; 6 - Channel 6 HMDA Enable
;; 7 - Channel 7 HMDA Enable
;;
HDMAEN = $420C

;; DMAPn
\loop n, 8
    \join DMAP, n = ($4300 + (n << 4))
\end

;; BBADn
\loop n, 8
    \join BBAD, n = ($4301 + (n << 4))
\end

;; A1Tn
\loop n, 8
    \join A1T, n, L = ($4302 + (n << 4))
    \join A1T, n, H = ($4303 + (n << 4))
\end

;; A1Bn
\loop n, 8
    \join A1B, n = ($4304 + (n << 4))
\end

;; DASnL
\loop n, 8
    \join DAS, n, L = ($4305 + (n << 4))
    \join DAS, n, H = ($4306 + (n << 4))
\end

;; ROM Speed (W)
;;
;; Fields: .......s
;; s - (0 = 2.68MHz, 1 = 3.58MHz)
;;
ROMSEL = $420D

;; WRAM Data (R/W)
WMDATA = $2180

;; WRAM Address (W)
WMADDL = $2181
WMADDM = $2182
WMADDH = $2183

;; Interrupt and Joypad Auto-Read (W)
;;
;; Fields: n.tt...j
;; j  - Joypad Auto-Read (0 = Disable, 1 = Enable)
;;
;;      Joypad Auto-Read will parallely read the joypad state into JOY1-4 at
;;      the start of VBlank.
;;      NOTE: Check status of auto-read in HVBJOY1
;;
;; tt - Enable IRQs for H and VBlank Timers
;;                      (00 = No IRQs)
;;                      (01 = IRQ when H counter == HTIME)
;;                      (10 = IRQ when V counter == VTIME and H counter == 0)
;;                      (11 = IRQ when V counter == VTIME and H counter == HTIME)
;; n - VBlank NMI (0 = Disable, 1 = Enable)
;;
NMITIMEN = $4200

;; NMI VBlank Flag and CPU Version (R)
;;
;; Fields: v...nnnn
;; v    - NMI Active (Does not flip during forced blank/NMI disabled)
;; nnnn - CPU Version
;;
RDNMI = $4210

;; Screen and Joypad Status (R)
;;
;; Fields: vh.....j
;; j - Joypad Auto-Read in-progress
;; h - HBlank Active (Continues to change even if VBlank forced)
;; v - VBlank Active (Continues to change even if VBlank forced)
;;
HVBJOY = $4212

;; Joypad Auto-Read Data (R)
JOY1L = $4218
JOY1H = $4219
JOY2L = $421A
JOY2H = $421B
JOY3L = $421C
JOY3H = $421D
JOY4L = $421E
JOY4H = $421F

\end ; _SNES_INC
