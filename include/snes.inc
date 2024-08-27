; vim: ft=nyasm
\ifndef _SNES_INC
_SNES_INC = 1

;; LoOAM Table OBJ Structure
;;
;; LoOAM Makes up the first 512 Words of OAM (128 OBJs)
\struct OAMOBJ
    ;; Lo 8 Bits of X (rest in HiOAM)
    ;;
    .X     1

    ;; Y Pos
    ;;
    .Y     1

    ;; Lo 8 Bits of Tile (Hi Bit in .Attrs)
    ;;
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
;; p        - OAM Priority Rotation (0 = Disable, 1 = Enable)
;;            TODO: better docs
;;
OAMADDL = $2102
OAMADDH = $2103

;; OAM Data Write (W)
;;
;; Note: When writing to LoOAM, OAMADDR is only incremented every 2 writes.
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

;; Main Screen Enable (W)
;;
;; Fields: ...o4321
;; 1 - BG1
;; 2 - BG2
;; 3 - BG3
;; 4 - BG4
;; o - OBJ
TM = $212C

;; Subscreen Enable (W)
;;
;; Fields: ...O4321
;; 1 - BG1
;; 2 - BG2
;; 3 - BG3
;; 4 - BG4
;; o - OBJ
TS = $212D

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
;;      Example 3 sequential writes for 2bpp:
;;      ----------------------------        ----------------------------
;;      |write 1 |write 17|        |        |write 1 |write 3 |write 5 |
;;      |write 2 |        |        |   vs   |write 2 |write 4 |........|
;;      |--------|--------|--------|        |--------|--------|--------|
;;      |write 3 |        |        |        |write 65|        |        |
;;      |........|        |        |        |        |        |        |
;;
;; m  - Increment Mode (0 = After access of VMDATAL/VMDATALREAD)
;;                     (1 = After access of VMDATAH/VMDATAHREAD)
;;
VMAIN = $2115

;; VRAM Address (W)
VMADDL = $2117
VMADDH = $2118

;; VRAM Data Write (W)
VMDATAL = $2119
VMDATAH = $211A

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

;; ROM Speed (W)
;;
;; Fields: .......s
;; s - (0 = 2.68MHz, 1 = 3.58MHz)
;;
ROMSEL = $420D

;; Interrupt and Joypad Auto-Read (W)
;;
;; Fields: n.tt...j
;; j  - Joypad Auto-Read (0 = Disable, 1 = Enable)
;;
;;      Joypad Auto-Read will parallely read the joypad state into JOY1-4 at
;;      the start of VBlank.
;;      Note: Check status of auto-read in HVBJOY1
;;
;; tt - Enable IRQs for H and VBlank Timers
;;                      (00 = No IRQs)
;;                      (01 = IRQ when H counter == HTIME)
;;                      (10 = IRQ when V counter == VTIME and H counter == 0)
;;                      (11 = IRQ when V counter == VTIME and H counter == HTIME)
;; n - VBlank NMI (0 = Disable, 1 = Enable)
;;
NMITIMEN = $4200

;; Screen and Joypad Status (R)
;;
;; Fields: vh.....j
;; j - Joypad Auto-Read in-progress
;; h - HBlank Active
;; v - VBlank Active
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