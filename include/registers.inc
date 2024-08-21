; vim: ft=nyasm

;; Screen Display (W)
;;
;; Fields: f...bbbb
;; f    - Force Blank
;; bbbb - Brightness
;;
INIDISP = $2100

;; BG Mode and CHR Size (W)
;;
;; Fields: 4321pmmm
;; mmm - BG Mode
;; p   - Mode 1 BG3 Priority (0 = normal, 1 = high)
;; 1   - BG1 CHR Size (0 = 8x8, 1 = 16x16)
;; 2   - BG2 CHR Size (0 = 8x8, 1 = 16x16)
;; 3   - BG3 CHR Size (0 = 8x8, 1 = 16x16)
;; 4   - BG4 CHR Size (0 = 8x8, 1 = 16x16)
BGMODE = $2105
