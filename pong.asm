
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  .inesprg 1
  .ineschr 1
  .inesmap 0
  .inesmir 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROGRAM - MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  .bank 0
  .org $C000

vblankwait:
  bit $2002
  bpl vblankwait
  rts
  
; START MAIN PRG
RESET:
  ; disable irq
  sei
  cld
  ldx #%01000000
  stx $4017
  
  ; set up stack
  ldx #$FF
  txs
  
  ldx #$00
  stx $2000 ; disable NMI
  stx $2001 ; disable rendering
  stx $4010 ; disable dmc

  ; 1st vblank
  jsr vblankwait
  
  ldx #$00
clrmem:
  lda #$00
  sta $0000, x
  sta $0100, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  lda #$FE
  sta $0300, x
  inx
  bne clrmem ; loop until x overflows

  ; 2nd vblank
  jsr vblankwait

  ; TEST INTENSIFY BLUE
  lda #%10000000
  sta $2001 ; set PPUMASK
  
LoadSprites:
  ldx #$00
LoadSpritesLoop:
  lda sprites, x
  sta $0200, x
  inx
  cpx #$1C
  bne LoadSpritesLoop

LoadPalette:
  lda $2002
  lda #$3F
  sta $2006
  lda #$00
  sta $2006
  ldx #$00
LoadPaletteLoop:
  lda palette, x
  sta $2007
  inx
  cpx #$04
  bne LoadPaletteLoop

  ; set ppu
  lda #%10000000
  sta $2000 ; set PPUCTRL
  lda #%00010000
  sta $2001 ; set PPUMASK
  lda #%00000000
  sta $2005 ; disable PPU scrolling
  
Forever:
  jmp Forever

; START NMI INTERRUPT
NMI:
  ; write 0020
  lda #$00
  sta $2003
  lda #$02
  sta $4014
  
  rti

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROGRAM - INTERRUPTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  .bank 1
  
  .org $E000
  sprites:
    .db $79,$01,$00,$20
	.db $80,$01,$00,$20
	.db $81,$01,$00,$20
	
	.db $79,$01,$00,$E0
	.db $80,$01,$00,$E0
	.db $81,$01,$00,$E0
	
	.db $80,$00,$00,$80
	
  palette:
    .db $0F,$17,$28,$39
  
  .org $FFFA
  .dw NMI
  .dw RESET
  .dw 0 ; no irq interrupt

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CHARACTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  .bank 2
  .org $0000
  .incbin "graphics.chr"