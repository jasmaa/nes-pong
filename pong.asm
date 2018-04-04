
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  .inesprg 1
  .ineschr 1
  .inesmap 0
  .inesmir 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  .rsset $0000

gamestate	.rs 1
ball_x		.rs 1
ball_y		.rs 1
ball_u_vel	.rs 1
ball_d_vel	.rs 1
ball_l_vel	.rs 1
ball_r_vel	.rs 1
paddle_1_y	.rs 1
paddle_1_h	.rs 1
ctrl_1		.rs 1
ctrl_2		.rs 1
score_1		.rs 1
score_2		.rs 1

STATE_TITLE		= $00
STATE_PLAYING	= $01
STATE_GAMEOVER	= $02
  
RWALL		= $F4
UWALL		= $20
DWALL		= $E0
LWALL		= $04

PADDLE_1_X	= $20
PADDLE_2_X	= $E0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROGRAM - MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  .bank 0
  .org $C000

vblankwait:
  bit $2002
  bpl vblankwait
  rts

; ===================  
; === START RESET ===

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
  cpx #$20
  bne LoadPaletteLoop

  
  ; set ball init
  lda #$80
  sta ball_x
  sta ball_y
  lda #$01
  sta ball_d_vel
  sta ball_r_vel
  
  ; set init state
  lda #STATE_PLAYING
  sta gamestate
  
  ; set ppu
  lda #%10010000
  sta $2000
  lda #%00011110
  sta $2001
  
Forever:
  jmp Forever
  
; === END RESET ===
; ================= 

; ===========================
; === START NMI INTERRUPT ===
NMI:
  ; set RAM address to 0200
  lda #$00
  sta $2003
  lda #$02
  sta $4014
  
  ; ppu clean up
  lda #%10010000
  sta $2000 ; set PPUCTRL
  lda #%00011110
  sta $2001 ; set PPUMASK
  lda #%00000000
  sta $2005 ; disable PPU scrolling
  sta $2005
  
  
; read controllers

; GAME ENGINE
GameEngine:

  ;jmp GameEngineDone

  lda gamestate
  cmp #STATE_TITLE
  beq RunTitle
  
  lda gamestate
  cmp #STATE_PLAYING
  beq RunPlaying
  
  lda gamestate
  cmp #STATE_GAMEOVER
  beq RunGameOver
GameEngineDone:
  jsr UpdateSprites
  rti
  
RunTitle:
  ; Running title
  jmp GameEngineDone
  
RunPlaying:
  ; Running main game
  
  ; update ball location
  lda ball_x
  clc
  adc ball_r_vel
  sta ball_x
  
  lda ball_x
  sec
  sbc ball_l_vel
  sta ball_x
  
  ; bounces
  
  lda ball_x
  
  BounceRightWall:
    cmp #RWALL
    bcc RightWallDone
    lda #$00
    sta ball_r_vel
    lda #$01
    sta ball_l_vel
  RightWallDone:
  
  BounceLeftWall:
    cmp #LWALL
    bcc LeftWallDone
    lda #$00
    sta ball_l_vel
    lda #$01
    sta ball_r_vel
  LeftWallDone:
  
  jmp GameEngineDone
  
RunGameOver:
  ; Running game over screen
  jmp GameEngineDone
; END GAME ENGINE

UpdateSprites:
  
  lda ball_y
  sta $0218
  
  lda #$00
  sta $0219
  
  lda #$00
  sta $021A
  
  lda ball_x
  sta $021B
  
  rts

; === END NMI INTERRUPT ===
; =========================

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
    .db $0F,$17,$28,$39, $0F,$17,$28,$39, $0F,$17,$28,$39, $0F,$17,$28,$39 
	.db $0F,$17,$28,$39, $0F,$17,$28,$39, $0F,$17,$28,$39, $0F,$17,$28,$39
  
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