
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
ball_u		.rs 1
ball_r		.rs 1
ball_x_speed	.rs 1
ball_y_speed	.rs 1
paddle_1_y	.rs 1
paddle_2_y	.rs 1
ctrl_1		.rs 1
ctrl_2		.rs 1
score_1		.rs 1
score_2		.rs 1
rand_seed	.rs 1
pointerLo	.rs 1
pointerHi	.rs 1

STATE_TITLE		= $00
STATE_PLAYING	= $01
STATE_GAMEOVER	= $02
  
RWALL		= $F4
UWALL		= $20
DWALL		= $E0
LWALL		= $04

PADDLE_1_X	= $20
PADDLE_2_X	= $E0
PADDLE_1_H	= $18
PADDLE_2_H	= $18
PADDLE_SPEED= $02

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROGRAM - MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  .bank 0
  .org $C000

vblankwait:
  bit $2002
  bpl vblankwait
  rts

; update random num
prng:
  ldx #$08
  lda rand_seed+0
  prng_1:
    asl A
    rol rand_seed+1
    bcc prng_2
    eor #$2D
  prng_2:
	dex
	bne prng_1
	sta rand_seed+0
	cmp #$00 ; reload flags
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
  cpx #$24
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

LoadBG:
  lda $2002
  lda #$20	; remember top row doesn't get rendered
  sta $2006
  lda #$00
  sta $2006
  ldx #$00
  ldy #$00
  
  lda #LOW(background)
  sta pointerLo
  lda #HIGH(background)
  sta pointerHi
  
; FIGURE OUT HOW TO LOAD ENTIRE BG!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
LoadBGLoop:
  lda [pointerLo], y	; can only be used with y
  sta $2007
  iny
  bne LoadBGLoop
  inc pointerHi
  inx
  cpx #$04
  bne LoadBGLoop

  
  ; set ball init
  lda #$80
  sta ball_x
  sta ball_y
  lda #$01
  sta ball_r
  
  lda #$01
  sta ball_x_speed
  lda #$01
  sta ball_y_speed
  
  
  ; set paddles init
  lda #$80
  sta paddle_1_y
  sta paddle_2_y
  
  ; set init state
  lda #STATE_PLAYING
  sta gamestate
  
  ; set seed
  lda #$66
  sta rand_seed
  
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
  jsr ReadCtrl1
  jsr ReadCtrl2

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

RunGameOver:
  ; Running game over screen
  jmp GameEngineDone
  
RunPlaying:
  ; Running main game
  
  
  ; update ball location
  ; move ball vert
  lda ball_u
  beq MoveBallDown
  lda ball_y
  sec
  sbc ball_y_speed
  sta ball_y
  jmp MoveBallVertDone
  MoveBallDown:
    lda ball_y
	clc
	adc ball_y_speed
	sta ball_y
  MoveBallVertDone:
  ; move ball hori
  lda ball_r
  beq MoveBallLeft
  lda ball_x
  clc
  adc ball_x_speed
  sta ball_x
  jmp MoveBallHoriDone
  MoveBallLeft:
    lda ball_x
	sec
	sbc ball_x_speed
	sta ball_x
  MoveBallHoriDone:
  
  ; move paddle locations
  MovePaddle1:
    ; hit up wall
    lda paddle_1_y
	sec
	sbc #$08
	cmp #UWALL
	bcc MovePaddle1UpDone
	
	; move paddle up
    lda ctrl_2
    and #%0001000
    beq MovePaddle1UpDone
	lda paddle_1_y
	sec
	sbc #PADDLE_SPEED
	sta paddle_1_y
  MovePaddle1UpDone:
    ; hit down wall
    lda paddle_1_y
	clc
	adc #$08
	cmp #DWALL
	bcs MovePaddle1DownDone
  
    ; move paddle down
	lda ctrl_2
    and #%0000100
    beq MovePaddle1DownDone
	lda paddle_1_y
	clc
	adc #PADDLE_SPEED
	sta paddle_1_y
  MovePaddle1DownDone:
  
  ; same as paddle 1
  MovePaddle2:
    lda paddle_2_y
	sec
	sbc #$08
	cmp #UWALL
	bcc MovePaddle2UpDone
  
    lda ctrl_1
    and #%0001000
    beq MovePaddle2UpDone
	lda paddle_2_y
	sec
	sbc #PADDLE_SPEED
	sta paddle_2_y
  MovePaddle2UpDone:
    lda paddle_2_y
	clc
	adc #$08
	cmp #DWALL
	bcs MovePaddle2DownDone
  
	lda ctrl_1
    and #%0000100
    beq MovePaddle2DownDone
	lda paddle_2_y
	clc
	adc #PADDLE_SPEED
	sta paddle_2_y
  MovePaddle2DownDone:
	
  ; wall bounces
  BounceRightWall:
    lda ball_x
    cmp #RWALL
    bcc RightWallDone
    
	jsr ScorePointPaddle1
	jsr RespawnBall
	jmp GameEngineDone
  RightWallDone:
  
  BounceLeftWall:
    lda ball_x
    cmp #LWALL
    bcs LeftWallDone
    
	jsr ScorePointPaddle2
	jsr RespawnBall
	jmp GameEngineDone
  LeftWallDone:
  
  BounceUpWall:
    lda ball_y
    cmp #UWALL
    bcs UpWallDone
    lda #$00
	sta ball_u
  UpWallDone:
  
  BounceDownWall:
    lda ball_y
    cmp #DWALL
    bcc DownWallDone
    lda #$01
	sta ball_u
  DownWallDone:
  
  ; paddle bounces
  BouncePaddle1:
    lda #PADDLE_1_X
	clc
	adc #$03
	cmp ball_x
	bcc BouncePaddle1Done
	lda #PADDLE_1_X
	sec
	sbc #$03
	cmp ball_x
	bcs BouncePaddle1Done
	
	lda paddle_1_y
	sec
	sbc #$0B				; 08 for paddle and 03 for ball
    cmp ball_y
    bcs BouncePaddle1Done
	lda paddle_1_y
	clc
	adc #$0B
    cmp ball_y
    bcc BouncePaddle1Done
	
	lda #$01
	sta ball_r
	
	; apply prng to paddle
	jsr prng
	jsr PRNGPaddle	
  BouncePaddle1Done:
  
  BouncePaddle2:
    lda #PADDLE_2_X
	sec
	sbc #$03
	cmp ball_x
	bcs BouncePaddle2Done
	lda #PADDLE_2_X
	clc
	adc #$03
	cmp ball_x
	bcc BouncePaddle2Done
	
	lda paddle_2_y
	sec
	sbc #$0B
    cmp ball_y
    bcs BouncePaddle2Done
	lda paddle_2_y
	clc
	adc #$0B
    cmp ball_y
    bcc BouncePaddle2Done
	
	lda #$00
	sta ball_r
	
	; apply prng to paddle
	jsr prng
	jsr PRNGPaddle
  BouncePaddle2Done:
  
  jmp GameEngineDone
  
; END GAME ENGINE


; read controllers
ReadCtrl1:
  lda #$01
  sta $4016
  lda #$00
  sta $4016
  ldx #$08
ReadCtrl1Loop:
  lda $4016
  lsr A
  rol ctrl_1
  dex
  bne ReadCtrl1Loop
  rts

ReadCtrl2:
  lda #$01
  sta $4016
  lda #$00
  sta $4016
  ldx #$08
ReadCtrl2Loop:
  lda $4017
  lsr A
  rol ctrl_2
  dex
  bne ReadCtrl2Loop
  rts
  
; update sprites
UpdateSprites:

  lda ball_y
  sta $0218
  lda ball_x
  sta $021B
  
  ; paddle 1 movement
  lda paddle_1_y
  sta $0204
  sec
  sbc #$08
  sta $0200
  clc
  adc #$0F
  sta $0208
  
  ; paddle 2 movement
  lda paddle_2_y
  sta $0210
  sec
  sbc #$08
  sta $020C
  clc
  adc #$0F
  sta $0214
  
  ; Score
  lda score_1
  clc
  adc #$02  ; offset to get to 0 sprite
  sta $0221
  lda score_2
  clc
  adc #$02
  sta $021D
  
  rts

ScorePointPaddle1:
  lda score_1
  clc
  adc #$01
  sta score_1
  
  ; TEMP check for win
  lda score_1
  cmp #$07
  bne Score1Done
  lda #STATE_GAMEOVER
  sta gamestate

  Score1Done:
  rts
ScorePointPaddle2:
  lda score_2
  clc
  adc #$01
  sta score_2
  
  ; TEMP check for win
  lda score_2
  cmp #$07
  bne Score2Done
  lda #STATE_GAMEOVER
  sta gamestate

  Score2Done:
  rts

RespawnBall:
  jsr prng
  and #$02
  beq RespawnRight
  RespawnLeft:
    lda #$00
	sta ball_r
	jmp RespawnDirDone
  RespawnRight:
    lda #$01
	sta ball_r
  RespawnDirDone:

  lda #$80
  sta ball_x
  sta ball_y
  
  rts

; apply prng to paddle
; do prng first!
; fix slope of ball!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
PRNGPaddle:
    and #$03
	lsr  A
	sta ball_x_speed
	
    rts
  
; === END NMI INTERRUPT ===
; =========================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROGRAM - INTERRUPTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  .bank 1
  
  .org $E000
  sprites:
    ; Paddle 1
    .db $78,$01,$00,$20 ; 00
	.db $80,$01,$00,$20 ; 04
	.db $88,$01,$00,$20 ; 08

    ; Paddle 2	
	.db $78,$01,$00,$E0 ; 0C
	.db $80,$01,$00,$E0 ; 10
	.db $88,$01,$00,$E0 ; 14
	
	; Ball 1
	.db $80,$00,$00,$80 ; 18
	
	; Score 2
	.db $10,$02,$00,$A0 ; 1C
	; Score 1
	.db $10,$02,$00,$60 ; 20
	
  palette:
    .db $0F,$17,$28,$39, $0F,$17,$28,$39, $0F,$17,$28,$39, $0F,$17,$28,$39 
	.db $0F,$17,$28,$39, $0F,$17,$28,$39, $0F,$17,$28,$39, $0F,$17,$28,$39
  
  background:
    .incbin "graphics.nam"
  
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