;=================================================================
; Leaf Shield
; by lion
;
; Based on code by carol.
;
; Assistance from Alcaro, Super Maks 64, This Eye o' Mine
; orz orz OTL orz
;=================================================================

;=================
; Define
;=================
	
	!StatusFlag = !1528
	; Sprite State Flag, serves as the index for the Jump Table.
	; $00 Fired/Off | $01 On | $02 Firing
	
	!DirectionFlag = !157C
	; Mirrors Mario's direction
	; $00 Left | $01 Right
	
	!CooldownTimer = !15AC
	; Cooldown until firing another shield.

	!DrawOrNot = !1510
	; Checks whether to draw graphics or not.
	; $00 Don't run SubGFX | $01 Run SubGFX
	
	!OffsetFrame = $02
	; Modified Frame Counter to use as index for X/Y Offset.
	
	!LoopCounter = $03
	; Loop Counter for Graphics drawing.
	
	!LoopCounterMirror = $04
	; Mirrors Loop Counter to use as index for X/Y Offset.
	
;=================
; Init / Main
;=================
	
print "INIT ", pc
+	lda $76							; Set sprite's direction
	sta !DirectionFlag,x			; To be the same as Mario's.
	rtl
	
print "MAIN ", pc
	phb : phk
	plb
	jsr Start
	plb
	rtl
	
;=================
; Sprite Routines
;=================

Start:

	jsr SubGFX						; Graphics drawing routine, but first, check if drawing is necessary.

	lda !14C8,x						; \
	cmp #$08						; | End code if sprite status != alive
	bne Return						; /
	lda $9D							; \ Halt code if sprites locked.
	bne Return						; /
	
	lda #$03
	%SubOffScreen()					; Don't run when offscreen.
	
	lda !StatusFlag,x				; \ Load State depending on
	asl : tax						; | Status Flag value.
	jsr (Status,x)					; /
	
Return:
	rts
	
Status:
	dw NoShield
	dw Shield
	dw Firing

;==================
; Off State
;==================

NoShield:
	ldx $15E9|!Base2				; Reload Sprite Index.
	
	lda $94							; \
	sta !E4,x						; |
	lda $95							; |
	sta !14E0,x						; | Follow Mario.
	lda $96							; |
	sta !D8,x						; |
	lda $97							; |
	sta !14D4,x						; /

	stz !DrawOrNot,x				; Don't draw graphics.

	lda !CooldownTimer,x			; \ If sprite still on screen...
	bne +							; |
	
	lda $18							; | Or R isn't being pressed...
	and #$10						; |
	beq +							; / End code.
	
	inc !StatusFlag,x				; If offscreen and R is pressed, set shield on.
	
+	rts

;==================
; On State
;==================

Shield:
	ldx $15E9|!Base2				; Reload Sprite Index.
	
	lda $94							; \
	sta !E4,x						; |
	lda $95							; |
	sta !14E0,x						; | Follow Mario.
	lda $96							; |
	sta !D8,x						; |
	lda $97							; |
	sta !14D4,x						; /
	
	lda #$01						; \
	sta !DrawOrNot,x				; / Draw graphics.
	
	jsr Hitbox						; Run contact routine
	
	lda $18							; If not pressing R,
	and #$10						;
	beq +							; End code.
	
	jsr Spawn						; Else spawn firing shield.
	
	lda #$40						; Set cooldown timer to
	sta !CooldownTimer,x			; avoid overloading the screen with shields.
	
+	rts
	
;==================
; Firing State
;==================

Firing:
	ldx $15E9|!Base2				; Reload Sprite Index.
	
	lda #$01						; \
	sta !DrawOrNot,x				; / Draw graphics.
	
	lda !DirectionFlag,x : phx		; \
	tax								; | Load X Speed depending on 
	lda XSpeed,x : plx				; | direction being faced.
	sta !B6,x						; /
	stz !AA,x						; Set Y Speed to 0
	
	jsl $01802A|!BankB				; Update X/Y depending on speed.
	
	jsr Hitbox					; Run contact routine
	
+	rts
	
XSpeed:
	db $C0,$40			; Left, Right

;==================
; Spawning Routine
;==================

ReturnS:
	rts

Spawn:
	stz $00					; \
	stz $01					; | Spawn in place of the
	stz $02					; | original shield.
	stz $03					; /

	lda !new_sprite_num,x	; \
	sec						; | Spawn a new shield.
	%SpawnSprite()
	bcs +					; /
	
	lda #$02				; \ Set status
	sta !StatusFlag,y		; / to firing.
	
	stz !StatusFlag,x		; And turn Mario's shield off.
	
+	rts

;==================
; Hitbox Routine
;==================

Hitbox:
    ldy #!SprSize
   
LoopKill:
	cpy $15E9|!Base2	; \ Don't kill self.
	beq +				; /
   
	lda !167A,y			; \
	and #$02			; | Invincible to cape/star/etc checked on tweaker bytes?
	bne +				; / If so, check next slot.
   
	lda !14C8,y			; \
	cmp #$08			; | If not alive, check the next slot. 
	bcc +				; /
	
    jsr ContactCheck	; Run routine for custom hitbox
    jsl $03B6E5|!BankB	; \ Check for contact between it
    jsl $03B72B|!BankB  ; / and the second sprite's hitbox
    bcc +				; If no contact end code.
	
    lda #$04            ; \
    sta !14C8,y         ; | Else spin kill them
    lda #$1F            ; | and spawn the spinkill stars for 1F frames.
    sta !1540,y         ; /

	jsr SpawnStars
	;phx
	;tyx
    ;jsl $07FC3B|!BankB	; Spawn the spinkill contact stars routine.
	;plx
    lda #$08            ; \ Play Sound effect
    sta $1DF9|!Base2    ; /
   
+   dey                 ; \ Check next Sprite.
    bpl LoopKill        ; /
    rts
   
ContactCheck:
    phy : phx           ; \
    tyx                 ; |
 
    lda !E4,x           ; |
    sec : sbc #$2A      ; |
    sta $04             ; |
   
    lda !14E0,x         ; |
    sbc #$00            ; |
    sta $0A             ; |
   
    lda #$54            ; | Set custom hitbox to 12 pixels above and
    sta $06             ; | to the left of current position (Mario's center)
   
    lda !D8,x           ; | and stretch it to 24x24 in terms of width and height.
    sbc #$2A            ; |
    sta $05             ; |
 
    lda !14D4,x         ; |
    sbc #$00            ; |
    sta $0B             ; |
   
    lda #$56            ; |
    sta $07             ; /

    plx : ply           ;
   
	rts
	
;==================
; Graphics routine
;==================

XDisp:
	db $D0,$00,$30,$00		; Frame 1
	db $D6,$12,$2A,$EE		; Frame 2
    db $E0,$20,$20,$E0		; Frame 3
	db $EE,$2A,$12,$D6		; Frame 4

YDisp:
	db $00,$D0,$00,$30		; Frame 1
	db $EE,$D6,$12,$2A		; Frame 2
    db $E0,$E0,$20,$20		; Frame 3
	db $D6,$EE,$2A,$12		; Frame 4

SubGFX:

	lda !DrawOrNot,x				; \
	bne Draw						;  | If DrawOrNot is set ($01), draw graphics.
	rts								; /

Draw:
	%GetDrawInfo()					; Upload OAM Index to Y.
									; Upload Sprite X position relative to screen border to $00.
									; And Sprite Y position relative to screen border to $01.

	lda $14							; \
	and #$0C						; / Preserve 3rd and 4th bits
	sta !OffsetFrame				; Use later for drawing graphics.
	
	lda #$03						;
	sta !LoopCounter				; Loop thrice.
	
	phx
	ldx !LoopCounter				; Load Loop Counter to X

Loop:
	txa
	sta !LoopCounterMirror			; Mirror Loop Counter to use for drawing graphics.

	phx
	lda !LoopCounterMirror			; Add the Loop Counter to the Frame Counter
	clc : adc !OffsetFrame			; to serve as pointer to the X/Y Offsets.
	tax								; 0 + 3 = 3rd value | 4 + 3 = 7th value, | 8 + 3 = 11th value | C + 3 = 15th value
									; Repeat for each tile.
	
	lda $00
	clc : adc XDisp,x
	sta $0300|!Base2,y				; Draw Tile in X Position, offset by index.
	
	lda $01
	clc : adc YDisp,x
	sta $0301|!Base2,y				; Draw Tile in Y position, offset by index.
	plx

	lda #$80
	sta $0302|!Base2,y				; Display tile, indexed by graphics pointer.

	lda #$0B
	ora $64
	sta $0303|!Base2,y				; Add property byte.
	
	iny #4							; Draw the 16x16 tile.

	dex
	bpl Loop						; Loop until $FF
	
	plx								; Reload Sprite Index.
	
	ldy #$02						; Size: 16x16
	lda #$03						; Four tiles.
	jsl $01B7B3|!BankB				; Don't draw when off screen.
	
	rts
	
SpawnStars:
	phy
	phx
	tyx
	lda #$03
	sta $0A
.starloop
	jsr .mainstars
	dec $0A
	bpl .starloop
	plx
	ply
	rts
.mainstars
	lda #$04
	sta $01
	sta $00
	phx
	ldx $0A
	lda .starh,x
	sta $02
	lda .starv,x
	sta $03
	plx
	lda #$10
	%SpawnExtended()
	lda #$17
	sta $176F|!Base2,y
	rts
	
.starh
	db $E0,$20,$E0,$20
.starv
	db $F0,$F0,$10,$10;