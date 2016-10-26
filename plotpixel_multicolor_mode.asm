;------------------------------------------------------------------------------
;  plotpixel_multicolor_mode.asm
;
;  This sample shows how to (slowly) set a single pixel in multicolor mode.The
;  implementation is a straight conversion of the Basic example from the
;  manual. It just calculates the address of the pixel in the video memory (No
;  lookup table). The address byte of the pixel is calculated this way:
;
;       BYTE = INT(X/8) * 8 + INT(Y/8) * 320 + (Y AND 7)
;
;  The bit within the byte will be switched on with:
;
;       BIT  = 7 - (X AND 7)
;
;  (C) 2016 Christian Bleicher
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; global constants and variables
;------------------------------------------------------------------------------
; VIC related constants
CE_VIC_BANK_IDX             = 0                                             ; index (0 - 3) of the 16KB bank (default = 0)
CE_VIC_BASE_ADDR            = CE_VIC_BANK_IDX * 16384                       ; startaddress of the currently used VIC bank (default = $0000)

CE_VIC_CHARSET_BLOCK_IDX    = 2                                             ; index (0 - 7) of the 2KB charset memory block (default = 2)
CE_VIC_CHARSET_ADDR_BASE    = CE_VIC_CHARSET_BLOCK_IDX * 2048               ; address of the charset (default = $1000)

CE_VIC_SCREEN_BLOCK_IDX     = 1                                             ; index (0 -15) of the 1KB screen memory block (default = 1)
CE_VIC_SCREENMEM_BASE       = CE_VIC_SCREEN_BLOCK_IDX * 1024                ; base address of the screen memory relative to the bank
CE_VIC_SCREENMEM            = CE_VIC_SCREENMEM_BASE + CE_VIC_BASE_ADDR      ; absolute address of the screen memory

CE_VIC_BITMAP_BLOCK_IDX     = 1                                             ; index (0 - 1) of the 8KB bitmap memory block (default = 1)
CE_VIC_BITMAP_ADDR_BASE     = CE_VIC_BITMAP_BLOCK_IDX * 8192                ; absolute address of the bitmap memory
CE_VIC_BITMAP_ADDR          = CE_VIC_BASE_ADDR + CE_VIC_BITMAP_ADDR_BASE

; zero page help variables
CE_ZP_AUX_REG_A             = $FB
CE_ZP_AUX_REG_B             = $FC
CE_ZP_AUX_REG_C             = $FD
CE_ZP_AUX_REG_D             = $FE
CE_ZP_AUX_REG_E             = $FF

;------------------------------------------------------------------------------
; simple basic start (adds the line '2016SYS2062' at the start of the prg)
; The program will be loaded to 2049 (0x0801). Add the 13 Bytes of the line
; "$0B,$08,$F0,$07,$9E,"2062",$00,$00,$00,$00" and you get the startaddress of
; the assembly code = 2062. '$9E' is the bytecode for the 'SYS' command.
; '$F0,$07' is 0x07F0 = '2016'. '$0B,$08' is the start of the next basic line,
; wich is after '2062' (this is where the zero padding bytes start).
;------------------------------------------------------------------------------
*=$0801
    .byte $0B,$08,$F0,$07,$9E,"2062",$00,$00,$00,$00

;------------------------------------------------------------------------------
; program entry
;------------------------------------------------------------------------------
main
    ; enable bitmap mode
    lda $D011
    ora #%00100000
    sta $D011

    ; enable multicolor mode
    lda $D016
    ora #%00010000
    sta $D016

    ; select address relative to the bank
    ; $D018 = %xxxx0xxx -> bitmap is at $0000
    ; $D018 = %xxxx1xxx -> bitmap is at $2000
    lda $D018
    and #%11110111
    ora #CE_VIC_BITMAP_BLOCK_IDX * 8
    sta $D018

    ; clear screen and color memory
    jsr ceClear

;------------------------------------------------------------------------------
; main loop
;------------------------------------------------------------------------------
mainloop

    ; draw a quad
    lda #32
    sta CE_ZP_AUX_REG_C
line
    ldx #16
    stx CE_ZP_AUX_REG_D
column
    ldx CE_ZP_AUX_REG_D
    ldy CE_ZP_AUX_REG_C
    jsr ceSetPixel
    inc CE_ZP_AUX_REG_D
    lda #80
    cmp CE_ZP_AUX_REG_D
    bne column

    inc CE_ZP_AUX_REG_C
    lda #164
    cmp CE_ZP_AUX_REG_C
    bne line

    ; draw single pixels at the edges of the screen
    ldx #159
    ldy #0
    jsr ceSetPixel
    ldx #159
    ldy #100
    jsr ceSetPixel
    ldx #159
    ldy #199
    jsr ceSetPixel
    ldx #0
    ldy #0
    jsr ceSetPixel
    ldx #0
    ldy #100
    jsr ceSetPixel
    ldx #0
    ldy #199
    jsr ceSetPixel

    jmp mainloop

;------------------------------------------------------------------------------
; Name: ceSetPixel
;
; Desc: set pixel routine
;
;       x position = x register   range: 0 - 159
;       y position = Y register   range: 0 - 199
;
;       %01 - use color from the upper 4-BITs of the screen memory at $0400
;       (lda #$f1 -> #$UPPER|LOWER, 'f' is grey)
;
;       BYTE = INT(X/8) * 8 + INT(Y/8) * 320 + (Y AND 7)
;       BIT  = 7 - (X AND 7)
;------------------------------------------------------------------------------
ceSetPixel
    lda #<CE_VIC_BITMAP_ADDR
    sta CE_ZP_AUX_REG_A
    lda #>CE_VIC_BITMAP_ADDR
    sta CE_ZP_AUX_REG_B
    txa                         ; transfer X to accumulator
    pha                         ; push accumulator to stack
    ; INT(X/8) * 8
    and #%11111100              ; remove the lower 2 bits (index of the pixel 0-3 within a char)
    asl                         ; multiply accumulator with 2 by left shifting (sets carry if value is bigger than 127)
    bcc skip                    ; branch to @skip on carry clear
    inc CE_ZP_AUX_REG_B         ; increment msb if x position is bigger than 127
    clc
skip
    adc CE_ZP_AUX_REG_A         ; in case the bitmap is not starting at an multiple of 256 add lsb to accumulator
    sta CE_ZP_AUX_REG_A
    bcc skip1
    inc CE_ZP_AUX_REG_B
skip1
    ; INT(Y/8) * 320
    tya                         ; transfer Y to accumulator
    pha                         ; push accumulator to stack
    lsr
    lsr
    lsr                         ; divide accumulator by 8 by right shifting it three times
    beq skip2                   ; if the result of the shifts is 0 (means y is smaller than 8 -> we are in the first line) branch to @skip2
    tay                         ; transfer accumulator to Y
loop
    clc
    lda CE_ZP_AUX_REG_A         ; add 320 bytes for every line
    adc #64                     ; 64 bytes lsb + 1 * 256 bytes msb = 320 bytes
    sta CE_ZP_AUX_REG_A
    lda CE_ZP_AUX_REG_B
    adc #1                      ; if carry is set, one will be added
    sta CE_ZP_AUX_REG_B
    dey
    bne loop
skip2
    pla                         ; pull y coord from stack
    and #%00000111
    tay
    pla                         ; pull x coord from stack
    and #%00000011
    tax
    lda #0
    sec                         ; set carry
loop1
    ror
    ror
    dex
    bpl loop1
    ora (CE_ZP_AUX_REG_A), Y
    sta (CE_ZP_AUX_REG_A), Y
    rts

;------------------------------------------------------------------------------
; Name: ceClear
;
; Desc: clear the bitmap memory
;------------------------------------------------------------------------------
ceClear
    jsr ceClearColor
    lda #<CE_VIC_BITMAP_ADDR    ; store bitmap address in variables
    sta CE_ZP_AUX_REG_A
    lda #>CE_VIC_BITMAP_ADDR
    sta CE_ZP_AUX_REG_B
    ldx #32                     ; 32 pages (32 * 256 = 8192 = 8KB)
    ldy #0
    tya                         ; set accumulator to 0
loop2
    sta (CE_ZP_AUX_REG_A), Y    ; store value in accumulator to (CE_ZP_AUX_REG_A + CE_ZP_AUX_REG_B) + Y
    dey                         ; if Y is 0 'dey' sets it to 255
    bne loop2
    inc CE_ZP_AUX_REG_B
    dex
    bne loop2
    rts

;------------------------------------------------------------------------------
; Name: ceClearColor
;
; Desc: clear color ram at $0400
;
;       the pixel color is stored in two bits:
;       %00 - use background color from $D021
;       %01 - use color from the upper 4-BITs of the screen memory at $0400
;       %10 - use color from the lower 4-BITs of the screen memory at $0400
;       %11 - from color ram at $D800
;------------------------------------------------------------------------------
ceClearColor
    lda #11
    sta $D021                       ; set background color to 11 = grey
    ldx #$0
    lda #$f1                        ; set clearcolor: f = pixelcolor (lightgrey), 1 = background (white)
loop3
    sta CE_VIC_SCREENMEM, X         ; 1. page screenmemory
    sta CE_VIC_SCREENMEM + 256, X   ; 2. page screenmemory
    sta CE_VIC_SCREENMEM + 512, X   ; 3. page screenmemory
    sta CE_VIC_SCREENMEM + 768, X   ; 4. page screenmemory
    dex
    bne loop3
    rts