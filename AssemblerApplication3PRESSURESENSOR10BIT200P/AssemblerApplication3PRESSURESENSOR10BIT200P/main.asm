;.equ Number = 4294000000 ; slightly above four billions
;	ldi R16,Byte1(Number) ; the lowest 8 bits into register 16
;;	ldi R17,Byte2(Number) ; Bits 9 to 16 into register 17
;	ldi R18,Byte3(Number) ; Bits 17 to 24 into register 18
;	ldi R19,Byte4(Number) ; Bits 25 to 32 into register 19

;Nothing with jumping and branching around, as simple as this. This works with numbers of any size, here we add a four-byte number in R19:R18:R17:R16 with a two-byte number in R21:R20:
;
;	ADD R16,R20 ; Add the lowest byte, result to R16
;	ADC R17,R21 ; Add the next higher byte and carry, result to R17
;	LDI R20,0 ; Write a zero to R20 (do not use CLR, which clears carry!)
;	ADC R18,R20 ; Add zero and carry, result to R18
;	ADC R19,R20 ; Add zero and carry, result to R19
; Program for attiny13A PRESSURE GUAGE with Hitachi 1602LCD and I2C back pack
;uses pressure sensor from 0-300 psi range
;Project compiled by Sajeev Sankaran ,Thodupuzha,Kerala
;thanks for danni from AVR freaks for the binary to ASCII routine 
;Thanks to Kodera2t for I2C routine 
;thanks to Donald Weiman    (weimandn@alfredstate.edu) for delay routines
;
.equ    fclk    = 1000000		; system clock frequency (for delays) 
.DEF ANSL = R0            		;To hold low-byte of answer     
.DEF ANSH = R1            		;To hold high-byte of answer
.DEF    A = R12           		;To hold multiplicand
.DEF    B = R11           		;To hold multiplier
.DEF    C = R10           		;To hold bit counter
.DEF nibble1 = R25  			;register to store data nibble
.DEF command1 = R22
.DEF command2 = R23
.DEF command3 = R24
.DEF command4 = R15
.DEF command5 = R14
.DEF command6 = R13
.DEF loader = R26
.def temp = R19
.def data = R16
.def cnt = R17
.def cnt2 = R18
;.def display = R25
.def temp1 = R20
.def temp2 = R21
.def free1 = R9
.def free2 = R8
.equ count_start = 0x04
.equ count_end = 0x01
.equ subnum = 0x01
.equ	SUB_COUNT	= 0x04
.equ long_delay =0xFF
.equ data_command1 = 0b00001001		; data control nibble ,led on P3, EN 0 on P2, R/W 0 (write) in P1 , RS 1 (0 instruction, 1 data) = 1001  =0x09
.equ data_command2 = 0b00001101		; data control nibble , 1101  = 0x0D   - EN goes hi=1
.equ data_command3 = 0b00001001		; data control nibble , 1001  = 0x09   - EN goes low=0
.equ inst_command1 = 0b00001000		;instruction control nibble ,  led on en-lo,Rw-0,rs =0   = 1000   = 0x08
.equ inst_command2 = 0b00001100		;instruction control nibble ,  led on,EN hi , rs/RW 0    = 1100   = 0x0C
.equ inst_command3 = 0b00001000		;instruction control nibble  , led on, EN lo ,rs/rw 0    = 1000   = 0x08
.equ slave_address = 0b01001110		;0x4E 



; PB0: SDA, PB1: SCL, PB2:A/D input

setup:
; Init stack for subroutines
	ldi loader,LOW(RAMEND) 			; Init stack
	out SPL,loader        			; to stack pointer

	ldi command1,inst_command1		;lower nibble is I2c command  led on en,Rw,rs =0
	ldi command2,inst_command2		;lower nibble led on,EN hi , rs/RW 0
	ldi command3,inst_command3		;loer nibble  led on, EN lo ,rs/rw 0
	ldi loader,data_command1
	mov command4,loader
	ldi loader,data_command2
	mov command5,loader
	ldi loader,data_command3
	mov command6,loader
; delay timer for upto 65 seconds (for main loop operations)----- use command "rcall delayYx1mS" for delay as per enetred value in the brakets in millisecs.

	;ldi YL, low(1000)			;enter desired milliseconds(ms) for the delay needed in the (.......),used for delay in the range of seconds
	;ldi YH, high(1000)			;enter desired milliseconds(ms) for the delay needed in the (.......),used for delay in the range of seconds

; PB0 (SDA) and PB1 (SCL) are tristated for hi and DDR is changed to output for low on I2C bus

	ldi loader, 0b00
	out DDRB, loader				;pb0 & pb1 is now input 
	out portb,loader				;pb0 & pb1 is now tristated , both i2c lines are pulled up by ext resitors to hi

; initially, SDA and SCL are high
	
	rcall lcd_init      			; LCD initialize as per hitachi data sheet HD44780


; starting A/D converter, free running mode
	;ldi	temp1, ((1<<ADEN)+(1<<ADSC)+(1<<ADATE)+(1<<ADIE)+(0<<ADIF)+(1<<ADPS2)+(0<<ADPS1)+(1<<ADPS2))
	ldi loader,0b11110101
	out   ADCSRA,loader
	ldi loader, 0x00
	out ADCSRB, loader
; selecting PB2 for A/D input
	ldi	loader, (1<<MUX1)+(0<<MUX0)+(0<<ADLAR)       ; adc2,PB4  , adc result left adjusted
	out   ADMUX,loader
	ldi	loader, 1<<ADC2D
	out	DIDR0, loader
	ldi temp ,5
	rcall delayTx1uS

main: 	

; reading A/D result in free1
	in free1,ADCL
	in free2,ADCH
	mov loader, free1

;ADCL should be right adjusted.ADLAR=0 ,low in ADCL and high bit in ADCH total 10 bit
	ldi loader,0
	mov r5,loader			;r5 is used to count upto 32 for 32 read averaging from ADC
addroutine:
	in free1,ADCL			; ADCL data is copied to register free1 (r9)
	in free2,ADCH			; ADCH data copied to register free2 (r8)
	mov r28,free1			; copy free1 to r28
	mov r29,free2			; copy free2 to r29
	rcall X1				; call function X1 which will check for ADC value less than 103 and load 103 to show 0 psi in LCD. if ADC value greater than 103 no changes are made.
	mov free1,r28			; corrected and checked ADC value low copied after X1 function call
	mov free2,r29			; corrected and checked ADC value high copied after X1 function call
	add r0,free1			; add routine
	adc r1,free2			; add routine
	ldi loader,0			; add routine
	mov free1,loader		; add routine
	adc r2,free1			; add routine
	adc r3,free1			; add routine

	ldi loader,32			; counter value 32
	inc r5					; increases the counter 0-32 to add 32 adc values for averaging
	cp r5,loader			;  compares counter reached 32
	brne addroutine			; if not 32 loop back to another add of ADC result
	ldi loader,0			; if 32 counts reached 0 loaded into R5 for next average cycle
	mov r5,loader
	

wipeclear:
	
	ldi loader,5			;32 added results need to be bit shifted right for division by 32

loop1:
	
	dec loader				; decrease counter till 0 from 5
	LSR r3
	ror r2
	ror r1
	ror r0
	cpi loader,0
	brne loop1				; loops 5 times for division by 32

	push r16
	push r17
	push r18
	push r19
	push r20
	push r21
	push r27
	push r22
	push r23
	push r24
	push r25

	ldi r24,24				; first part of multiplier 24 (0.244)
	rcall multiplication
	mov r22,r18
	mov r23,r19
	ldi r24,4				; second part of the multiplier 4(0.244)
	rcall multiplication
	ldi loader,3
loop2:
	
	dec loader
	lsr r19
	ror r18
	cpi loader,0
	brne loop2
        add r22,r18
	adc r23,r19
			;averaged and calculated pressure value in R23:R22

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;.def    a0              = r0 = ANSL = R22
;.def    a1              = r1 = ANSH = R23
;.def    a2              = r12 = A   = R16
;.def    a3              = r11 = B   = R17
;.def    a4              = r10 = C   = R18
;************************************************************************
;*                                                                      *
;*                      binary to ASCII decimal conversion              *
;*                                                                      *
;************************************************************************
;input: a1, a0 = 16 bit value 0 ... 65535
;output: a4, a3, a2, a1, a0 = digits
;cycle: 27 .. 183
;bytes: 42
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


binbcd:
	ldi r18,-1 + '0'
        ;mov     C, loader			;0x2F  = -1 decimal + ascii 0 (decimal 48 or 0x30)  decimal 47 or 0x2F
_bib1:  inc     r18					; times operation done
        subi    r22, low(10000)
        sbci    r23, high(10000)    ; if carry flag set
        brcc    _bib1				; branch if carry flag clear till-ve operation (original value less than 10000)

	ldi r17,10 + '0'
        ;ldi     B, loader			;0x3A  , 10 decimal + ascii 0=46 in hex a+30 =3A  = decimal 58
_bib2:  dec     r17					; decrease one count from 58 to reach 48
        subi    r22, low(-1000)		; subtract with -1000 decimal (adds 1000)
        sbci    r23, high(-1000)		; adds 1000 till carry is set
        brcs    _bib2				; branch if carry set

	ldi r16, -1 + '0'
        ;mov     A, loader			;0x2F  = -1 decimal + ascii 0 (decimal 48 or 0x30)  decimal 47 or 0x2F
_bib3:  inc     r16					;times operation done
        subi    r22, low(100)		;
        sbci    r23, high(100)		;subtract 100 till carry flag is set indicating negative number
        brcc    _bib3				;branch if carry flag clear
	
	ldi loader, 10 + '0'
        mov     r23, loader			;0x3A  , 10 decimal + ascii 0=46 in hex a+30 =3A  = decimal 58
_bib4:  dec     r23
        subi    r22, -10
        brcs    _bib4

        subi    r22, -'0'
        ;ret	

mov r0,r22
mov r1,r23
mov r2,r16
mov r3,r17
mov r4,r18

pop r25
pop r24
pop r23
pop r22
pop r27
pop r21
pop r20
pop r19
pop r18
pop r17
pop r16	

	


	ldi loader,0b11000000			;0b1100_000 ; (0xC0) second line,first digit  ,DDRAM address as per data sheet is 0x47 (1000000)+ 10000000 is the DDRAM control code = 0b11000000
	mov nibble1,loader                 	;loads info in loader to nibble register for AND,OR,SWAP etc to write data
	rcall nibble_write_instruction		;calls LCD instruction/command write subroutine

	mov loader,r4					;10000 position ASCII value
	mov nibble1,loader
	rcall nibble_write_data				;calls LCD data write subroutine

	

	mov loader,r3					;1000 position ASCII value
	mov nibble1,loader
	rcall nibble_write_data



	mov loader,r2					;100 position ASCII value
	mov nibble1,loader
	rcall nibble_write_data

	
	ldi loader,0b00101110			;decimal symbol
	mov nibble1,loader
	rcall nibble_write_data


	mov loader,r1					;10 position ASCII value
	mov nibble1,loader
	rcall nibble_write_data



	mov loader,r0					;1 position ASCII value
	mov nibble1,loader
	rcall nibble_write_data


	ldi loader,0b11001000			;load DDRAM address C8 (8th position)
	mov nibble1,loader
	rcall nibble_write_instruction


	ldi loader, 0b01010000			;ASCII for P
	mov nibble1,loader
	rcall nibble_write_data

	ldi loader,0b01010011			;ASCII for S
	mov nibble1,loader
	rcall nibble_write_data

	ldi loader,0b01001001			;ASCII for I
	mov nibble1,loader
	rcall nibble_write_data


    clr r0
	clr r1
	clr r2
	clr r3
	
 	rjmp main



start:
	ldi temp1, 0b01       			;scl 1 , sda 0
	out ddrb, temp1
	rcall delay10uS
	ldi temp1, 0b11					;scl 0 , sda 0
	out ddrb, temp1
	rcall delay10uS
	ret
	;cl, da
		
ends:
	ldi temp1,0b11					;both scl & sda pulled low so as next step ensures a low to hi transition of SDA for stop
	out ddrb, temp1					; scl 0 , sda 0
	rcall delay10uS
	ldi temp1,0b01					;scl 1 ,sda 0
	out ddrb, temp1					;while scl is hi ,sda is low , next step sda will go hi creating a stop condition
	rcall delay10uS
	ldi temp1,0b00					;scl 1 . sda 1
	out ddrb, temp1					;sda reached hi from low in the previous step
	rcall delay10uS	
	ret

init:
	ldi temp1, 0b00 				; 
	out ddrb,temp1					; data direction set as input = SCL & SDA now hi
	out PORTB, temp1				; ports also loaded with 0 , Zstate
	rcall delay10uS
	ret
	;cl da
bit_high:
	ldi temp1, 0b11    				; scl 0 ,sda 0
	out ddrb, temp1					; writing 1 to ddr will make it output and alreday port is 0 so SDA & SCL is pulled low
	rcall delay10uS
	ldi temp1, 0b10					; scl 0 ,sda 1
	out ddrb, temp1
	rcall delay10uS
	ldi temp1, 0b00					;scl 1 , sda 1
	out ddrb, temp1
	rcall delay10uS
	ldi temp1, 0b10					; scl 0 , sda 1
	out ddrb, temp1
	rcall delay10uS
	ldi temp1, 0b11					; scl 0 ,sda 0
	out ddrb, temp1
	rcall delay10uS
	ret
	;cl=1 da=0
	
bit_low:
	ldi temp1, 0b11					;scl 0 ,sda 0
	out ddrb, temp1
	rcall delay10uS
	ldi temp1, 0b11					;scl 0 , sda 0
	out ddrb, temp1
	rcall delay10uS
	ldi temp1, 0b01					; scl 1 , sda 0
	out ddrb, temp1
	rcall delay10uS
	ldi temp1, 0b11					; scl 0 , sda 0
	out ddrb, temp1
	rcall delay10uS
	ldi temp1, 0b11					; scl 0 , sda 0
	out ddrb, temp1
	rcall delay10uS
	ret
	;cl da

ack:
	ldi temp1, 0b10    				; scl 0 , sda 1
	out ddrb,temp1
;recheck:	
	;sbic pinb,pinb0				; if pinb0 is 0 then slave ACKed
	;rjmp recheck					; if pinb0 is 1 wait for ACK
	rcall delay10uS
	ldi temp1, 0b00					; scl 1, sda 1 ( if ACK sda will become 0 when slave pulls low)
	out ddrb, temp1
	rcall delay10uS
	ldi temp1, 0b10
	out ddrb, temp1
	rcall delay10uS
	ldi temp1, 0b11					; scl 0 , sda 0 
	out ddrb, temp1
	rcall delay10uS
	ret
	
	

; writing data is stored in data
writedata:
	ldi cnt,0x00
	ldi cnt2,0x02
rep:
	mov temp2, data
	andi temp2,0b10000000
	cpi temp2, 0b10000000
	breq highbit
	rcall bit_low
	rjmp sendend
highbit:
	rcall bit_high
sendend:
	lsl data
	inc cnt
	cpi cnt,8
	brne rep
	ldi temp1, 0b10					; scl 0 , sda 1 ( sda released for slave to send ACK)
	out ddrb, temp1
	ret

nibble_write_instruction:
	rcall init
	rcall start
	ldi data,slave_address
	rcall writedata
	rcall ack
	ANDi nibble1,0XF0               ;upper nibble is preserved in register nibble1
	OR nibble1,command1
	mov data,nibble1
	rcall writedata
	rcall ack
	mov nibble1,loader             	;copies data for upper nibble operation
	ANDi nibble1,0XF0               ;upper nibble is preserved in register nibble1
	OR nibble1,command2
	mov data,nibble1
	rcall writedata
	rcall ack
	mov nibble1,loader             	;copies data for upper nibble operation
	ANDi nibble1,0XF0               ;upper nibble is preserved in register nibble1
	OR nibble1,command3
	mov data,nibble1
	rcall writedata
	rcall ack
	mov nibble1,loader
	swap nibble1                    ;previous lower nibble is swapped to high nibble position
	ANDi nibble1,0XF0               ;upper nibble is preserved in register nibble1
	OR nibble1,command1
	mov data,nibble1
	rcall writedata
	rcall ack
	mov nibble1,loader            	;copies data for upper nibble operation
	swap nibble1                    ;previous lower nibble is swapped to high nibble position
	ANDi nibble1,0XF0               ;upper nibble is preserved in register nibble1
	OR nibble1,command2
	mov data,nibble1
	rcall writedata
	rcall ack
	mov nibble1,loader
	swap nibble1                    ;previous lower nibble is swapped to high nibble position
	ANDi nibble1,0XF0               ;upper nibble is preserved in register nibble1
	OR nibble1,command3
	mov data,nibble1
	rcall writedata
	rcall ack
	rcall ends
	ldi temp,20
 	rcall delayTx1mS
	ret





nibble_write_data:
	rcall init
	rcall start
	ldi data,slave_address
	rcall writedata
	rcall ack
	ANDi nibble1,0XF0               ;upper nibble is preserved in register nibble1
	OR nibble1,command4				;adding the upper nibble and LCD commands as lower bits of the 8 bits in write data
	mov data,nibble1
	rcall writedata
	rcall ack
	mov nibble1,loader             	;copies data for upper nibble operation
	ANDi nibble1,0XF0               ;upper nibble is preserved in register nibble1
	OR nibble1,command5
	mov data,nibble1
	rcall writedata
	rcall ack
	mov nibble1,loader             	;copies data for upper nibble operation
	ANDi nibble1,0XF0               ;upper nibble is preserved in register nibble1
	OR nibble1,command6
	mov data,nibble1
	rcall writedata
	rcall ack
	mov nibble1,loader
	swap nibble1                    ;previous lower nibble is swapped to high nibble position
	ANDi nibble1,0XF0               ;upper nibble is preserved in register nibble1
	OR nibble1,command4
	mov data,nibble1
	rcall writedata
	rcall ack
	mov nibble1,loader            	;copies data for upper nibble operation
	swap nibble1                    ;previous lower nibble is swapped to high nibble position
	ANDi nibble1,0XF0               ;upper nibble is preserved in register nibble1
	OR nibble1,command5
	mov data,nibble1
	rcall writedata
	rcall ack
	mov nibble1,loader
	swap nibble1                    ;previous lower nibble is swapped to high nibble position
	ANDi nibble1,0XF0               ;upper nibble is preserved in register nibble1
	OR nibble1,command6
	mov data,nibble1
	rcall writedata
	rcall ack
	rcall ends
	ldi temp,5
	rcall delayTx1uS
	ret



	

lcd_init:
	ldi temp,50
	rcall delayTx1mS      				;50ms delay as part of startup
	
	rcall init
	rcall start
	ldi data,slave_address         		;0x27 and write bit =0x4E
	rcall writedata						;writedata subroutine is called to command PCF8574T i2c chip only during initialization, any 4 bit LCD is done by instruction_write or data_write subroutine after initialisation
	rcall ack
	ldi data,0b00111100         		;Function set LCD , high nibble 0011 & lower commands led = 1,EN,RW,RS all 0.
	rcall writedata						;writedata subroutine is called to command PCF8574T i2c chip only during initialization, any 4 bit LCD is done by instruction_write or data_write subroutine after initialisation
	rcall ack
	ldi data,0b00111000         		;Function set LCD , high nibble 0011 & lower commands led = 1,EN,RW,RS all 0.
	rcall writedata						;writedata subroutine is called to command PCF8574T i2c chip only during initialization, any 4 bit LCD is done by instruction_write or data_write subroutine after initialisation
	rcall ack
	rcall ends
	ldi temp,20							; value loaded here (20) decides the number of milli seconds in the delay below
 	rcall delayTx1mS					;20 milli seconds delay, slightly above hitachi data sheet 

	
	rcall init
	rcall start
	ldi data,slave_address         		;0x27 and write bit =0x4E
	rcall writedata
	rcall ack
	ldi data,0b00111100         		;Function set LCD , high nibble 0011 & lower commands led = 1,EN,RW,RS all 0.
	rcall writedata
	rcall ack
	ldi data,0b00111000         		;Function set LCD , high nibble 0011 & lower commands led = 1,EN,RW,RS all 0.
	rcall writedata
	rcall ack
	rcall ends
	ldi temp,20
 	rcall delayTx1mS					;20 milli seconds delay, slightly above hitachi data sheet
	
	
	
	rcall init
	rcall start
	ldi data,slave_address         		;0x27 and write bit =0x4E
	rcall writedata						;writedata subroutine is called to command PCF8574T i2c chip only during initialization, any 4 bit LCD is done by instruction_write or data_write subroutine after initialisation
	rcall ack
	ldi data,0b00111100         		;Function set LCD , high nibble 0011 & lower commands led = 1,EN,RW,RS all 0.
	rcall writedata
	rcall ack
	ldi data,0b00111000         		;Function set LCD , high nibble 0011 & lower commands led = 1,EN,RW,RS all 0.
	rcall writedata
	rcall ack
	rcall ends
	ldi temp,20
 	rcall delayTx1mS					;20 milli seconds delay, slightly above hitachi data sheet

	rcall init
	rcall start
	ldi data,slave_address         		;0x27 and write bit =0x4E
	rcall writedata						;writedata subroutine is called to command PCF8574T i2c chip only during initialization , any 4 bit LCD is done by instruction_write or data_write subroutine
	rcall ack
	ldi data,0b00101100         		;Function set LCD , high nibble 0011 & lower commands led = 1,EN,RW,RS all 0.
	rcall writedata						;writedata subroutine is called to command PCF8574T i2c chip only during initialization , any 4 bit LCD is done by instruction_write or data_write subroutine
	rcall ack
	ldi data,0b00101000         		;Function set LCD , high nibble 0011 & lower commands led = 1,EN,RW,RS all 0.
	rcall writedata						;writedata subroutine is called to command PCF8574T i2c chip only , any 4 bit LCD is done by instruction_write or data_write subroutine
	rcall ack
	rcall ends
	ldi temp,20
 	rcall delayTx1mS					;20 milli seconds delay, slightly above hitachi data sheet



	
 
    ldi loader,0b00101000            	; (0x28 4 bit ,2 line ,5x7)Function set LCD ,
	mov nibble1,loader					;loads info in loader to nibble register for AND,OR,SWAP etc to write data
	rcall nibble_write_instruction 		;calls LCD instruction/command write subroutine
			
	
	ldi loader,0b00001110         		;Function set LCD , (0x0C ). DISPLAY ON
	mov nibble1,loader					;loads info in loader to nibble register for AND,OR,SWAP etc to write data
	rcall nibble_write_instruction		;calls LCD instruction/command write subroutine

	
	ldi loader,0b00000110        		;Function set LCD  (entry mode set 0x06),  ENTRY MODE SET
	mov nibble1,loader					;loads info in loader to nibble register for AND,OR,SWAP etc to write data
	rcall nibble_write_instruction		;calls LCD instruction/command write subroutine
	
	
		
	ldi loader,0b00000001         		;Function set LCD  (clear display 0x01),  CLEAR DISPLAY
	mov nibble1,loader					;loads info in loader to nibble register for AND,OR,SWAP etc to write data
	rcall nibble_write_instruction		;calls LCD instruction/command write subroutine
	

	ret







; ============================== Time Delay Subroutines =====================
; Name:     delayYx1mS
; Purpose:  provide a delay of (YH:YL) x 1 mS
; Entry:    (YH:YL) = delay data
; Exit:     no parameters
; Notes:    the 16-bit register provides for a delay of up to 65.535 Seconds
;           requires delay1mS

delayYx1mS:
    rcall    delay1mS                        ; delay for 1 mS
    sbiw    YH:YL, 1                        ; update the the delay counter
    brne    delayYx1mS                      ; counter is not zero

; arrive here when delay counter is zero (total delay period is finished)
    ret
; ---------------------------------------------------------------------------
; Name:     delayTx1mS
; Purpose:  provide a delay of (temp) x 1 mS
; Entry:    (temp) = delay data
; Exit:     no parameters
; Notes:    the 8-bit register provides for a delay of up to 255 mS
;           requires delay1mS

delayTx1mS:
    rcall    delay1mS                        ; delay for 1 mS
    dec     temp                            ; update the delay counter
    brne    delayTx1mS                      ; counter is not zero

; arrive here when delay counter is zero (total delay period is finished)
    ret

; ---------------------------------------------------------------------------
; Name:     delay1mS
; Purpose:  provide a delay of 1 mS
; Entry:    no parameters
; Exit:     no parameters
; Notes:    chews up fclk/1000 clock cycles (including the 'call')

delay1mS:
    push    YL                              ; [2] preserve registers
    push    YH                              ; [2]
    ldi     YL, low(((fclk/1000)-18)/4)     ; [1] delay counter              (((fclk/1000)-18)/4)
    ldi     YH, high(((fclk/1000)-18)/4)    ; [1]                            (((fclk/1000)-18)/4)

delay1mS_01:
    sbiw    YH:YL, 1                        ; [2] update the the delay counter
    brne    delay1mS_01                     ; [2] delay counter is not zero

; arrive here when delay counter is zero
    pop     YH                              ; [2] restore registers
    pop     YL                              ; [2]
    ret                                     ; [4]

; ---------------------------------------------------------------------------
; Name:     delayTx1uS
; Purpose:  provide a delay of (temp) x 1 uS with a 16 MHz clock frequency
; Entry:    (temp) = delay data
; Exit:     no parameters
; Notes:    the 8-bit register provides for a delay of up to 255 uS
;           requires delay1uS

delayTx1uS:
    rcall    delay10uS                        ; delay for 1 uS
    dec     temp                            ; decrement the delay counter
    brne    delayTx1uS                      ; counter is not zero

; arrive here when delay counter is zero (total delay period is finished)
    ret

; ---------------------------------------------------------------------------
; Name:     delay10uS
; Purpose:  provide a delay of 1 uS with a 16 MHz clock frequency ;MODIFIED TO PROVIDE 10us with 1200000cs chip by Sajeev
; Entry:    no parameters
; Exit:     no parameters
; Notes:    add another push/pop for 20 MHz clock frequency

delay10uS:
    ;push    temp                            ; [2] these instructions do nothing except consume clock cycles
    ;pop     temp                            ; [2]
    ;push    temp                            ; [2]
    ;pop     temp                            ; [2]
    ;ret                                     ; [4]
     nop
     nop
     nop
     ret

; ============================== End of Time Delay Subroutines ==============


multiplication:
;;;;;;;;;;;;;;;;;;;;;;;;;;; 103/0x67 is deducted at the time of addition routine to correct the sensor off set
;;;;;;;;;;;;;;;;;;;;;;;;;;;(5/1023) x (200/4) x ADC count = pressure measured
;;;;;;;;;;;;;;;;;;;;;;;;;;; the sensor is 200 psi, 0psi at 0.5v and 200psi at 4.5V
;;;;;;;;;;;;;;;;;;;;;;;;;;; we subtract 103/0x67 from the ADC reading as 0.5v=103ADC to get 0 psi
;;;;;;;;;;;;;;;;;;;;;;;;;;; when 103 subtracted from 4.5v=920ADC we get 199.48PSI, 1023/10=102.5 ADC readings per 0.5 volts
;;;;;;;;;;;;;;;;;;;;;;;;;;;0.244 x ADC count , ADC count x 24 + ADC count x 4 >>>3 gives a corrected value
	; Numbers in R17:R16 and R21
;registers used ,r16,r17,r18,r19,r20,r21,r27
	ldi loader,0x03
	and r1,loader		;extract the 2 low bytes from the high register =ADC high bits ADCH
	mov loader,r1
	mov r17,loader		; hi bit loaded in high side of register pair of multiplicand
	mov loader,r0		; lo bit of averaged ADC value to low byte of the mulitiplicand register pair
	mov r16,loader		; same as above
	clr r27				; r27 is needed to shift the number left, zero at start
	mov r21,r24			; multiplier 0.293 ,29 first then with 3 later and add after bitshift
	clr R20				; result here
	clr R19				; result here
	clr R18				; result here
		

MultLoop:
	lsr R21				; shift least significant bit to carry
	brcc MultWoAdd		; if clear skip adding
	add R18,R16			; Add number to result, lowest byte
	adc R19,R17			; Add number with carry to result, second byte
	adc R20,R27			; Add number with carry to result, third byte
MultWoAdd:
	lsl R16				; Shift number left, bit 0 = 0, bit 7 to carry
	rol R17				; Roll left, bit 0 = carry, bit 7 to carry
	rol R27				; Roll left, bit 0 = carry, bit 7 to carry
	tst R21				; Multiplication complete?
	brne MultLoop		; No, repeat
						; Result is in R20:R19:R18, done ,1023 being highest count 29667 will be 15 bit answer of 1023 x 29
						; next add 1023 x 3 after 3 LSR to the previous number to get multiplication by 293
						; then put decimal where needed in display to get .293 mul
	ret

X1:
	sbiw r29:r28,63		; wnted to minus 103 but sbiw allows constants upto decimal 63. first minus 63 and next 40 to achive 103
	sbiw r29:r28,40		; first 63 the 40 to acive subtraction of 103 from register pair as max sbiw is 63 decimal
	brlt X2
	ret
X2:
	ldi r28,0
	ldi r29,0
	ret