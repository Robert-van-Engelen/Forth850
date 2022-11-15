;-------------------------------------------------------------------------------
;
;         FFFFFFFF  OOOOOO  RRRRRRR TTTTTTTT HH    HH   88888   55555555   0000
;        FF       OO    OO RR    RR   TT    HH    HH  88   88  55        00  00
;       FF       OO    OO RR    RR   TT    HH    HH 88     88 55       00    00
;      FF       OO    OO RR    RR   TT    HH    HH  88   88  55       00    00
;     FFFFFF   OO    OO RRRRRRR    TT    HHHHHHHH   88888   5555555  00    00
;    FF       OO    OO RR RR      TT    HH    HH  88   88        55 00    00
;   FF       OO    OO RR  RR     TT    HH    HH 88     88       55 00    00
;  FF       OO    OO RR   RR    TT    HH    HH  88   88  55    55  00  00
; FF        OOOOOO  RR    RR   TT    HH    HH   88888    555555    0000    v0.9
;
;
; Author:
;   Dr. Robert van Engelen
;
;-------------------------------------------------------------------------------
;
; BSD 3-Clause License
;
; Copyright (c) 2022, Robert van Engelen
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;
; 1. Redistributions of source code must retain the above copyright notice, this
;    list of conditions and the following disclaimer.
;
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
;
; 3. Neither the name of the copyright holder nor the names of its
;    contributors may be used to endorse or promote products derived from
;    this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;
;		FORTH850 CPU REGISTERS AND RAM USAGE
;
;-------------------------------------------------------------------------------
;
;	A	unassigned, available as temp
;	BC	instruction pointer (IP)
;	DE	top of stack (TOS)
;	HL	unassigned, avaialble as temp
;	SP	parameter stack pointer
;	IX	unassigned, avaialble as temp
;	IY	address of the next routine used with jp (iy)
;	[rp]	return stack pointer in RAM
;
;	A' BC' DE' HL' not used by an interrupt routine, available as temps
;
;-------------------------------------------------------------------------------

.title		Forth850
.list		(me)
.area		PROGRAM (ABS)
.org		0x0100

FAST = 1	; fast (1) or compact (0)
FULL = 0	; include additional standard definitions
TEST = 0	; include tests.asm

;-------------------------------------------------------------------------------
;
;		SCREEN
;
;-------------------------------------------------------------------------------

win_rows	.equ 6
win_cols	.equ 24

;-------------------------------------------------------------------------------
;
;		SYSCALLS
;
;-------------------------------------------------------------------------------

CLRLN		.equ 0x84f7		; clear bottom line
GETCHR		.equ 0xbcc4		; returns A with ascii key code
GETKEY		.equ 0xbcfd		; returns A with key code
RDPSTR		.equ 0xbd00		; read pixel string HL size B at DE=xy
REGOUT		.equ 0xbd03		; display CPU registers, wait for key
PUTCHR		.equ 0xbe62		; put A=char at DE=xy, DE=xy unchanged
INSLN		.equ 0xbe65		; insert line at DE=xy
INKEY		.equ 0xbe53		; returns A with key code cf=1 or 0 cf=0
WRPSTR		.equ 0xbfd0		; draw pixel string HL size B at DE=xy
SCROLL		.equ 0xbfeb		; scroll screen up
REPCHR		.equ 0xbfee		; put A=char at DE=xy B times
PUTSTR		.equ 0xbff1		; put string HL=addr length B at DE=xy
SHTDWN		.equ 0xbd2d		; power off, ON restarts in RUN mode

;-------------------------------------------------------------------------------
;
;		CONFIGURATION
;
;-------------------------------------------------------------------------------

pad_size	.equ 256		; PAD size (do not change)
tib_size	.equ 256		; TIB size (do not change)
tmp_size	.equ 256		; TMP string buffer size (do not change)
r_size		.equ 256		; return stack size (adjustable)
s_size		.equ 256		; parameter stack size (adjustable)
h_size		.equ 40			; hold space size (adjustable)

;-------------------------------------------------------------------------------
;
;		MEMORY LAYOUT
;
;-------------------------------------------------------------------------------

USER		.equ 0x7ffe		; MON USER+1 allocated space for FORTH
PAD0		.equ 0x7b00		; PAD buffer
TIB0		.equ 0x7c00		; terminal input buffer
TMP0		.equ 0x7d00		; temporary string buffer 0 (base temp)
TMP1		.equ 0x7e00		; temporary string buffer 1 (next temp)

;-------------------------------------------------------------------------------
;
;		WORD CONTROL BITS
;
;-------------------------------------------------------------------------------

length_bits	.equ 0x3f		; word length bitmask and max length
smudge_bit	.equ 6			; smudge bit, should be bit 6
smudge_bits	.equ 1<<smudge_bit	; smudge bitmask
immediate_bit	.equ 7			; immediate bit, must be bit 7
immediate_bits	.equ 1<<immediate_bit	; immediate bitmask

;-------------------------------------------------------------------------------
;
;		FIG FORTH VOCABULARY KLUDGE
;
;-------------------------------------------------------------------------------

fig_kludge	.equ 0x2001|smudge_bits	; a blank name with smudge bit set

;-------------------------------------------------------------------------------
;
;		FORTH SYS CONSTANTS
;
;-------------------------------------------------------------------------------

colon_sys	.equ 0xfdef
do_sys		.equ 0xfedf
dest		.equ 0xfdee
orig		.equ 0xfeed

;-------------------------------------------------------------------------------
;
;		ASSEMBLY MACROS
;
;-------------------------------------------------------------------------------

; Compile a code definition header

last_link = 0

.macro		CODE name,label
		link = last_link
		last_link = .
		.nchr len,^|name|
		.dw link
		.db len
		.str ^|name|
label:
.endm

; Compile an immediate code definition header

.macro		CODE_IMM name,label
		link = last_link
		last_link = .
		.nchr len,^|name|
		.dw link
		.db len|immediate_bits
		.str ^|name|
label:
.endm

; Compile a colon definition header

.macro		COLON name,label
		CODE ^|name|,label
		call docol
.endm

; Compile an immediate colon definition header

.macro		COLON_IMM name,label
		CODE_IMM ^|name|,label
		call docol
.endm

; Compile a user variable header

.macro		VARIABLE name,label
		CODE ^|name|,label
		call dovar
.endm

; Compile a user constant header

.macro		CONSTANT name,label
		CODE ^|name|,label
		call docon
.endm

; Compile a user value header

.macro		VALUE name,label
		CODE ^|name|,label
		call doval
.endm

; Compile a user double value header

.macro		TWOVALUE name,label
		CODE ^|name|,label
		call dotwoval
.endm

; Compile the next routine

.macro		JP_NEXT
		jp (iy)		;  8(46); jump to next routine
.endm

.macro		NEXT
.if FAST
		ld a,(bc)	;  7	;
		ld l,a		;  4	;
		inc bc		;  6	;
		ld a,(bc)	;  7	;
		ld h,a		;  4	;
		inc bc		;  6	; [ip++]->hl with xt
		jp (hl)		;  4(38); jump to hl
.else
		JP_NEXT		;  8(46); jump to next routine
.endif
.endm

; Compile an inline string literal

.macro		SLIT string
		.nchr len,^|string|
		.dw doslit
		.db len
		.str ^|string|
.endm

;-------------------------------------------------------------------------------
;
;		ENTRY POINT BOOT UP
;
;-------------------------------------------------------------------------------

boot::		ld (bsp),sp		; save BASIC sp to [bsp]
		ld hl,(rp0)		;
		ld (rp),hl		; set [rp0]->rp FORTH RP
		ld de,-r_size		;
		add hl,de		; [rp0]-r_size->hl
		ld (sp0),hl		; set hl->[sp0]
		ld sp,hl		; set [sp0]->sp FORTH parameter sp
		ex de,hl		;
		ld hl,-1		;
		sbc hl,sp		;
		ld (sp1),hl		; -1-[sp0]->[sp1] sp for overflow check
		ex de,hl		;
		ld de,-s_size-h_size	;
		add hl,de		; [sp0]-s_size-h_size->hl
		ld (top),hl		; hl->[top]
		ld iy,next		; set iy to next for jp (iy) in JP_NEXT
		call docol		; (:) start interpreting
		.dw decimal
		.dw page
		    SLIT ^|FORTH|
		.dw type
		.dw unused,dolit,7,udotr
		    SLIT ^| bytes free|
		.dw type
		.dw repl

rp0		.equ USER
sp0:		.dw 0			; sp0
sp1:		.dw 0			; sp1=-1-sp0
top:		.dw 0			; dictionary top
bsp:		.dw 0			; saved BASIC stack pointer

;-------------------------------------------------------------------------------
;
;		START OF THE DICTIONARY
;
;-------------------------------------------------------------------------------

; (:)		-- ; R: -- ip
;		call colon definition;
;		runtime of the : compile-only word

		CODE (:),docol
		ld hl,(rp)	; 16	; [rp]->hl
		dec hl		;  6	;
		ld (hl),b	;  7	;
		dec hl		;  6	;
		ld (hl),c	;  7	; save bc->[--rp] with caller ip on the return stack
		ld (rp),hl	; 16	; ip-2->rp
		pop bc		; 10(68); pop ip saved by call docol
;		continue with ON/BREAK key check
cont:		in a,(0x1f)	; 11	; port 0x1f bit 7 is set if ON/BREAK is depressed
		add a		;  4	; test ON/BREAK key
		jr c,break	;  7(22); if ON/BREAK pressed then break
;		next
next:		ld a,(bc)	;  7	;
		ld l,a		;  4	;
		inc bc		;  6	;
		ld a,(bc)	;  7	;
		ld h,a		;  4	;
		inc bc		;  6	; [ip++]->hl with xt
		jp (hl)		;  4(38); jump to hl
;		break
break:		call INKEY		; INKEY
		jr c,break		; repeat INKEY while a key is depressed
		ld a,-28		;
		jp throw_a		; throw -28 "user interrupt"

; (;)		-- ; R: ip --
;		return to caller from colon definition;
;		runtime of the ; compile-only word

		CODE ^|(;)|,doret
		ld hl,(rp)	; 16	; [rp]->hl
		ld c,(hl)	;  7	;
		inc hl		;  6	;
		ld b,(hl)	;  7	;
		inc hl		;  6	;
		ld (rp),hl	; 16(58); restore [rp++]->bc with ip of the caller
		NEXT			; continue

; (EXIT)	-- ; R: ip --
;		return to caller from colon definition;
;		runtime of the EXIT compile-only word

		CODE (EXIT),doexit
		jr doret		; same as (;)

; (;CODE)	-- ; R: ip --
;		set LASTXT cfa to ip and return from colon definition;
;		a runtime word compiled by the DOES> compile-only word

		CODE ^|(;CODE)|,doscode
		ld hl,(lastxt+3)	; LASTXT->hl with last defined word xt
		inc hl			;
		ld (hl),c		;
		inc hl			;
		ld (hl),b		; ip->[LASTXT+1] overwrite call address
		jr doret		; (;) return to caller

; (DOES)	addr -- addr ; R: -- ip
;		calls the DOES> definition with pfa addr;
;		a runtime word compiled by the DOES> compile-only word coded as call dodoes

		CODE (DOES),dodoes
		ld hl,(rp)	; 16	; [rp]->hl
		dec hl		;  6	;
		ld (hl),b	;  7	;
		dec hl		;  6	;
		ld (hl),c	;  7	;
		ld (rp),hl	; 16	; save bc->[--rp] with old ip on the return stack
		pop bc		; 10	; pop bc with new ip of the DOES> routine saved by call dodoes
		pop hl		; 10	; pop pfa addr
		push de		; 11	; save TOS
		ex de,hl	;  4(93); set new TOS to hl with pfa addr
		NEXT			; continue

; (VAR)		-- addr
;		leave parameter field address (pfa) of variable;
;		runtime word of a VARIABLE coded as call dovar

		CODE (VAR),dovar
		pop hl		; 10	; pop hl with pfa addr saved by call dovar
		push de		; 11	; save TOS
		ex de,hl	;  4(25); set new TOS to hl with pfa addr
		NEXT			; continue

; (VAL)		-- x
;		fetch value;
;		runtime word of a VALUE coded as call doval

		CODE (VAL),doval
		pop hl		; 10	; pop hl with pfa addr saved by call doval
push_fetch:	push de		; 11	; save TOS
fetch_de:	ld e,(hl)	;  7	;
		inc hl		;  6	;
		ld d,(hl)	;  7(41); set [hl]->de as new TOS
		NEXT			; continue

; (2VAL)	-- dx
;		fetch double value;
;		runtime word of a 2VALUE coded as call dotwoval

		CODE (2VAL),dotwoval
		pop hl		; 10	; pop hl with pfa addr saved by call dotwocon
push_twofetch:	push de		; 11	;
twofetch_de:	inc hl		;  6	;
		inc hl		;  6	;
		ld e,(hl)	;  7	;
		inc hl		;  6	;
		ld d,(hl)	;  7	;
		dec hl		;  6	;
		dec hl		;  6	;
		dec hl		;  6	;
		jr push_fetch	; 31(102; save de as 2OS, set [hl]->de as new TOS and continue

; (CON)		-- x
;		fetch constant;
;		runtime word of a CONSTANT coded as call docon

		CODE (CON),docon
		jr doval		; same as (VAL)

; (2CON)	-- x
;		fetch double constant;
;		runtime word of a 2CONSTANT coded as call dotwocon

		CODE (2CON),dotwocon
		jr dotwoval		; same as (2VAL)

; (DEF)		--
;		execute deferred word;
;		runtime word of a DEFER coded as call dodef

		CODE (DEF),dodef
		pop hl		; 10	; pop hl with pfa addr saved by call dodef
		ld a,(hl)	;  7	;
		inc hl		;  6	;
		ld h,(hl)	;  7	;
		ld l,a		;  4	; [hl]->hl with execution token
		jp (hl)		;  4(38); execute the execution token

; (LIT)		-- x
;		fetch literal;
;		runtime word compiled by EVALUATE, INTERPRET and NUMBER

		CODE (LIT),dolit
		push de			; save TOS
		ld a,(bc)		;
		ld e,a			;
		inc bc			;
		ld a,(bc)		;
		ld d,a			;
		inc bc			; set [ip++]->de as new TOS
		NEXT			; continue

; (2LIT)	-- x1 x2
;		fetch double literal;
;		runtime word compiled by EVALUATE, INTERPRET and NUMBER

		CODE (2LIT),dotwolit
		push de			; save TOS
		ld a,(bc)		;
		ld e,a			;
		inc bc			;
		ld a,(bc)		;
		ld d,a			;
		inc bc			; set [ip++]->de as new TOS
		ld a,(bc)		;
		ld l,a			;
		inc bc			;
		ld a,(bc)		;
		ld h,a			;
		inc bc			; set [ip++]->hl as new 2OS
		push hl			; save hl as 2OS
		JP_NEXT			; continue

; (SLIT)	-- c-addr u
;		fetch literal string;
;		runtime word compiled by S" and ."

		CODE (SLIT),doslit
		push de			; save TOS
		ld a,(bc)		;
		inc bc			; [ip++]->a with string length byte
		push bc			; save bc=c-addr as new 2OS
		ld e,a			;
		ld d,0			; set a->de with u as new TOS
		add c			;
		ld c,a			;
		ld a,d			; 0->a
		adc b			;
		ld b,a			; ip+u->ip
		JP_NEXT			; continue

;-------------------------------------------------------------------------------
;
;		CONSTANTS
;
;-------------------------------------------------------------------------------

; 0		-- 0
;		leave constant 0
;
;    0 CONSTANT 0

		CODE 0,zero
		push de			; save TOS
zero_next:	ld de,0			; set new TOS to 0
		NEXT			; continue

false		.equ zero		; alias
false_next	.equ zero_next		; alias

; 1		-- 1
;		leave constant 1
;
;    1 CONSTANT 1

		CODE 1,one
		push de			; save TOS
one_next:	ld de,1			; set new TOS to 1
		NEXT			; continue

; -1		-- -1
;		leave constant -1
;
;    -1 CONSTANT -1

		CODE -1,mone
		push de			; save TOS
mone_next:	ld de,-1		; set new TOS to -1
		NEXT			; continue

true		.equ mone		; alias
true_next	.equ mone_next		; alias

.if FULL

;+ FALSE	-- 0
;		leave 0
;
;    0 CONSTANT FALSE

		CODE FALSE,false_
		jr zero

;+ TRUE		-- -1
;		leave -1
;
;    -1 CONSTANT TRUE

		CODE TRUE,true_
		jr mone

.endif

; BL		-- 32
;		leave constant 32 (space)
;
;    #32 CONSTANT BL

		CODE BL,bl
		push de			; save TOS
		ld de,0x20		; set new TOS to 0x20
		JP_NEXT			; continue

; PAD		-- c-addr
;		leave address of the PAD;
;		the PAD is a free buffer space of 256 bytes not used by Forth850 

		CODE PAD,pad
		push de			; save TOS
		ld de,PAD0		; set new TOS to PAD0
		JP_NEXT			; continue

; TIB		-- c-addr
;		leave address of TIB;
;		the terminal input buffer used by Forth850

		CODE TIB,tib
		push de			; save TOS
		ld de,TIB0		; set new TOS to TIB0
		JP_NEXT			; continue

; TMP		-- c-addr
;		leave address of the next temp string buffer;
;		switches between two string buffers of 256 free bytes each;
;		used by S" to store a string when interpreting

		CODE TMP,tmp
		push de			; save TOS
		ld hl,1$		; 1$->hl
		ld a,(hl)		; [1$]->a with counter 0 or 1
		xor 1			; a^1->a
		ld (hl),a		; [1$]^1->[1$]
		ld de,TMP0		;
		add d			;
		ld d,a			; set TMP0+(a<<8)->de as new TOS
		JP_NEXT			; continue
1$:		.db 1			; previous tmp buffer 0 or 1

;-------------------------------------------------------------------------------
;
;		STACK OPERATIONS
;
;-------------------------------------------------------------------------------

; DROP		x --
;		drop TOS

		CODE DROP,drop
		pop de		; 10	; pop new TOS
		NEXT			; continue

; DUP		x -- x x
;		duplicate TOS

		CODE DUP,dup
		push de		; 11	; set new TOS
		NEXT			; continue

; ?DUP		x -- x x or 0 -- 0
;		duplicate TOS if nonzero

		CODE ?DUP,qdup
		ld a,e		;  4	;
		or d		;  4	; test de=0
		jr nz,dup	; 23/7	; if de<>0 then DUP
		NEXT			; continue

; SWAP		x1 x2 -- x2 x1
;		swap TOS with 2OS

		CODE SWAP,swap
		pop hl		; 10	; pop hl with 2OS
		push de		; 11	; save de as new 2OS
		ex de,hl	;  4(25); set new TOS to hl
		NEXT			; continue

; OVER		x1 x2 -- x1 x2 x1
;		copy 2OS over TOS

		CODE OVER,over
		pop hl		; 10	; pop hl with 2OS
		push hl		; 11	; keep 2OS
		push de		; 11	; save TOS
		ex de,hl	;  4(36); set new TOS to hl with old 2OS
		NEXT			; continue

; ROT		x1 x2 x3 -- x2 x3 x1
;		rotate cells
;
;    : ROT >R SWAP R> SWAP ;

		CODE ROT,rot
		pop hl		; 10	; pop hl with 2OS
		ex (sp),hl	; 19	; save hl as new 3OS, set hl to old 3OS
		push de		; 11	; save TOS
		ex de,hl	;  4(44); set new TOS to hl with old 3OS
		NEXT			; continue

; -ROT		x1 x2 x3 -- x3 x1 x2
;		undo (or left) rotate cells
;
;    : -ROT ROT ROT ;

		CODE -ROT,mrot
		pop hl		; 10	; pop hl with 2OS
		ex de,hl	;  4	; set de to 2OS, hl to TOS
		ex (sp),hl	; 19	; save hl as new 3OS, set hl to old 3OS
		push hl		; 11(44); save hl as new 2OS
		NEXT			; continue

; NIP		x1 x2 -- x2
;		nip 2OS
;
;    : NIP SWAP DROP ;

		CODE NIP,nip
		pop hl		; 10	; discard 2OS
		NEXT			; continue

; TUCK		x1 x2 -- x2 x1 x2
;		tuck TOS under 2OS
;
;    : TUCK SWAP OVER ;

		CODE TUCK,tuck
		pop hl		; 10	; pop hl with 2OS
		push de		; 11	; save TOS as new 3OS
		push hl		; 11(32); save hl as new 2OS
		NEXT			; continue

; 2DROP		xd1 xd2 -- xd1
;		drop double TOS
;
;    : 2DROP DROP DROP ;

		CODE 2DROP,twodrop
		pop de		; 10	; discard 2OS
		pop de		; 10(20); pop new TOS
		NEXT		; continue

; 2DUP		xd -- xd xd
;		duplicate double TOS
;
;    : 2DUP OVER OVER ;

		CODE 2DUP,twodup
		pop hl		; pop hl with 2OS
		push hl		; keep 2OS as new 4OS
		push de		; save de as new 3OS
		push hl		; save hl as new 2OS
		NEXT		; continue

; 2SWAP		xd1 xd2 -- xd2 xd1
;		swap double TOS with double 2OS
;
;    : 2SWAP ROT >R ROT R> ;
;    : 2SWAP 3 ROLL 3 ROLL ;

		CODE 2SWAP,twoswap
		pop ix			; pop ix with 2OS
		pop hl			; pop hl with 3OS
		ex (sp),ix		; save ix as new 4OS, ix with new 2OS
		push de			; save de as new 3OS
		push ix			; save ix as new 2OS
		ex de,hl		; set new TOS to hl
		NEXT			; continue

; 2OVER		xd1 xd2 -- xd1 xd2 xd1
;		copy double 2OS over double TOS
;
;    : 2OVER >R >R 2DUP R> R> 2SWAP ;
;    : 2OVER 3 PICK 3 PICK ;

		CODE 2OVER,twoover
		push de			; save TOS
		ld hl,6			;
		add hl,sp		; sp+6->hl
		ld e,(hl)		;
		inc hl			;
		ld d,(hl)		;
		push de			; save [sp+6]->de new 2OS
		dec hl			;
		dec hl			;
		ld d,(hl)		;
		dec hl			;
		ld e,(hl)		; set [sp+4]->de as new TOS
		NEXT			; continue

.if FULL

;+ 2ROT		xd1 xd2 xd3 -- xd2 xd3 xd1
;		rotate double cells
;
;    : 2ROT 5 ROLL 5 ROLL ;

		COLON 2ROT,tworot
		.dw dolit,5,roll,dolit,5,roll
		.dw doret

.endif

; DEPTH		-- u
;		parameter stack depth
;
;    : DEPTH sp0 @ SP@ - 2- 2/ ;

		CODE DEPTH,depth
		push de			; save TOS
		ld hl,(sp0)		; [sp0]->hl
		dec hl			;
		scf			; 1->cf
		sbc hl,sp		;
		ex de,hl		; set [sp0]-sp-2->de as TOS
		jp twoslash		; 2/ divide TOS by 2

; CLEAR		... --
;		purge parameter stack
;
;    : CLEAR sp0 @ SP! ;

		CODE CLEAR,clear
		ld sp,(sp0)		; [sp0]->sp
		JP_NEXT			; continue

; .S		--
;		display parameter stack
;
;    : .S DEPTH 0 ?DO sp0 @ I 2+ CELLS - ? LOOP ;

		COLON .S,dots
		.dw depth,zero,doqdo,2$
1$:		.dw   dolit,sp0,fetch,i,twoplus,cells,minus,question
		.dw doloop,1$
2$:		.dw doret

; SP@		-- addr
;		fetch stack pointer

		CODE SP@,spfetch
		push de			; save TOS
		ld hl,0			;
		add hl,sp		; sp->hl
		ex de,hl		; set new TOS to hl with sp
		JP_NEXT			; continue

; SP!		addr --
;		store stack pointer

		CODE SP!,spstore
		ex de,hl		;
		ld sp,hl		; addr->sp
		pop de			;
		JP_NEXT			; continue

.if 0 ; unused, but perhaps useful later

;- N>R		n*x n -- ; R: -- n*x n
;		move n cells to the return stack

		CODE N>R,ntor
		push de			; save TOS
		exx			; save bc with ip
		pop bc			;
		push bc			; TOS->bc
		inc bc			;
		ld l,c			;
		ld h,b			;
		add hl,bc		;
		ld c,l			;
		ld b,h			; 2*(bc+1)->bc
		ld hl,(rp)		; [rp]->hl
		or a			; hl-bc->hl
		sbc hl,bc		;
		ld (rp),hl		; rp-bc->rp
		ex de,hl		; rp-bc->de
		ld hl,0			;
		add hl,sp		; sp->hl
		ldir			; [hl++]->[de++] until --bc=0
		ld sp,hl		; hl->sp
		exx			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue

;- NR>		R: n*x n -- ; -- n*x n
;		move n cells from the return stack

		CODE NR>,nrfrom
		push de			; save TOS
		exx			; save bc with ip
		ld hl,(rp)		; [rp]->hl
		ld c,(hl)		;
		inc hl			;
		ld b,(hl)		;
		dec hl			; [rp]->bc
		ex de,hl		; [rp]->de
		inc bc			;
		ld l,c			;
		ld h,b			;
		add hl,bc		;
		ld c,l			;
		ld b,h			; 2*(bc+1)->bc
		ld hl,0			;
		add hl,sp		;
		sbc hl,bc		;
		ld sp,hl		; sp-bc->sp,de
		ex de,hl		; [rp]->hl
		ldir			; [hl++]->[de++] until --bc=0
		ld (rp),hl		; hl->rp
		exx			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue

.endif

; >R		x -- ; R: -- x
;		move TOS to the return stack

		CODE >R,tor
		ld hl,(rp)	; 16	; [rp]->hl
		dec hl		;  6	;
		ld (hl),d	;  7	;
		dec hl		;  6	;
		ld (hl),e	;  7	; de->[--rp]
		ld (rp),hl	; 16	;
		pop de		; 10(58); pop new TOS
		NEXT			; continue

; DUP>R		x -- x ; R: -- x
;		duplicate TOS to the return stack, a single word for DUP >R

		CODE DUP>R,duptor
		push de		; 11	; save TOS
		jr tor		; 60(71); >R

; R>		R: x -- ; -- x
;		move cell from the return stack

		CODE R>,rfrom
		push de		; 11	; save TOS
		ld hl,(rp)	; 16	; [rp]->hl
		ld e,(hl)	;  7	;
		inc hl		;  6	;
		ld d,(hl)	;  7	;
		inc hl		;  6	; set [rp++]->de as new TOS
		ld (rp),hl	; 16(59);
		NEXT			; continue

; RDROP		R: x -- ; --
;		drop cell from the return stack, a single word for R> DROP

		CODE RDROP,rdrop
		ld hl,(rp)		; [rp]->hl
		inc hl			;
		inc hl			;
		ld (rp),hl		; rp+2->rp
		NEXT			; continue

; R@		R: x -- x ; -- x
;		fetch cell from the return stack

		CODE R@,rfetch
		ld hl,(rp)		; [rp]->hl
		jp push_fetch		; set [hl]->de as new TOS and continue

; 2>R		x1 x2 -- ; R: -- x1 x2
;		move double TOS to the return stack, a single word for SWAP >R >R

		CODE 2>R,twotor
		pop hl			; pop hl with 2OS
		push de			; save TOS
		ex de,hl		; 2OS->de
		ld hl,(rp)		; [rp]->hl
		dec hl			;
		ld (hl),d		;
		dec hl			;
		ld (hl),e		; save de->[--rp] with 2OS
		ld (rp),hl		;
		pop de			; restore TOS
		jr tor			; >R

; 2R>		R: x1 x2 -- ; -- x1 x2
;		move double cell from the return stack, a single word for R> R> SWAP

		CODE 2R>,tworfrom
		push de			; save TOS
		ld hl,(rp)		; [rp]->hl
		ld e,(hl)		;
		inc hl			;
		ld d,(hl)		;
		inc hl			;
		push de			; save [rp++]->de as new 2OS
		ld e,(hl)		;
		inc hl			;
		ld d,(hl)		;
		inc hl			; [rp++]->de as TOS
		ld (rp),hl		;
		jp swap			; SWAP

; 2R@		R: x1 x2 -- x1 x2 ; -- x1 x2
;		fetch double cell from the return stack

		CODE 2R@,tworfetch
		push de			; save TOS
		ld ix,(rp)		; [rp]->ix
		ld e,(ix+2)		;
		ld d,(ix+3)		;
		push de			; save [ix+2]->de as new 2OS
		ld e,(ix+0)		;
		ld d,(ix+1)		; set [ix]->de as TOS
		JP_NEXT			; continue

; RP@		-- addr
;		fetch return stack pointer

		CONSTANT RP@,rpfetch
rp:		.dw 0

; RP!		addr --
;		store return stack pointer

		CODE RP!,rpstore
		ld hl,rp		; [rp]->hl
		jr store_hl		; set de->[hl] pop new TOS and continue

; PICK		xu ... x0 u -- xu ... x0 xu
;		pick u'th cell from the parameter stack
;
;    : PICK 1+ CELLS SP@ + @ ;

		CODE PICK,pick
		ld l,e			;
		ld h,d			;
		add hl,de		;
		adc hl,sp		; sp+2*n->hl
		jp fetch_de		; set [hl]->de as new TOS and continue

.if FULL

;+ ROLL		xu x(u+1) ... x1 x0 u -- x(u+1) ... x1 x0 xu
;		roll u cells on the parameter stack

		CODE ROLL,roll
		pop hl			; pop hl with 2OS
		di			; disable interrupts to protect exposed stack
		di			; "
		ld (3$),sp		; save sp->3$
		inc e			; e++
		jr 2$			;
1$:		ex (sp),hl		; loop, exchange [sp++],hl
		pop af			;
2$:		dec e			;
		jr nz,1$		; until --e=0
		ld sp,(3$)		; restore 3$->sp
		ei			; enable interrupts
		ex de,hl		; set new TOS to hl
		JP_NEXT			; continue
3$:		.dw 0			; stack pointer temporary

.endif

;-------------------------------------------------------------------------------
;
;		FETCH AND STORE
;
;-------------------------------------------------------------------------------

; @		addr -- x
;		fetch from cell

		CODE @,fetch
		ex de,hl	;  4	; addr->hl
		ld e,(hl)	;  7	;
		inc hl		;  6	;
		ld d,(hl)	;  7(24); set [hl]->de as new TOS
		NEXT			; continue

; C@		c-addr -- char
;		fetch char

		CODE C@,cfetch
		ld a,(de)		; [de]->a
a_next:		ld e,a			;
		ld d,0			; a->de
		NEXT			; continue

; 2@		addr -- x1 x2
;		fetch from double cell
;
;    : 2@ DUP CELL+ @ SWAP @ ;

		CODE 2@,twofetch
		ex de,hl		;
		jp twofetch_de		; [hl+2]->2OS [hl]->TOS and continue

; !		x addr --
;		store in cell

		CODE !,store
		pop hl		; 10	; pop addr->hl
		ex de,hl	;  4	; x->de, addr->hl
store_hl:	ld (hl),e	;  7	;
		inc hl		;  6	;
		ld (hl),d	;  7	; de->[hl] with x
		pop de		; 10(44); pop new TOS
		NEXT			; continue

; (TO)		x --
;		store in value;
;		runtime of the TO compile-only word

		CODE (TO),doto
		ld a,(bc)		;
		ld l,a			;
		inc bc			;
		ld a,(bc)		;
		ld h,a			;
		inc bc			; [ip++]->hl
		jr store_hl		; de->[hl] with x pop new TOS and continue

; C!		char c-addr --
;		store char

		CODE C!,cstore
		pop hl			; pop addr->hl
		ex de,hl		; char->de, c-addr->hl
		ld (hl),e		; set e->[hl] with char
		pop de			; pop new TOS
		NEXT			; continue

; 2!		x1 x2 addr --
;		store in double cell
;
;    : 2! TUCK ! CELL+ ! ;

		CODE 2!,twostore
		pop hl			; pop x2->hl
		ex de,hl		; x2->de, addr->hl
twostore_hl:	ld (hl),e		;
		inc hl			;
		ld (hl),d		;
		inc hl			; de->[hl++] with x2
		pop de			; pop de with x1
		jr store_hl		; de->[hl] with x1 pop new TOS and continue

; (2TO)		dx --
;		store in double value;
;		runtime of the TO compile-only word

		CODE (2TO),dotwoto
		ld a,(bc)		;
		ld l,a			;
		inc bc			;
		ld a,(bc)		;
		ld h,a			;
		inc bc			; [ip++]->hl
		jr twostore_hl		; de->[hl], 2OS->[hl+2] pop new TOS and continue

; +!		n addr --
;		increment cell

		CODE +!,plusstore
		pop hl			; pop addr->hl
		ex de,hl		; x->de, addr->hl
plusstore_hl:	ld a,(hl)		;
		add e			;
		ld (hl),a		;
		inc hl			;
		ld a,(hl)		;
		adc d			;
		ld (hl),a		; [hl]+de->[hl]
		pop de			; pop new TOS
		NEXT			; continue

; (+TO)		n --
;		increment value;
;		runtime of the +TO compile-only word

		CODE (+TO),doplusto
		ld a,(bc)		;
		ld l,a			;
		inc bc			;
		ld a,(bc)		;
		ld h,a			;
		inc bc			; [ip++]->hl
		jr plusstore_hl		; set [hl]+de->[hl] pop new TOS and continue

; ON		addr --
;		store TRUE (-1) in cell
;
;    : ON -1 SWAP ! ;

		CODE ON,on
		ld a,-1			; -1->a
store_a_de:	ex de,hl		; addr->hl
store_a_hl:	ld (hl),a		;
		inc hl			;
		ld (hl),a		; a->[hl]
		pop de			; pop new TOS
		NEXT			; continue

; OFF		addr --
;		store FALSE (0) in cell
;
;    : OFF 0 SWAP ! ;

		CODE OFF,off
		xor a			; 0->a
		jr store_a_de		; 0->[addr] pop new TOS and continue

;-------------------------------------------------------------------------------
;
;		ARITHMETIC AND LOGIC OPERATIONS
;
;-------------------------------------------------------------------------------

; +		n1 n2 -- n3
;		sum n1+n2

		CODE +,plus
		pop hl			; pop n1->hl
		add hl,de		; x1+x2->hl
		ex de,hl		; set new TOS to hl
		NEXT			; continue

; M+		d1 n -- d2
;		double sum d1+n

		CODE M+,mplus
		pop hl			; pop hl with high order of d1
		ex (sp),hl		; exchange hl low/high order of d1
		add hl,de		; hl+n->de
		pop de			; pop de with high order d2
		push hl			; save hl with low order d2
		jr nc,1$		; if cf=1 then
		inc de			;   de++ increment high order of d2
1$:		NEXT			; continue

; D+		d1 d2 -- d3
;		double sum d1+d2
;
;    : D+ >R M+ R> + ;

		COLON D+,dplus
		.dw tor,mplus,rfrom,plus
		.dw doret

; -		n1 n2 -- n3
;		difference n1-n2

		CODE -,minus
		pop hl			; pop n1->hl
		or a			; 0->cf
		sbc hl,de		; set n1-n2->de as new TOS
		ex de,hl		; set new TOS to hl
		NEXT			; continue

; D-		d1 d2 -- d3
;		double difference d1-d2
;
;    : D- DNEGATE D+ ;

		CODE D-,dminus
		.dw dnegate,dplus
		.dw doret

; UM*		u1 u2 -- ud
;		unsigned double product u1*u2

		CODE UM*,umstar
		push de			; save TOS
		exx			; save bc with ip
		pop bc			; pop u2->bc
		pop de			; pop u1->de
		xor a			; 0->cf
		ld l,a			;
		ld h,a			; 0->hl
		ld a,17			; 17->a loop counter
1$:		rr h		;  8	; loop
		rr l		;  8	;
		rr d		;  8	;
		rr e		;  8	;   de,hl+cf>>1->de,hl
		jr nc,2$	;  7	;   if cf=1 then
		add hl,bc	; 11	;     hl+bc->hl
2$:		dec a		;  4	;
		jp nz,1$	; 10(64); until --b=0
		push de			; save de with low order ud
		push hl			; save hl with high order ud
		exx			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue

; M*		n1 n2 -- d
;		signed double product n1*n2
;
;    : M*
;      2DUP XOR >R
;      ABS SWAP ABS UM*
;      R> 0< IF DNEGATE THEN ;

		COLON M*,mstar
		.dw twodup,xor,tor
		.dw abs,swap,abs,umstar
		.dw rfrom,zeroless,doif,1$
		.dw   dnegate
1$:		.dw doret

; *		n1|u1 n2|u2 -- n3|u3
;		signed and unsigned product n1*n2
;
;    : * UM* DROP ;

.if FAST

;		new and faster Z80 method for signed 6x16->16 bit multiplication:
;		51 cy/bit compared to Zilog's published 62 cy/bit
;		faster than other Z80 methods published in online resources,
;		unless someone can point to a better method...

		CODE *,star
		push de			; save TOS
		exx			; save bc with ip
		pop de			; n2->de
		pop bc			; n1->bc
		ld hl,0			; 0->hl
		ld a,c			; c->a low order byte of n1
		ld c,b			; b->c save high order byte of n1
		ld b,8			; 8->b loop counter
1$:		rra		;  4	; loop, a>>1->a set cf
		jr nc,2$	;  7	;   if cf=1 then
		add hl,de	; 11	;     hl+de->hl
2$:		sla e		;  8	;
		rl d		;  8	;   de<<1->de
		djnz 1$		; 13(51); until --b=0
		ld a,c			; c->a high order byte of n1
		ld b,8			; 8->b loop counter
3$:		rra		;  4	; loop, a>>1->a set cf
		jr nc,4$	;  7	;   if cf=1 then
		add hl,de	; 11	;     hl+de->hl
4$:		sla e		;  8	;
		rl d		;  8	;   de<<1->de
		djnz 3$		; 13(51); until --b=0
		push de			; save de with product as TOS
		exx			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue

.else

		COLON *,star
		.dw umstar,drop
		.dw doret

.endif

; UMD*		ud1 u -- ud2
;		unsigned double product ud1*u
;
;    : UMD* DUP>R UM* DROP SWAP R> UM* ROT + ;

		COLON UMD*,umdstar
		.dw duptor
		.dw umstar,drop,swap
		.dw rfrom,umstar,rot,plus
		.dw doret

; MD*		d1 n -- d2
;		signed double product d1*n
;
;    : MD*
;      2DUP XOR >R
;      ABS -ROT DABS ROT
;      UMD*
;      R> 0< IF DNEGATE THEN ;

		COLON MD*,mdstar
		.dw twodup,xor,tor
		.dw abs,mrot,dabs,rot
		.dw umdstar
		.dw rfrom,zeroless,doif,1$
		.dw   dnegate
1$:		.dw doret

; UM/MOD	ud u1 -- u2 u3
;		remainder and quotient ud/u1;
;		the result is undefined when u1=0

		CODE UM/MOD,umslashmod
		push de			; save TOS
		exx			; save bc with ip
		pop bc			; pop u1->bc divisor
		pop hl			; pop ud->hl high order dividend
		pop de			; pop ud->de low order dividend
		ld a,16			; 16->a loop counter
		sla e			;
		rl d			; de<<1->de
1$:		adc hl,hl	; 15	; loop, hl<<1+cf->hl
		jr nc,2$	; 12/ 7	;   if cf=1 then
		or a		;     4	;     0->cf
		sbc hl,bc	;    15	;     hl-bc->hl
		or a		;     4	;     0->cf
		jp 3$		;    10	;   else
2$:		sbc hl,bc	; 15	;     hl-bc->hl
		jr nc,3$	; 12/ 7 ;     if cf=1 then
		add hl,bc	;    11	;       hl+bc->hl to undo sbc, sets cf
3$:		rl e		;  8	;
		rl d		;  8	;   de<<1+cf->de with inverse cf we'll need
		dec a		;  4	;
		jp nz,1$	; 10(88); until --a=0
		push hl			; save hl with u2 remainder
		push de			; save de with u3 complemented quotient
		exx			; restore bc with ip
		pop de			; pop new TOS with complemented quotient
		jp invert		; complement TOS, faster than ccf in loop

; SM/REM	d1 n1 -- n2 n3
;		symmetric signed remainder and quotient d1/n1;
;		the result is undefined when n1=0
;
;    : SM/REM
;      2DUP XOR >R
;      OVER >R
;      ABS >R
;      DABS R> UM/MOD
;      SWAP
;      R> 0< IF NEGATE THEN
;      SWAP
;      R> 0< IF NEGATE THEN ;

		COLON SM/REM,smslashrem
		.dw twodup,xor,tor
		.dw over,tor
		.dw abs,tor
		.dw dabs,rfrom,umslashmod
		.dw swap
		.dw rfrom,zeroless,doif,1$
		.dw   negate
1$:		.dw swap
		.dw rfrom,zeroless,doif,2$
		.dw   negate
2$:		.dw doret

; FM/MOD	d1 n1 -- n2 n3
;		floored signed modulus and quotient d1/n1;
;		the result is undefined when n1=0
;
;    : FM/MOD
;      DUP>R
;      SM/REM
;      DUP 0< IF
;        SWAP R> + SWAP 1-
;      ELSE
;        RDROP
;      THEN ;

		COLON FM/MOD,fmslashmod
		.dw duptor
		.dw smslashrem
		.dw dup,zeroless,doif,1$
		.dw   swap,rfrom,plus,swap,oneminus
		.dw doahead,2$
1$:		.dw   rdrop
2$:		.dw doret

; /MOD		n1 n2 -- n3 n4
;		signed symmetric remainder and quotient n1/n2;
;		the result is undefined when n2=0
;
;    : /MOD SWAP S>D ROT SM/REM ;

		COLON /MOD,slashmod
		.dw swap,stod,rot
		.dw smslashrem
		.dw doret	

; MOD		n1 n2 -- n3
;		signed symmetric remainder of n1/n2;
;		the result is undefined when n2=0
;
;    : / /MOD DROP ;

		COLON MOD,mod
		.dw slashmod,drop
		.dw doret

; /		n1 n2 -- n3
;		signed symmetric quotient n1/n2;
;		the result is undefined when n2=0
;
;    : / /MOD NIP ;

		COLON /,slash
		.dw slashmod,nip
		.dw doret

; */MOD		n1 n2 n3 -- n4 n5
;		signed product symmetric remainder and quotient n1*n2/n3;
;		the result is undefined when n3=0
;
;    : */MOD -ROT M* ROT SM/REM ;

		COLON */MOD,starslashmod
		.dw mrot,mstar,rot
		.dw smslashrem
		.dw doret

; */		n1 n2 n3 -- n4
;		signed product symmetric quotient n1*n2/n3;
;		the result is undefined when n3=0
;
;    : */ */MOD NIP ;

		COLON */,starslash
		.dw starslashmod,nip
		.dw doret

; M*/		d1 n1 n2 -- d2
;		signed double product symmetric quotient d1*n1/n2;
;		the result is undefined when n2=0
;
;    : M*/ >R MD* R> SM/REM NIP ;

		COLON M*/,mstarslash
		.dw tor
		.dw mdstar
		.dw rfrom
		.dw smslashrem,nip
		.dw doret

.if FULL

;+ D*		d1|ud1 d2|ud2 -- d3|ud3
;		signed and unsigned double product d1*d2
;
;    : D* >R ROT DUP>R -ROT MD* 2R> * 0 SWAP D+ ;

.if 0

;		new fast Z80 method for signed 32x32->32 bit multiplication
;		disabled since this is only a bit faster than coded in Forth below

		CODE D*,dstar
		ld hl,0			; 0->hl high order d3, de with d2 high order
		exx			; save bc with ip
		pop de			; d2->de' low order d2
		pop hl			; d1->hl' high order d1
		pop bc			; d1->bc' low order d1
		ld a,h			;
		push af			; save d1 high order byte 3
		ld a,l			;
		push af			; save d1 high order byte 2
		ld a,b			;
		push af			; save d1 low order byte 1
		ld a,c			;
		push af			; save d1 low order byte 0
		ld hl,0			; 0->hl' low order d3
		ld c,4			; 4->c outer loop counter
1$:		pop af			; loop, [sp++]->a next d1 byte
		ld b,8			;   8->b inner loop counter
2$:		rra		;  4	;   loop, a>>1->a set cf
		jr nc,3$	;  7	;     if cf=1 then
		add hl,de	; 11	;       hl'+de'->hl add low order
		exx		;  4	;
		adc hl,de	; 15	;       hl+de+cf->hl add high order
		exx		;  4	;
3$:		sla e		;  8	;
		rl d		;  8	;     de'<<1->de' shift low order
		exx		;  4	;
		rl e		;  8	;
		rl d		;  8	;     de<<1+cf->de shift high order
		exx		;  4	;
		djnz 2$		; 13(98);   until --b=0
		dec c			;
		jr nz,1$		; until --c=0
		push hl			; save hl' with low order d3
		exx			; restore bc with ip
		ex de,hl		; set new TOS to hl with high order d3
		JP_NEXT			; continue

.else

		COLON D*,dstar
		.dw tor
		.dw rot,duptor,mrot
		.dw mdstar
		.dw tworfrom,star
		.dw zero,swap,dplus
		.dw doret

.endif

.endif

; AND		x1 x2 -- x1&x2
;		bitwise and x1 with x2

		CODE AND,and
		pop hl			; pop x1->hl
		ld a,e			;
		and l			;
		ld e,a			;
		ld a,d			;
		and h			;
		ld d,a			; set hl&de->de as new TOS
		NEXT			; continue

; OR		x1 x2 -- x1|x2
;		bitwise or x1 with x2

		CODE OR,or
		pop hl			; pop x1->hl
		ld a,e			;
		or l			;
		ld e,a			;
		ld a,d			;
		or h			;
		ld d,a			; set hl|de->de as new TOS
		NEXT			; continue

; XOR		x1 x2 -- x1^x2
;		bitwise xor x1 with x2

		CODE XOR,xor
		pop hl			; pop x1->hl
		ld a,e			;
		xor l			;
		ld e,a			;
		ld a,d			;
		xor h			;
		ld d,a			; set hl^de->de as new TOS
		NEXT			; continue

; =		x1 x2 -- flag
;		true if x1=x2

		CODE ^|=|,equal
		pop hl			; pop x1->hl
		xor a			; 0->a, 0->Cf
		sbc hl,de		; test if hl=de
true_if_z_next:	ld d,a			; require a=0
		ld e,a			; set new TOS to FALSE
		jr nz,1$		; set new TOS to TRUE if x1=x2 else FALSE
		dec de			;
1$:		NEXT			; continue

; <>		x1 x2 -- flag
;		true if x1<>x2

		CODE <>,notequal
		pop hl			; pop x1->hl
		xor a			; 0->a, 0->cf
		sbc hl,de		; test if hl=de
true_if_nz_next:ld d,a			; require a=0
		ld e,a			; set new TOS to FALSE
		jr z,1$			; set new TOS to TRUE if x1<>x2 else FALSE
		dec de			;
1$:		NEXT			; continue

; <		n1 n2 -- flag
;		true if n1<n2 signed
;
;    : <
;      2DUP XOR 0< IF
;        DROP 0<
;        EXIT
;      THEN
;      - 0< ;

		CODE <,less
		pop hl			; pop n1->hl
less_hl_de:	xor a			; 0->a, 0->cf
		sbc hl,de		; test hl<de
		jp pe,true_if_p_next	; if not OV then
true_if_m_next:	ld d,a			;   require a=0
		ld e,a			;   set new TOS to FALSE
		jp p,1$			;   if not positive then
		dec de			;     set new TOS to TRUE
1$:		NEXT			; continue
true_if_p_next:	ld d,a			; require a=0
		ld e,a			; set new TOS to FALSE
		jp m,1$			; if not negative then
		dec de			;   set new TOS to TRUE
1$:		NEXT			; continue

; >		n1 n2 -- flag
;		true if n1>n2 signed
;
;    : > SWAP < ;

		CODE >,greater
		pop hl			; pop n1->hl
		ex de,hl		; n1->de, n2->hl
		jr less_hl_de		; set new TOS to TRUE if n1>n2

; U<		u1 u2 -- flag
;		true if u1<u2 unsigned
;
;    : U<
;      2DUP XOR 0< IF
;        NIP 0<
;        EXIT
;      THEN
;      - 0< ;

		CODE U<,uless
		pop hl			; pop n1->hl
uless_hl_de:	or a			; 0->cf
		sbc hl,de		; subtract 2OS from TOS
true_if_c_next:	sbc a			; -cf->a
		ld d,a			;
		ld e,a			; set new TOS to TRUE if cf=1 else FALSE
		NEXT			; continue

; U>		u1 u2 -- flag
;		true if u1>u2 unsigned
;
;    : U> SWAP U< ;

		CODE U>,ugreater
		pop hl			; pop u1->hl
		ex de,hl		; u1->de. u2->hl
		jr uless_hl_de		; set new TOS to TRUE if u1>u2 else FALSE

; 0=		x -- flag
;		true if x=0

		CODE 0=,zeroequal
		ld a,e			;
		or d			;
		sub 1			; cf=1 if x=0
		jr true_if_c_next	; set new TOS to TRUE if x=0 else FALSE

; 0<		n -- flag
;		true if n<0

		CODE 0<,zeroless
		sla d			; cf=1 if n<0
		jr true_if_c_next	; set new TOS to TRUE if cf=1 else FALSE

; D0=		dx -- flag
;		true if dx=0
;
;    : D0= OR 0= ;

		CODE D0=,dzeroequal
		ld a,e			;
		or d			;
		pop de			;
		or e			;
		or d			;
		sub 1			; cf=1 if dx=0
		jr true_if_c_next	; set new TOS to TRUE if cf=1 else FALSE

; D0<		d -- flag
;		true if d<0
;
;    : D0< NIP 0< ;

		CODE D0<,dzeroless
		sla d			; cf=1 if d is negative
		pop de			; pop to discard low order of d
		jr true_if_c_next	; set new TOS to TRUE if cf=1 else FALSE

; S>D		n -- d
;		widen n to a double

		CODE S>D,stod
		push de			; save TOS
		sla d			; test if n<0
		jr true_if_c_next	; set new TOS to -1 if cf=1 else 0

; D>S		d -- n
;		narrow d to a single;
;		may throw -11 "result out of range" valid range is -32768 to 65535

		CODE D>S,dtos
		ld a,e			;
		or d			; test TOS=0 high order of d
		jr nz,3$		; if TOS=0 then
		pop de			;   pop to discard TOS high order of d
2$:		JP_NEXT			;   continue
3$:		pop hl			; pop 2OS->hl low order of d
		bit 7,h			;
		jr z,4$			; if 2OS is negative then
		ld a,d			;
		add e			;
		rra			;
		inc a			;   test TOS=0xffff
		ex de,hl		;   set new TOS to hl
		jr z,2$			;   if TOS=0xffff then continue
4$:		ld a,-11		;
		jp throw_a		; throw -11 "result out of range"

.if FULL

;+ D=		d1 d2 -- flag
;		true if d1=d2
;
;    : D= D- D0= ;

		COLON D=,dequal
		.dw dminus,dzeroequal
		.dw doret

;+ D<		d1 d2 -- flag
;		true if d1<d2
;
;    : <
;      DUP 3 PICK XOR 0< IF
;        2DROP D0<
;        EXIT
;      THEN
;      D- D0< ;

		COLON D<,dless
		.dw dup,dolit,3,pick,xor,zeroless,doif,1$
		.dw   twodrop,dzeroless
		.dw   doexit
1$:		.dw dminus,dzeroless
		.dw doret

;+ DU<		du1 du2 -- flag
;		true if ud1<ud2
;
;    : DU<
;      DUP 3 PICK XOR 0< IF
;        2SWAP 2DROP D0<
;        EXIT
;      THEN
;      D- D0< ;

		CODE DU<,duless
		pop hl			; pop hl with low order d2
		ex (sp),hl		; save low order d2, hl with high order d1
		or a			; 0->cf
		sbc hl,de		; compare high order d1 with high order d2
		pop de			; pop de with low order d2
		pop hl			; pop hl with low order d1
		jp c,true_next		; if cf=1 then set TOS to TRUE
		sbc hl,de		; compare low order d1 with low order d2
		jp true_if_c_next	; set TOS to TRUE if cf=1

.endif

; MAX		n1 n2 -- n3
;		signed max of n1 and n2
;
;    : MAX
;      2DUP < IF SWAP THEN
;      DROP ;

		COLON MAX,max
		.dw twodup,less,doif,1$
		.dw   swap
1$:		.dw drop
		.dw doret

; MIN		n1 n2 -- n3
;		signed min of n1 and n2
;
;    : MIN
;      2DUP > IF SWAP THEN
;      DROP ;

		COLON MIN,min
		.dw twodup,greater,doif,1$
		.dw   swap
1$:		.dw drop
		.dw doret

; UMAX		u1 u2 -- u3
;		unsigned max of u1 and u2
;
;    : UMAX
;      2DUP U< IF SWAP THEN
;      DROP ;

		COLON UMAX,umax
		.dw twodup,uless,doif,1$
		.dw   swap
1$:		.dw drop
		.dw doret

; UMIN		u1 u2 -- u3
;		unsigned min of u1 and u2
;
;    : UMIN
;      2DUP U> IF SWAP THEN
;      DROP ;

		COLON UMIN,umin
		.dw twodup,ugreater,doif,1$
		.dw   swap
1$:		.dw drop
		.dw doret

.if FULL

;+ DMAX		d1 d2 -- d3
;		signed double max of d1 and d2
;
;    : DMAX
;      2OVER 2OVER D< IF 2SWAP THEN
;      2DROP ;

		COLON DMAX,dmax
		.dw twoover,twoover,dless,doif,1$
		.dw   twoswap
1$:		.dw twodrop
		.dw doret

;+ DMIN		d1 d2 -- d3
;		signed double min of d1 and d2
;
;    : DMIN
;      2OVER 2OVER D< INVERT IF 2SWAP THEN
;      2DROP ;

		COLON DMIN,dmin
		.dw twoover,twoover,dless,invert,doif,1$
		.dw   twoswap
1$:		.dw twodrop
		.dw doret

.endif

; WITHIN	x1 x2 x3 -- flag
;		true if x1 is within x2 up to x3 exclusive
;
;    : WITHIN OVER - >R - R> U< ;

		COLON WITHIN,within
		.dw over,minus,tor,minus,rfrom,uless
		.dw doret

; INVERT	x1 -- x2
;		one's complement ~x
;
;    : INVERT 1+ NEGATE ;

.if FAST

		CODE INVERT,invert
		ld a,e			;
		cpl			;
		ld e,a			; ~e->e
		ld a,d			;
		cpl			;
		ld d,a			; ~d->d
		NEXT			; continue

.else

		CODE INVERT,invert
		inc de			;
		jr negate		; set new TOS to -x1-1 and continue

.endif
	

; NEGATE	n1 -- n2
;		two's complement -n

		CODE NEGATE,negate
		xor a			; 0->a
		sub e			;
		ld e,a			; -e->e
		sbc a			;
		sub d			;
		ld d,a			; -cf-d->d, set new TOS
		NEXT			; continue

; ABS		n1 -- n2
;		absolute value |n1|
;
;    : ABS DUP 0< IF NEGATE THEN ;

		CODE ABS,abs
		bit 7,d			; test if TOS is negative
		jr nz,negate		; NEGATE if TOS is negative
		NEXT			; continue

; DNEGATE	d1 -- d2
;		two's complement -d1
;
;    : DNEGATE SWAP INVERT SWAP INVERT 1 M+ ;

		CODE DNEGATE,dnegate
		pop hl			; pop hl with low order d1
		push de			; save de with high order d1
		ex de,hl		; hl->de
		xor a			; 0->cf
		ld l,a			;
		ld h,a			; 0->hl
		sbc hl,de		; -de->hl low order d2
		pop de			; pop de with high order d1
		push hl			; save hl with low order d2
		ld l,a			;
		ld h,a			; 0->hl
		sbc hl,de		; -de->hl high order d2
		ex de,hl		; set new TOS to hl
		JP_NEXT			; continue

; DABS		d1 -- d2
;		absolute value |d1|
;
;    : DABS DUP 0< IF DNEGATE THEN ;

		CODE DABS,dabs
		bit 7,d			; test if TOS is negative
		jr nz,dnegate		; DNEGATE if TOS is negative
		JP_NEXT			; continue

; LSHIFT	x1 u -- x2
;		logical shift left x1<<u

		CODE LSHIFT,lshift
		pop hl			; pop x1->hl
		jr 2$			; while --e is nonnegative
1$:		add hl,hl		;   hl<<1->hl
2$:		dec e			;
		jp p,1$			; repeat
		ex de,hl		; set new TOS to hl
		JP_NEXT			; continue

; RSHIFT	x1 u -- x2
;		logical shift right x1>>u

		CODE RSHIFT,rshift
		pop hl			; pop x1->hl
		jr 2$			; while --e is nonnegative
1$:		srl h			;
		rr l			;   hl>>1->hl
2$:		dec e			;
		jp p,1$			; repeat
		ex de,hl		; set new TOS to hl
		JP_NEXT			; continue

; 1+		n1 -- n2
;		increment n1+1
;
;    : 1+ 1 + ;

		CODE 1+,oneplus
		inc de			; set de++ as TOS
		NEXT			; continue

; 2+		n1 -- n2
;		increment n1+2
;
;    : 2+ 2 + ;

		CODE 2+,twoplus
		inc de
		jr oneplus		; set de+2 as TOS

; 1-		n1 -- n2
;		decrement n1-1
;
;    : 1- 1 - ;

		CODE 1-,oneminus
		dec de			; set de-- as TOS
		NEXT			; continue

; 2-		n1 -- n2
;		decrement n1-2
;
;    : 2- 2 - ;

		CODE 2-,twominus
		dec de
		jr oneminus		; set de-2->de as TOS

; 2*		n1 -- n2
;		arithmetic shift left n1<<1
;
;    : 2* 2 * ;

		CODE 2*,twostar
		sla e
		rl d			; set 2*de->de as TOS
		NEXT			; continue

; 2/		n1 -- n2
;		arithmetic shift right n1>>1
;
;    : 2/ 2 / ;

		CODE 2/,twoslash
		sra d
		rr e			; set de/2->de as TOS
		NEXT			; continue

.if FULL

;+ CELL+	addr -- addr
;		increment to next cell
;
;    : CELL+ 2+ ;

		CODE CELL+,cellplus
		jr twoplus		; same as 2+

;+ CELLS	n1 -- n2
;		convert to cell unit
;
;    : CELLS 2* ;

		CODE CELLS,cells
		jr twostar		; same as 2*

;+ CHAR+	n1 -- n1
;		increment to next char
;
;    : CHAR+ 1+ ;

		CODE CHAR+,charplus
		jr oneplus		; same as 1+

;+ CHARS	n1 -- n2
;		convert to char unit
;
;    : CHARS ;

		CODE CHARS,chars
		JP_NEXT			; do nothing

.else

cellplus	.equ twoplus		; alias
cells		.equ twostar		; alias

.endif

;-------------------------------------------------------------------------------
;
;		STRING AND MEMORY OPERATIONS
;
;-------------------------------------------------------------------------------

; COUNT		c-addr1 -- c-addr2 u
;		convert counted string to string
;
;    : COUNT DUP 1+ SWAP C@ ;

		COLON COUNT,count
		.dw dup
		.dw oneplus,swap,cfetch
		.dw doret

; COMPARE	c-addr1 u1 c-addr2 u2 -- -1|0|1
;		compare strings, leaves -1=less or 0=equal or 1=greater

		CODE COMPARE,compare
		push de			; save TOS
		exx			; save bc with ip
		pop bc			; pop u2->bc
		pop de			; pop c-addr2->de
		pop hl			; u1->hl
		push hl			; keep u1 on the stack
		xor a			; 0->a flags u1=u2, 0->cf
		sbc hl,bc		;
		jr z,1$			; if u1<>u2 then
		inc a			;   1->a flags u1>u2
		jr nc,1$		;   if u1<u2 then
		pop bc			;     pop u1->bc
		push bc			;     rebalance stack
		ld a,-1			;   -1->a flags u1<u2
1$:		pop hl			; pop to discard u1
		pop hl			; pop c-addr1->hl
		ex af,af'		; save a with -1|0|1 flag
		ld a,c			;
		or b			;
		jr z,3$			; if bc<>0 then
;		compare chars
2$:		ld a,(de)	;  7	;   loop
		cpi		; 16	;     compare [hl++] to [de], --bc
		jr nz,5$	;  7	;     while characters [hl] and [de] are equal
		inc de		;  6	;     de++
		jp pe,2$	; 10(46);   until bc=0
;		chars match, check lengths
3$:		ex af,af'		; restore a with -1|0|1 flag
4$:		exx			; restore bc with ip
		ld e,a			;
		add a			;
		sbc a			; a=-1 if e<0 else 0
		ld d,a			; a->de set sign extended TOS
		JP_NEXT			; continue
;		strings differ
5$:		dec hl			; hl-- to correct cpi overshoot
		cp (hl)			; test a<[hl]
		ccf			; complement cf, cf=1 if [hl]<a
		sbc a			; a=-1 if cf=1 else 0
		add a			; a=-2 if cf=1 else 0
		inc a			; a=-1 if cf=1 else 1
		jr 4$			;

; S=		c-addr1 u1 c-addr2 u2 -- flag
;		true if strings match
;
;    : S= COMPARE 0= ;

		COLON S=,sequal
		.dw compare,zeroequal
		.dw doret

; SEARCH	c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag
;		true if the second string is in the first;
;		leaves matching address, remaining length and true;
;		or leaves the first string and false

		CODE SEARCH,search
		push de			; save TOS
		exx			; save bc with ip
		pop bc			; pop u2->bc
		pop de			; pop c-addr2->de
		pop hl			; pop u1->hl
		or a			; 0->cf
		sbc hl,bc		; u1-u2->hl
		jr c,5$			; if u2>u1 then impossible search
		push bc			;
		pop ix			; u2->ix
		ld a,c			;
		or b			;
		jr z,4$			; if u2=0 then found
		ld c,l			;
		ld b,h			;
		inc bc			; u1-u2+1->bc correct for cpir
		pop hl			;
		push hl			; c-addr1->hl, keep c-addr1 on the stack
;		find char match
1$:		push de			; loop, save de with c-addr2
		ld a,(de)		;   [de]->a
		cpir		; 21/16	;   repeat until a=[hl++] or --bc=0
		jr nz,6$		;   if no match then not found
		pop de			;   restore de with c-addr2
		push bc			;
		push de			;
		push hl			;   save bc,de,hl
		push ix			;
		pop bc			;   u2->bc
;		compare substrings
		dec bc			;   u2-1->bc since u2>0
		ld a,c			;
		or b			;
		jr z,3$			;   if bc<> 0 then
		inc de			;     de++ to start matching at c-addr2+1
2$:		ld a,(de)	;  7	;     loop
		cpi		; 16	;       compare [hl++] to [de], --bc
		jr nz,3$	;  7	;       while characters [hl] and [de] are equal
		inc de		;  6	;       de++
		jp pe,2$	; 10(46);     until bc=0
3$:		pop hl			;
		pop de			;
		pop bc			;   restore bc,de,hl
		jr nz,1$		; repeat
;		substrings match
		dec hl			; hl-- to correct cpir overshoot
		ex (sp),hl		; save hl with c-addr3, discard c-addr1
		add ix,bc		; compute u3=u2+bc
4$:		push ix			; save ix with u3 as new 2OS
		exx			; restore bc with ip
		jp true_next		; set new TOS to TRUE
;		impossible search
5$:		add hl,bc		; u1-u2+u2=u1->hl
		push hl			; save hl with u1
		exx			; restore bc with ip
		jp false_next		; set new TOS to FALSE
;		not found
6$:		pop de			; pop to discard c-addr2
		pop de			;
		push de			; c-addr1->de, keep c-addr1 as 3OS
		sbc hl,de		; (c-addr1)+u1-de=u1->hl, cf=0 asserted
		push hl			; save hl with u1 as 2OS
		exx			; restore bc with ip
		jp false_next		; set new TOS to FALSE

; CMOVE		c-addr1 c-addr2 u --
;		move u bytes from c-addr1 to c-addr2 (from begin)
;
;    : CMOVE
;      SWAP >R
;      BEGIN DUP WHILE
;        NEXT-CHAR R@ C!
;        R> 1+ >R
;      REPEAT
;      RDROP
;      2DROP ;

		CODE CMOVE,cmove
		push de			; save TOS
		exx			; save bc with ip
		pop bc			; pop u->bc
		pop de			; pop c-addr2->de
		pop hl			; pop c-addr1->hl
		ld a,c			;
		or b			; test bc=0
		jr z,1$			; if bc<>0 then
		ldir			;   repeat [hl++]->[de++] until --bc=0
1$:		exx			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue

; CMOVE>	c-addr1 c-addr2 u --
;		move u bytes from c-addr1 to c-addr2 up (from end)

		CODE CMOVE>,cmoveup
		push de			; save TOS
		exx			; save bc with ip
		pop bc			; pop u->bc
		pop hl			; pop c-addr2->hl
		add hl,bc		;
		ex de,hl		; c-addr2+u->de
		pop hl			; pop c-addr1->hl
		add hl,bc		; c-addr1+u->hl
		ld a,c			;
		or b			; test bc=0
		jr z,1$			; if bc<>0 then
		dec de			;   c-addr2+u-1->de
		dec hl			;   c-addr1+u-1->hl
		lddr			;   repeat [hl--]->[de--] until --bc=0
1$:		exx			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue

; MOVE		c-addr1 c-addr2 u --
;		move u bytes from c-addr1 to c-addr2
;
;    : MOVE
;      -ROT
;      2DUP U< IF
;        ROT CMOVE>
;      ELSE
;        ROT CMOVE
;      THEN ;

		COLON MOVE,move
		.dw mrot,twodup,uless,doif,1$
		.dw   rot,cmoveup
		.dw   doexit
1$:		.dw rot,cmove
		.dw doret

; FILL		c-addr u char --
;		fill memory with char

		CODE FILL,fill
		ld a,e			; char->a
		exx			; save bc with ip
		pop bc			; pop u->bc
		pop de			; pop c-addr->de
		or a			; 0->cf
		ld hl,-1		;
		adc hl,bc		; bc-1->hl and set flags
		jr nc,1$		; if u<>0 then
		ld (de),a		;   char->[de]
		jr z,1$			;   if u<>1 then
		dec bc			;     bc-- since bc=u>1
		ld l,e			;
		ld h,d			;     c-addr->hl
		inc de			;     c-addr+1->de
		ldir			;     [hl++]->[de++] until --bc=0
1$:		exx			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue

; ERASE		c-addr u --
;		fill memory with zeros
;
;    : ERASE 0 FILL ;

		COLON ERASE,erase
		.dw zero,fill
		.dw doret

; BLANK		c-addr u --
;		fill memory with 0x20 (bl) chars
;
;    : ERASE BL FILL ;

		COLON BLANK,blank
		.dw bl,fill
		.dw doret

; CHOP		c-addr u1 char -- c-addr u2
;		truncate string up to matching char;
;		leaves string if char not found;
;		char=0x20 (bl) chops 0x00 to 0x20 (white space and control)

		CODE CHOP,chop
		ld a,e			; char->a
		exx			; save bc with ip
		ex af,af'		; save a with char
		pop bc			; pop u1->bc
		ld e,c			;
		ld d,b			; u1->de
		ld a,c			;
		or b			; test bc=0, 0->cf
		jr z,2$			; if bc=0 then not found
		pop hl			;
		push hl			; c-addr->hl
		ex af,af'		; restore a with char
		cp 0x20			;
		jr z,3$			; if a=0x20 then find white space
		or a			; 0->cf for when cpir ends with nz
;		find char in string
		cpir		; 21/16	; repeat until a=[hl++] or --bc=0
		jr nz,2$		; if match then
1$:		ccf			;   complement cf to correct cpi bc--
2$:		ex de,hl		; u1->hl
		sbc hl,bc		; u1-bc-cf->hl
		push hl			; save hl as TOS
		exx			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue
;		find white space or control char in string
3$:		cp (hl)		;  7	; loop to compare a to [hl]
		cpi		; 16	;   hl++,bc--
		jr nc,1$	;  7	;   if [hl]<a then found
		jp pe,3$	; 10	; until bc=0
		jr 1$			; not found

; TRIM		c-addr1 u1 char -- c-addr2 u2
;		trim initial chars;
;		char=0x20 (bl) trims 0x00 to 0x20 (white space and control)

		CODE TRIM,trim
		ld a,e			; char->a
		exx			; save bc with ip
		pop bc			; u1->bc
		pop hl			; c-addr1->hl
1$:		ex af,af'		; save a
		ld a,c			;
		or b			;
		jr z,3$			; if bc<>0 then
		ex af,af'		;   restore a
;		trim char from front of the string
2$:		cpi		; 16	;   loop
		jr nz,4$	;  7	;     while a=[hl++],--bc
		jp pe,2$	; 10	;   until b=0
;		done trimming
3$:		push hl			; save hl as 2OS
		push bc			; save bc as TOS
		exx			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue
4$:		cp 0x20			;
		jr nz,5$		; if char=0x20 then
;		trim white space and control char
		dec hl			;
		cp (hl)			;
		inc hl			;
		jr nc,1$		;   if [hl-1]<=0x20 then keep trimming
;		stop trimming at mismatch
5$:		inc bc			; correct bc++ for mismatch
		dec hl			; correct hl-- for mismatch
		jr 3$			; finalize trimming

; -TRIM		c-addr u1 char -- c-addr u2
;		trim trailing chars;
;		char=0x20 (bl) trims 0x00 to 0x20 (white space and control)

		CODE -TRIM,mtrim
		ld a,e			; char->a
		exx			; save bc with ip
		pop bc			; u1->bc
		pop hl			; c-addr1->hl
		push hl			; keep c-addr1
		add hl,bc		;
		dec hl			; (c-addr)+u1-1->hl trim from end
1$:		ex af,af'		; save a with char
		ld a,c			;
		or b			;
		jr z,3$			; if bc<>0 then
		ex af,af'		;   restore a with char
;		trim char from back of the string
2$:		cpd		; 16	;   loop
		jr nz,4$	;  7	;     while a=[hl--],--bc
		jp pe,2$	; 10	;   until b=0
;		done trimming
3$:		push bc			; save bc as TOS
		exx			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue
4$:		cp 0x20			;
		jr nz,5$		; if char=0x20 then
;		trim white space and control char
		inc hl			;
		cp (hl)			;
		dec hl			;
		jr nc,1$		;   if [hl+1]<=0x20 then keep trimming
;		stop trimming at mismatch
5$:		inc bc			; correct bc++ for cpd bc-- mismatch
		jr 3$			; finalize trimming

; -TRAILING	c-addr u1 -- c-addr u2
;		trim trailing white space and control characters
;
;    : -TRAILING BL -TRIM ;

		COLON -TRAILING,mtrailing
		.dw bl,mtrim
		.dw doret

; /STRING	c-addr1 u1 n -- c-addr2 u2
;		slice n characters off string
;
;    : /STRING ROT OVER + -ROT - ;

		CODE /STRING,slashstring
		pop hl			; pop u1->hl
		ex (sp),hl		; c-addr1->hl, save u1
		add hl,de		; c-addr1+n->hl
		ex (sp),hl		; u1->hl, save c-addr1+n as new 2OS
		or a			; 0->cf
		sbc hl,de		; u1-n->hl
		ex de,hl		; set new TOS to hl
		JP_NEXT			; continue

; NEXT-CHAR	c-addr1 u1 -- c-addr2 u2 char
;		get next char from string
;
;    : NEXT-CHAR OVER C@ >R 1- SWAP 1+ SWAP R> ;
;    : NEXT-CHAR OVER C@ -ROT 1- SWAP 1+ SWAP ROT ;

		CODE NEXT-CHAR,nextchar
		pop hl			; c-addr1->hl
		ld a,(hl)		; [c-addr1]->a
		inc hl			; c-addr1+1->hl
		push hl			; save c-addr1+1 as 3OS
		dec de			;
		push de			; save u1-1->de as new 2OS
		ld e,a			;
		ld d,0			; set new TOS to a with char
		JP_NEXT			; continue

;-------------------------------------------------------------------------------
;
;		OUTPUT
;
;-------------------------------------------------------------------------------

xy:
x:		.db 0			; cursor column 0 to win_cols-1
y:		.db 0			; cursor row 0 to win_rows-1

; X!		u --
;		set cursor column 0 to 23

		CODE X!,xstore
		ld a,e			; TOS->a
		ld (x),a		; a->[x]
		pop de			; pop new TOS
		JP_NEXT			; continue

; Y!		u --
;		set cursor row 0 to 5

		CODE Y!,ystore
		ld a,e			; TOS->a
		ld (y),a		; a->[y]
		pop de			; pop new TOS
		JP_NEXT			; continue

; X@		-- u
;		fetch cursor column

		CODE X@,xfetch
		push de			; save TOS
		ld a,(x)		; [x]->a
		ld e,a			;
		ld d,0			; set [x]->de as new TOS
		JP_NEXT			; continue

; Y@		-- u
;		fetch cursor row

		CODE Y@,yfetch
		push de			; save TOS
		ld a,(y)		; [y]->a
		ld e,a			;
		ld d,0			; set [y]->de as new TOS
		JP_NEXT			; continue

; AT-XY		u1 u2 --
;		set column x to u1 (0 to 23) and row y to u2 (0 to 5)
;
;    : AT-XY Y! X! ;

		COLON AT-XY,atxy
		.dw ystore
		.dw xstore
		.dw doret

; EMIT		char --
;		emit char;
;		supports the following control codes:
;		 8 (BS)
;		 9 (TAB)
;		10 (LF)
;		11 (VT scroll)
;		12 (FF clear)
;		13 (CR)
;		28 (right)
;		29 (left)
;		30 (up)
;		31 (down)

		CODE EMIT,emit
		ld hl,xy		; xy->hl
		ld a,e			; char->a
		cp 0x20			; test if control
		jr c,emit_check_bs		; if a<0x20 then handle control
		exx			; save regs
		ld de,(xy)		; x->e,y->d
		call PUTCHR		; PUTCHR at de (xy)
		exx			; restore regs
cursor_right:	ld a,(hl)		;
		cp win_cols-1		;
		jr nc,emit_crlf		; if [x]>=win_cols-1 then CRLF
		inc (hl)		; [x]+1->[x]
emit_exit:	pop de			; pop new TOS
		JP_NEXT			; continue
;		handle control chars
emit_check_bs:	cp 0x08			;
		jr nz,emit_check_tab	; if BS then
cursor_left:	dec (hl)		;   [x]--
		jp p,emit_exit		;   if [x]<0 then
		ld (hl),win_cols-1	;     win_cols-1->[x]
cursor_up:	inc hl			;
		dec (hl)		;     [y]--
		jp p,emit_exit		;     if [y]<0 then
		inc (hl)		;       [y]++
		exx			;
		ld de,0			;
		call INSLN		;       scroll down
		exx			;
		jr emit_exit		;   exit
emit_check_tab:	cp 0x09			;
		jr nz,emit_check_lf	; if TAB then
emit_tab:	ld a,(hl)		;
		and -8			;
		add 8			;
		cp win_cols		;
		ld (hl),a		;   ([x]&-8)+8->[x]
		jr c,emit_exit		;   if [x]<win_cols then exit
emit_crlf:	ld (hl),0		;   0->[x] carriage return
cursor_down:	inc hl			;
		ld a,(hl)		;
		cp win_rows-1		;
		jr nc,scroll_up		;   if [y]>=win_rows-1 then scroll up
		inc (hl)		;   [y]++
		jr emit_exit		;   exit
emit_check_lf:	cp 0x0a			;
		jr z,emit_crlf		; if LF then CRLF
emit_check_vt:	cp 0x0b			;
		jr nz,emit_check_ff	; if VT then
scroll_up:	exx			;   save regs
		call SCROLL		;   scroll up
		exx			;   restore regs
		jr emit_exit		;   exit
emit_check_ff:	cp 0x0c			;
		jr nz,emit_check_cr	; if FF then
emit_cls:	exx			;   save regs
		ld a,0x20		;
		ld b,win_rows*win_cols	;
		ld de,0			;
		ld (xy),de		;   0->[xy]
		call REPCHR		;   repeat space to clear screen
		exx			;   restore regs
		jr emit_exit		;   exit
emit_check_cr:	cp 0x0d			;
		jr nz,emit_check_rt	; if CR then
emit_cr:	ld (hl),0		;   0->[x]
		jr emit_exit		;   exit
emit_check_rt:	cp 0x1c			;
		jr z,cursor_right	; if RIGHT then cursor right
emit_check_lt:	cp 0x1d			;
		jr z,cursor_left	; if LEFT then cursor left
emit_check_up:	cp 0x1e			;
		jr z,cursor_up		; if UP then cursor up
emit_check_dn:	cp 0x1f			;
		jr z,cursor_down	; if DOWN then cursor down
		jr emit_exit		; exit

; TYPE		c-addr u --
;		type string to output
;
;    : TYPE
;      BEGIN DUP WHILE
;        NEXT-CHAR EMIT
;      REPEAT
;      2DROP ;

		COLON TYPE,type
1$:		.dw dup,doif,2$
		.dw   nextchar,emit
		.dw doagain,1$
2$:		.dw twodrop
		.dw doret

; CR		--
;		carriage return and line feed
;
;    : CR $A EMIT ;

		COLON CR,cr
		.dw dolit,0x0a,emit
		.dw doret

; SPACE		--
;		emit a space (BL)
;
;    : SPACE BL EMIT ;

		COLON SPACE,space
		.dw bl,emit
		.dw doret

; SPACES	n --
;		emit n spaces
;
;    : SPACES
;      DUP 0< IF
;        DROP
;        EXIT
;      THEN
;      0 ?DO SPACE LOOP ;

		COLON SPACES,spaces
		.dw dup,zeroless,doif,1$
		.dw   drop
		.dw   doexit
1$:		.dw zero,doqdo,3$
2$:		.dw   space
		.dw doloop,2$
3$:		.dw doret

; PAGE		--
;		clear screen
;
;    : PAGE $C EMIT ;

		COLON PAGE,page
		.dw dolit,0x0c,emit
		.dw doret

.if FULL

;+ DUMP		c-addr u --
;		dump memory in hex
;
;    : DUMP
;      BASE @ >R
;      HEX
;      0 ?DO
;        DUP ? 1+
;      LOOP
;      DROP
;      R> BASE ! ;

		COLON DUMP,dump
		.dw base,fetch,tor
		.dw hex
		.dw zero,doqdo,2$
1$:		.dw   dup,question
		.dw   oneplus
		.dw doloop,1$
2$:		.dw drop
		.dw rfrom,base,store
		.dw doret

.endif

;-------------------------------------------------------------------------------
;
;		PICTURED NUMERIC OUTPUT
;
;-------------------------------------------------------------------------------

; BASE		-- addr
;		variable with numeric base for conversion
;
;    VARIABLE BASE

		VARIABLE BASE,base
		.dw 10

; DECIMAL	--
;		set BASE to 10
;
;    : DECIMAL #10 BASE ! ;

		CODE DECIMAL,decimal
		ld hl,10		;
set_base:	ld (base+3),hl		; 10->[base]
		JP_NEXT			; continue

; HEX		--
;		set BASE to 16
;
;    : HEX #16 BASE ! ;

		CODE HEX,hex
		ld hl,16		;
		jr set_base		; 16->[base] continue

; HP		-- addr
;		hold pointer
;
;    0 VALUE HP

		VALUE HP,hp
		.dw 0

; <#		--
;		begin pictured numeric output
;
;    : <# HERE h_size + TO HP ;

		COLON <#,lesshash
		.dw here,dolit,h_size,plus,doto,hp+3
		.dw doret

; HOLD		char --
;		hold char for pictured numeric output
;
;    : HOLD HP 1- DUP TO HP C! ;

		COLON HOLD,hold
		.dw hp,oneminus,dup,doto,hp+3,cstore
		.dw doret

.if FULL

;+ HOLDS	c-addr u --
;		hold string for pictured numeric output
;
;    : HOLDS
;      BEGIN DUP WHILE
;        1- 2DUP + C@ HOLD
;      REPEAT
;      2DROP ;

		COLON HOLDS,holds
1$:		.dw dup,doif,2$
		.dw   oneminus,twodup,plus,cfetch,hold
		.dw doagain,1$
2$:		.dw twodrop
		.dw doret

.endif

; #		ud1 -- ud2
;		hold digit
;
;    : #
;      0 BASE @ UM/MOD >R
;      BASE @ UM/MOD
;      SWAP DUP #9 > IF
;        #7 +
;      THEN
;      '0 + HOLD
;      R> ;

		COLON #,hash
		.dw zero,base,fetch,umslashmod,tor
		.dw base,fetch,umslashmod,swap
		.dw dup,dolit,9,greater,doif,1$
		.dw   dolit,7,plus
1$:		.dw dolit,'0,plus,hold
		.dw rfrom
		.dw doret

; #S		ud -- 0 0
;		hold all remaining digits
;
;    : #S BEGIN # 2DUP D0= UNTIL ;

		COLON #S,hashs
1$:		.dw   hash,twodup,dzeroequal
		.dw dountil,1$
		.dw doret

; SIGN		n --
;		hold minus sign if n<0
;
;    : SIGN 0< IF '- HOLD THEN ;

		COLON SIGN,sign
		.dw zeroless,doif,1$
		.dw   dolit,'-,hold
1$:		.dw doret

; #>		ud -- c-addr u
;		end pictured numeric output, leave string
;
;    : #> 2DROP HP HERE h_size + OVER - ;

		COLON #>,hashgreater
		.dw twodrop
		.dw hp,here,dolit,h_size,plus,over,minus
		.dw doret

;-------------------------------------------------------------------------------
;
;		NUMERIC OUTPUT
;
;-------------------------------------------------------------------------------

; D.R		d +n --
;		output signed double d right aligned in field of +n chars wide
;
;    : D.R -ROT TUCK DABS <# #S ROT SIGN #> ROT OVER - SPACES TYPE ;

		COLON D.R,ddotr
		.dw mrot
		.dw tuck,dabs
		.dw lesshash,hashs,rot,sign,hashgreater
		.dw rot,over,minus,spaces,type
		.dw doret

; D.		d --
;		output signed double d with space
;
;    : D. 0 D.R SPACE ;

		COLON D.,ddot
		.dw zero,ddotr,space
		.dw doret

; U.R		u +n --
;		output unsigned u right aligned in field of +n chars wide
;
;    : U.R 0 SWAP D.R ;

		COLON U.R,udotr
		.dw zero,swap,ddotr
		.dw doret

; U.		u --
;		output unsigned u with space
;
;    : U. 0 D. ;

		COLON U.,udot
		.dw zero,ddot
		.dw doret

; .R		n +n --
;		output signed n right aligned in field of +n chars wide
;
;    : .R SWAP S>D ROT D.R ;

		COLON .R,dotr
		.dw swap,stod,rot,ddotr
		.dw doret

; .		n --
;		output signed n with space
;
;    : . S>D D. ;

		COLON .,dot
		.dw stod,ddot
		.dw doret

; ?		addr --
;		output signed cell stored at addr
;
;    : ? @ . ;

		COLON ?,question
		.dw fetch,dot
		.dw doret

;-------------------------------------------------------------------------------
;
;		PORT I/O
;
;-------------------------------------------------------------------------------

; OUT		u1 u2 --
;		output byte u1 to port u2

		CODE OUT,out
		ld a,e			; u2->a port
		pop de			; pop u1->de
		push bc			; save bc with ip
		ld c,a			;
		out (c),e		; u1->out(u2)
		pop bc			; restore bc with ip
		pop de			; set new TOS
		JP_NEXT			; continue

; INP		u1 -- u2
;		input from port u1

		CODE INP,inp
		push bc			; save bc with ip
		ld c,e			; u1->c
		in e,(c)		; in(u1)->e
		ld d,0			; 0->d
		pop bc			; restore bc with ip
		JP_NEXT			; continue

.if FULL

;+ BEEP		--
;		sound the speaker for a short ~2KHz beep

		CODE BEEP,beep
		di			;
		di			; disable interrupts
		xor a			; zero pattern
		ld l,a			;
		ld h,a			; reset counters
1$:		out (0x18),a		; loop, out to speaker port
2$:		dec l			;   loop
		jr nz,2$		;   until --l=0
		cpl			;   switch pattern on/off
		dec h			;
		jr nz,1$		; until --h=0
		ei			; enable interrupts
		JP_NEXT			; continue

.endif

;-------------------------------------------------------------------------------
;
;		PIXELS
;
;-------------------------------------------------------------------------------

; DRAW		c-addr u --
;		draw pixel patterns on screen at xy;
;		writes string c-addr u of pixel patterns at xy;
;		specify xy with AT-XY, xy not changed after DRAW

		CODE DRAW,draw
		push de			; save TOS
		exx			; save bc with ip
		pop bc			; pop u->bc
		ld b,c			; u->b
		pop hl			; pop c-addr->hl
		ld de,(xy)		; xy->dr
		call WRPSTR		; draw pixel string
		exx			; restore bc with ip
		pop de			; set new TOS
		JP_NEXT			; continue

; VIEW		c-addr u --
;		view screen pixels at xy;
;		read string of screen pixel patterns at xy into buffer c-addr u
;		specify xy with AT-XY, xy not changed after VIEW

		CODE VIEW,view
		push de			; save TOS
		exx			; save bc with ip
		pop bc			; pop u->bc
		ld b,c			; u->b
		pop hl			; pop c-addr->hl
		ld de,(xy)		; xy->de
		call RDPSTR		; read pixel string
		exx			; restore bc with ip
		pop de			; set new TOS
		JP_NEXT			; continue

; REVERSE	+n --
;		reverse video of the +n characters displayed at xy;
;		specify xy with AT-XY

		CODE REVERSE,reverse
		push de			; save TOS
		exx			; save bc with ip
		pop bc			; pop +n->bc
		ld de,(xy)		; xy->de
		ld hl,4$		; pixelbuffer->hl
		jr 3$			;
1$:		ld b,6			; loop, 6->b
		push de			;   save de with xy
		push hl			;   save hl with pixelbuffer
		call RDPSTR		;   read pixel string from screen
		pop hl			;   restore hl with pixelbuffer
		push hl			;   save hl with pixelbuffer
		ld b,6			;   6->b
2$:		ld a,(hl)		;   loop
		cpl			;
		ld (hl),a		;     ~[hl]->[hl]
		inc hl			;     hl++
		djnz 2$			;   until --b=0
		ld b,6			;   6->b
		pop hl			;   restore hl with pixelbuffer
		pop de			;   restore de with xy
		push de			;   save de with xy
		push hl			;   save hl with pixelbuffer
		call WRPSTR		;   write pixel string to screen
		pop hl			;   restore hl with pixelbuffer
		pop de			;   restore de with xy
		inc e			;   e++
3$:		dec c			;   c--
		jp p,1$			; until c<0
		exx			; restore bc with ip
		pop de			; set new TOS
		JP_NEXT			; continue
4$:		.db 0,0,0,0,0,0		; 6 byte pixel buffer

;-------------------------------------------------------------------------------
;
;		INPUT
;
;-------------------------------------------------------------------------------

; INKEY		-- x
;		check key, where 0x00=no key and 0x52=multiple keys

		CODE INKEY,inkey
		push de			; save TOS
		push bc			; save bc with ip
		call INKEY		; INKEY key code, changes bc and bc'
		pop bc			; restore bc with ip
		jp a_next		; set a->de new TOS and continue

.if FULL

;+ KEY-CLEAR	--
;		wait until no keys pressed
;
;    : KEY-CLEAR BEGIN INKEY 0= UNTIL ;

		CODE KEY-CLEAR,keyclear
		push bc			; save bc with ip and de with TOS
1$:		call INKEY		; INKEY key code, changes bc and bc'
		jr c,1$			; repeat until no key is depressed
		pop bc			; restore bc with ip
		JP_NEXT			; continue

;+ KEY?		-- flag
;		true if a key is pressed
;
;    : KEY? INKEY 0= 0= ;

		CODE KEY?,keyq
		push de			; save TOS
		push bc			; save bc with ip
		call INKEY		; INKEY key code, changes bc and bc'
		pop bc			; restore bc with ip
		jp true_if_c_next	; set new TOS to TRUE if cf=1

.endif

; GETKEY	-- char
;		wait and read key;
;		leaves ASCII char or special key code:
;		ON      =$05
;		BS      =$08
;		DEL     =$09
;		CA      =$0b
;		CLS     =$0c
;		ENTER   =$0d
;		DIGIT   =$0e
;		F-E     =$0f
;		INS     =$12
;		ANS     =$15
;		CONST   =$17
;		RCM     =$19
;		M+      =$1a
;		M-      =$1b
;		right   =$1c
;		left    =$1d
;		up      =$1e
;		down    =$1f;
;		calc keys and BASIC keys produce BASIC tokens as key code $fe:
;		SIN     =$fe register B=$95 BASIC token for SIN (ignored)

		CODE GETKEY,getkey
		push de			; save TOS
		push bc			; save bc with ip
		call GETCHR		; GETCHR ASCII key code, changes bc and bc'
		pop bc			; restore bc with ip
		jp a_next		; set a->de new TOS and continue

; KEY		-- char
;		display cursor and wait to read key;
;		same as GETKEY leaves ASCII char or special key code

		COLON KEY,key
		.dw one,reverse
		.dw getkey
		.dw one,reverse
		.dw doret

;-------------------------------------------------------------------------------
;
;		EDITOR
;
;-------------------------------------------------------------------------------

.macro		WORD word
set_'word:	ld (word),de
		pop de
		JP_NEXT
get_'word:	push de
		ld de,(word)
		JP_NEXT
word:		.dw 0
.endm

		; starting cursor xy
		WORD edit_atx
		WORD edit_aty

		; min <= cur <= len <= max
		WORD edit_buf		; buffer string address
		WORD edit_min		; minimum cursor position (e.g. prompt)
		WORD edit_cur		; cursor position
		WORD edit_len		; string length
		WORD edit_max		; buffer size, maximum string length

edit_toxy:	call docol		; n -- x y	cursor pos n to xy
		.dw get_edit_atx,plus
		.dw dolit,win_cols,slashmod
		.dw get_edit_aty,plus
		.dw doret

; EDIT		c-addr +n1 n2 n3 n4 -- c-addr +n5
;		edit buffer c-addr;
;		buffer size +n1;
;		string in buffer has length n2;
;		place cursor at n3;
;		non-editable left margin n4;
;		leaves c-addr and length +n5

		COLON EDIT,edit
		.dw set_edit_min
		.dw set_edit_cur
		.dw set_edit_len
		.dw set_edit_max
		.dw set_edit_buf
		; set initial cursor x y
		.dw xfetch,set_edit_atx
		.dw yfetch,set_edit_aty
		; type buffer to output
		.dw get_edit_buf,get_edit_len,type
		; adjust parameters to fit viewing window
1$:		.dw   get_edit_aty
		.dw   get_edit_len,edit_toxy,nip
		.dw   dolit,win_rows-1,minus,zero,max,minus
		.dw   set_edit_aty
		.dw   get_edit_len,edit_toxy,atxy,key
		; case
		.dw   dolit,0x0d,doof,2$
		; of ENTER
		.dw     get_edit_buf,get_edit_len
		.dw     doexit
2$:		.dw   dolit,0x08,doof,3$
		; of BS backspace
		.dw     get_edit_len,get_edit_min,ugreater,doif,5$
		.dw       get_edit_len,oneminus
		.dw       dup,set_edit_len
		.dw       dup,set_edit_cur
		.dw       edit_toxy,atxy,space
		.dw     doahead,5$
		; otherwise, append char if within $20 to $7e
3$:		.dw   dup,bl,dolit,0x7f,within,doif,4$
		.dw     get_edit_len,get_edit_max,uless,doif,4$
		.dw       dup,get_edit_len,edit_toxy,atxy,emit
		.dw       dup,get_edit_buf,get_edit_len,plus,cstore
		.dw       get_edit_len,oneplus,dup,set_edit_len,set_edit_cur
4$:		.dw   drop
		; endcase
5$:		.dw doagain,1$
		.dw doret

; ACCEPT	c-addr +n1 -- +n2
;		accept user input into buffer c-addr +n1;
;		leaves length +n2
;
;    : ACCEPT 0 0 0 EDIT NIP ;

		COLON ACCEPT,accept
		.dw zero,zero,zero,edit,nip
		.dw doret

;-------------------------------------------------------------------------------
;
;		PARSING
;
;-------------------------------------------------------------------------------

; >IN		-- addr
;		variable with offset into input buffer (TIB)
;
;    VARIABLE >IN

		VARIABLE >IN,toin
		.dw 0

; SOURCE-ID	-- 0|-1
;		value with 0=source input or -1=string input
;
;    0 VALUE SOURCE-ID

		VALUE SOURCE-ID,sourceid
		.dw 0

; SOURCE	-- c-addr u
;		double value with input source
;
;    TIB 0 2VALUE SOURCE

		TWOVALUE SOURCE,source
		.dw 0			; input length u
		.dw TIB0		; input buffer c-addr

.if 0 ; unused, but perhaps useful later

;- RESTORE-INPUT	x x x x 4 -- flag
;		restore input parameters from the stack

		COLON RESTORE-INPUT,restoreinput
		.dw drop,toin,store
		.dw dotwoto,source+3
		.dw doto,sourceid+3
		.dw false
		.dw doret

;- SAVE-INPUT	-- x x x x 4
;		save input parameters on the stack

		COLON SAVE-INPUT,saveinput
		.dw sourceid
		.dw source
		.dw toin,fetch
		.dw dolit,4
		.dw doret

.endif

; REFILL	-- flag
;		attempt to refill the input buffer;
;		leaves false when end of input

		COLON REFILL,refill
		.dw sourceid,invert,dup,doif,1$
		.dw   tib
		.dw   dup,dolit,tib_size
		.dw   accept,dotwoto,source+3
		.dw   toin,off
1$:		.dw doret

; SKIPS		char "<chars>" --
;		skips chars in input, 0x20 (bl) skips 0x00 to 0x20
;
;    : SKIPS SOURCE >IN @ /STRING ROT TRIM DROP SOURCE DROP - >IN ! ;

		COLON SKIPS,skips
		.dw source
		.dw toin,fetch,slashstring
		.dw rot,trim,drop
		.dw source,drop,minus,toin,store
		.dw doret

; PARSE		char "ccc<char>" -- c-addr u
;		parse "ccc" up to char when present
;
;    : PARSE SOURCE >IN @ /STRING ROT CHOP DUP 1+ >IN @ + SOURCE NIP UMIN >IN ! ;

		COLON PARSE,parse
		.dw source
		.dw toin,fetch,slashstring
		.dw rot,chop
		.dw dup,oneplus,toin,fetch,plus
		.dw source,nip,umin,toin,store
		.dw doret

; PARSE-WORD	char "<chars>ccc<char>" -- c-addr u
;		parse char-delimited word;
;		may throw -18 "parsed string overflow"
;
;    : PARSE-WORD
;      DUP SKIPS PARSE
;      DUP tmp_size-1 U> IF -18 THROW THEN ;

		COLON PARSE-WORD,parseword
		.dw dup,skips,parse
		.dw dup,dolit,tmp_size-1,ugreater,doif,1$
		.dw   dolit,-18,throw
1$:		.dw doret

; CHECK-NAME	c-addr u -- c-addr u
;		check if name is valid;
;		may throw -16 "attempt to use a zero-length string as a name";
;		may throw -19 "definition name too long"
;
;    : CHECK-NAME
;      DUP 0= IF -16 THROW THEN
;      DUP length_bits U> IF -19 THROW THEN ;

		COLON CHECK-NAME,checkname
		.dw dup,zeroequal,doif,1$
		.dw   dolit,-16,throw
1$:		.dw dup,dolit,length_bits,ugreater,doif,2$
		.dw   dolit,-19,throw
2$:		.dw doret

; PARSE-NAME	"<spaces>name<space>" -- c-addr u
;		parse space-delimited name;
;		check if name length is valid
;
;    : PARSE-NAME BL PARSE-WORD CHECK-NAME ;

		COLON PARSE-NAME,parsename
		.dw bl,parseword,checkname
		.dw doret

.if FULL

;+ WORD		char "<chars>ccc<char>" -- c-addr
;		parse word as a counted string
;
;    : WORD TMP DUP ROT PARSE-WORD ROT 2DUP C! 1+ SWAP CMOVE ;

		COLON WORD,word
		.dw tmp,dup,rot		; -- tmp tmp char
		.dw parseword,rot	; -- tmp c-addr u tmp
		.dw twodup,cstore	;
		.dw oneplus,swap	; -- tmp c-addr tmp+1 u
		.dw cmove
		.dw doret

.endif

; (		"ccc<paren>" --
;		start a comment block;
;		parse and skip input up to the closing )
;
;    : (
;      ') PARSE
;      BEGIN
;        + DROP
;        SOURCE + = IF
;          DROP REFILL
;        ELSE
;          C@ ') <> IF
;            REFILL
;          ELSE
;            FALSE
;          THEN
;        THEN
;      0= UNTIL ; IMMEDIATE

		COLON_IMM (,paren
1$:		.dw   dolit,'),parse
		.dw   plus,dup
		.dw   source,plus,equal,doif,2$
		.dw     drop,refill
		.dw   doahead,4$
2$:		.dw     cfetch,dolit,'),notequal,doif,3$
		.dw       refill
		.dw     doahead,4$
3$:		.dw       false
4$:		.dw   zeroequal
		.dw dountil,1$
		.dw doret

; \		"ccc<eol>" --
;		start a comment line;
;		parse and skip input up to the end of line;
;		note that the PC-G850 symbol for \ is ¥
;
;    : \ $A PARSE 2SROP ;

;		COLON_IMM \134,backslash	; this does not work
;		COLON_IMM ^|\134|,backslash	; this neither
;		COLON_IMM \,backslash		; this crashes the assembler
;		COLON_IMM \\,backslash		; this too
;		COLON_IMM ^|\|,backslash	; this too
		link = last_link		; expand macro manually
		last_link = .			; expand macro manually
		.dw link			; expand macro manually
		.db 0x81			; expand macro manually
		.str ^|\134|			; expand macro manually
		call docol			; expand macro manually
		.dw dolit,'\n,parse
		.dw twodrop
		.dw doret

; .(		"ccc<paren>" --
;		emit CR then type "ccc" up to the closing )
;
;    : .( ') PARSE CR TYPE ; IMMEDIATE

		COLON_IMM .(,dotparen
		.dw dolit,'),parse
		.dw cr,type
		.dw doret

; >DIGIT	char -- n
;		convert char digit to numeric digit when within BASE;
;		leaves -1 if char is invalid

		CODE >DIGIT,todigit
		ld a,d			;
		or a			; test d=0 TOS high byte
		jp nz,mone_next		; set new TOS to -1 if TOS high byte is nonzero
		ld a,e			; char->a
		cp '0			; test char<'0'
		jp c,mone_next		; set new TOS to -1 if char<'0'
		cp '9+1			; test char<='9'
		jr c,1$			; set new TOS to char-'0' if char<='9'
		and 0xdf		; make char upper case
		cp 'A			; test char<'A'
		jp c,mone_next		; set new TOS to -1 if char<'A'
		cp 'Z+1			; test char>'Z'
		jp nc,mone_next		; set new TOS to -1 if char>'Z'
		sub 7			; convert char 'A'..'Z' to n
1$:		sub '0			; convert char to n
		ld hl,base+3		; BASE->hl
		cp (hl)			; test n>=[BASE] using BASE low order byte
		jp nc,mone_next		; set new TOS to -1 if n>=[BASE]
		ld e,a			; set new TOS to n
		JP_NEXT			; continue

; >NUMBER	ud1 c-addr1 u1 -- ud2 c-addr2 u2
;		convert string to number;
;		updates accumulated double ud1 to ud2;
;		leaves string with the remaining unconvertable chars or empty
;
;    : >NUMBER
;      BEGIN DUP WHILE
;        NEXT-CHAR >DIGIT
;        DUP 0< IF
;          DROP -1 /STRING
;          EXIT
;        THEN
;        >R
;        2SWAP
;        BASE @ UMD*
;        R> M+
;        2SWAP
;      REPEAT ;

		COLON >NUMBER,tonumber
1$:		.dw dup,doif,3$
		.dw   nextchar,todigit
		.dw   dup,zeroless,doif,2$
		.dw     drop
		.dw     mone,slashstring
		.dw     doexit
2$:		.dw   tor
		.dw   twoswap
		.dw   base,fetch,umdstar
		.dw   rfrom,mplus
		.dw   twoswap
		.dw doagain,1$
3$:		.dw doret

; DBL		-- flag
;		true if >DOUBLE or NUMBER produced a double
;
;    0 VALUE DBL

		VALUE DBL,dbl
		.dw 0

; >DOUBLE	c-addr u -- d true | false
;		convert string to signed double;
;		leaves true if string is converted;
;		leaves false if string is unconvertable

		COLON >DOUBLE,todouble
		.dw false,doto,dbl+3
		.dw nextchar
		.dw dolit,'$,doof,1$
		.dw   dolit,16
		.dw doahead,6$
1$:		.dw dolit,'#,doof,2$
		.dw   dolit,10
		.dw doahead,6$
2$:		.dw dolit,'%,doof,3$
		.dw   dolit,2
		.dw doahead,6$
3$:		.dw dolit,'',doof,5$
		.dw   dolit,3,uless,doif,4$
		.dw     cfetch,stod
		.dw     true
		.dw     doexit
4$:		.dw   drop
		.dw   false
		.dw   doexit
		.dw doahead,6$
5$:		.dw   tor
		.dw   mone,slashstring
		.dw   base,fetch
		.dw   rfrom
		.dw drop
6$:		.dw base,fetch,tor
		.dw base,store
		.dw nextchar
		.dw dolit,'-,equal,duptor
		.dw invert,slashstring
		.dw zero,zero,twoswap
		.dw tonumber,dup,doif,7$
		.dw   true,doto,dbl+3
		.dw   nextchar
		.dw   dolit,'.,notequal,slashstring
		.dw   tonumber
7$:		.dw nip
		.dw doif,8$
		.dw   rdrop,rfrom,base,store
		.dw   twodrop
		.dw   false
		.dw   doexit
8$:		.dw rfrom,doif,9$
		.dw   dnegate
9$:		.dw rfrom,base,store
		.dw true
		.dw doret

.if FULL

;+ CHAR		"<spaces>name<space>" -- char
;		parse char;
;		note that the syntax 'char is preferred instead of this legacy word
;
;    : CHAR PARSE-NAME DROP C@ ;

		COLON CHAR,char
		.dw parsename,drop,cfetch
		.dw doret

.endif

;-------------------------------------------------------------------------------
;
;		DICTIONARY SEARCH
;
;-------------------------------------------------------------------------------

; L>NAME	lfa -- nt
;		convert link field address to name token (nfa)

		CODE L>NAME,ltoname
		jp twoplus		; same as 2+

; NAME>STRING	nt -- c-addr u
;		convert name token (nfa) to string

		COLON NAME>STRING,nametostring
		.dw count,dolit,length_bits,and
		.dw doret

; NAME>		nt -- xt
;		convert name token (nfa) to execution token (cfa)

		COLON NAME>,namefrom
		.dw nametostring,plus
		.dw doret

; >NAME		xt -- nt
;		convert execution token (cfa) to name token (lfa);
;		may throw -24 "invalid numeric argument"

		CODE >NAME,toname
		push bc			; save bc with ip
		ld b,0			; 0->b for add hl,bc
		ld hl,(context+3)	; CONTEXT->hl
;		loop over dictionary
1$:		ld a,(hl)	;  7	; loop
		inc hl		;  6	;
		ld h,(hl)	;  7	;
		ld l,a		;  4	;   [hl]->hl follow link
		or h		;  4	;
		jr z,3$		;  7	;   if hl=0 then throw -24
		push hl		; 11	;   save hl with lfa
		inc hl		;  6	;
		inc hl		;  6	;   hl+2->hl with nt (nfa)
		ld a,(hl)	;  7	;   get word length
		bit smudge_bit,a;  8	;
		jr nz,2$	;  7	;   if smudge bit not set then
		and length_bits	;  7	;     ignore control bits
		inc a		;  4	;
		ld c,a		;  4	;     word length+1
		add hl,bc	; 11	;     hl+length+1->hl with cfa
		sbc hl,de	; 15	;     test if hl=de with xt
2$:		pop hl		; 10	;   restore hl with lfa
		jr nz,1$	; 12	; until hl=xt matches
;		found the matching word
		inc hl			;
		inc hl			; hl+2-hl with nt (nfa)
		pop bc			; restore bc with ip
		ex de,hl		; set new TOS to hl
		JP_NEXT			; continue
;		not found
3$:		ld a,-24		;
		jp throw_a		; throw -24 "invalid numeric argument"

; >BODY		xt -- pfa
;		convert execution token to parameter field address

		CODE >BODY,tobody
		inc de			;
		jp twoplus		; set de+3->de new TOS and continue

; FIND-WORD	c-addr u -- c-addr 0 | xt 1 | xt -1
;		search dictionary for matching word;
;		leaves execution token and 1=immediate or -1=not immediate;
;		leaves c-addr and 0 when not found

		CODE FIND-WORD,findword
		ld a,d			;
		or a			; test d=0 high order byte of u
		jp nz,zero_next		; if u is too large then set new TOS to 0
		sla e			; shift u to compare w/o immediate bit
		jp c,zero_next		; if u is too large then set new TOS to 0
		jp z,zero_next		; if u=0 then set new TOS to 0
		push de			; save de with 2*u
		exx			; save bc with ip
		pop bc			; pop 2*u->bc
		pop de			; pop c-addr->de
		ld hl,(context+3)	; CONTEXT->hl
		jr 3$			; start searching
;		loop over dictionary
1$:		pop de			; restore de with c-addr
2$:		pop hl		; 10	; loop, restore hl with lfa
3$:		ld a,(hl)	;  7	;
		inc hl		;  6	;
		ld h,(hl)	;  7	;
		ld l,a		;  4	;   [hl]->hl follow link at hl=lfa
		or h		;  4	;
		jr z,6$		;  7	;   if hl=0 then not found
		push hl		; 11	;   save hl with lfa
		inc hl		;  6	;
		inc hl		;  6	;   hl+2->hl with nt (nfa)
		ld a,(hl)	;  7	;   word length
		add a		;  4	;   shift away immediate bit
		cp c		;  4	;   test a=c word length match (both shifted)
		jr nz,2$	; 12(95);   if lengths differ then continue searching
;		compare string to word
		push de			;   save de with c-addr
		inc hl			;   hl++ point to nfa chars
		ld b,c			;   2*u->b
		srl b			;   u->b word length (nonzero)
;		loop over word chars
4$:		ld a,(de)	;  7	;   loop
		cp (hl)		;  7	;     compare [de]=[hl]
		jr z,5$		; 12/7	;     if mismatch then
		and 0xdf	;    7	;       make upper case
		cp 'A		;    7	;
		jr c,1$		;    7	;       if a<'A' then continue search
		cp 'Z+1		;    7	;
		jr nc,1$	;    7	;       if a>'Z' then continue search
		xor (hl)	;    7	;
		and 0xdf	;    7	;       case insensitive compare [de]=[hl]
		jr nz,1$	;    7	;       if mismatch then continue search
5$:		inc de		;  6	;     de++ point to next char of c-addr
		inc hl		;  6	;     hl++ point to next char of word
		djnz 4$		; 13(51/102);until --b=0
;		found a matching word
		pop de			;   discard saved c-addr
		ex (sp),hl		;   save hl with xt as 2OS, restore hl with lfa
		inc hl			;
		inc hl			;   hl+2->hl with nt (nfa)
		bit immediate_bit,(hl)	;   test immediate bit of [hl] word length
		exx			;   restore bc with ip
		jp nz,one_next		;   set new TOS to 1 if word is immediate
		jp mone_next		;   set new TOS to -1
;		not found
6$:		push de			; save de with c-addr as 2OS
		exx			; restore bc with ip
		jp zero_next		; set new TOS to 0

; '		"<spaces>name<space>" -- xt
;		parse name and get execution token;
;		may throw -13 "undefined word"
;
;    : ' PARSE-NAME FIND-WORD 0= IF -13 THROW THEN ;

		COLON ',tick
		.dw parsename
		.dw findword,zeroequal,doif,1$
		.dw   dolit,-13,throw
1$:		.dw doret

.if FULL

;+ FIND		c-addr -- c-addr 0 | xt 1 | xt -1
;		search dictionary for counted string;
;		see FIND-WORD

		COLON FIND,find
		.dw count,findword
		.dw doret

.endif

; WORDS		--
;		display context vocabulary words

		COLON WORDS,words
		.dw cr
		.dw zero			; -- 0
		.dw context			; -- 0 l
1$:		.dw fetch,qdup,doif,5$
		.dw   dup,ltoname		; -- n l nfa
		.dw   dup,cfetch,dolit,smudge_bits,and,doif,2$
		.dw     drop
		.dw   doahead,4$
2$:		.dw     rot,swap		; -- l n nfa
		.dw     nametostring		; -- l n c-addr u
		.dw     rot,over,plus,oneplus	; -- l c-addr u n+u+1
		.dw     dup,dolit,142,ugreater,doif,3$
		.dw       drop
		.dw       dup			; -- l c-addr u u
		.dw       getkey,drop
		.dw       cr
3$:		.dw     mrot			; -- l u|n+u+1 c-addr u
		.dw     type,space
		.dw     swap			; -- u|n+u+1 l
4$:		.dw doagain,1$
5$:		.dw drop
		.dw doret

;-------------------------------------------------------------------------------
;
;		COMPILING
;
;-------------------------------------------------------------------------------

; HERE		-- addr
;		address of free memory after the dictionary;
;		new definitions are added here;
;		note that numeric output words use HERE for conversion

		CONSTANT HERE,here
		.dw end

; LASTXT	-- xt
;		leaves the last execution token defined
;
;    0 VALUE LASTXT

		VALUE LASTXT,lastxt
		.dw forth

; STATE		-- addr
;		compilation state;
;		STATE @ leaves TRUE when compiling;
;		STATE @ leaves FALSE when interpreting
;
;    VARIABLE STATE

		VARIABLE STATE,state
		.dw 0

; [		--
;		switch state to interpreting
;
;    : [ STATE OFF ;

		CODE_IMM [,leftbracket
		ld hl,0			; 0->hl
store_state:	ld (state+3),hl		; hl->STATE
		JP_NEXT			; continue

; ]		--
;		switch state to compiling
;
;    : ] STATE ON ;

		CODE ],rightbracket
		ld hl,-1		; -1->hl
		jr store_state		; hl->STATE and contnue

; HIDE		--
;		hide the last definition
;
;    : HIDE CURRENT @ L>NAME DUP C@ smudge_bits OR SWAP C! ;

		COLON HIDE,hide
		.dw current,fetch
		.dw ltoname,dup,cfetch
		.dw dolit,smudge_bits,or
		.dw swap,cstore
		.dw doret

; REVEAL	--
;		reveal the last definition
;
;    : REVEAL CURRENT @ L>NAME DUP C@ ~smudge_bits AND SWAP C! ;

		COLON REVEAL,reveal
		.dw current,fetch
		.dw ltoname,dup,cfetch
		.dw dolit,~smudge_bits,and
		.dw swap,cstore
		.dw doret

; IMMEDIATE	--
;		make the last definition immediate
;
;    : IMMEDIATE CURRENT @ L>NAME DUP C@ immediate_bits OR SWAP C! ;

		COLON IMMEDIATE,immediate
		.dw current,fetch
		.dw ltoname,dup,cfetch
		.dw dolit,immediate_bits,or
		.dw swap,cstore
		.dw doret

; ?COMP		--
;		check if compiling;
;		may throw -14 "interpreting a compile-only word"

		CODE ?COMP,qcomp
		ld a,(state+3)
		or a
		ld a,-14		; "interpreting a compile-only word"
throw_a_if_nz:	jp nz,next
throw_a:	push de			; Save TOS
		ld e,a
		ld d,-1			; set new TOS to negative error code in a
		jp throw		; THROW

; ?SYS		-- ; C: x --
;		check if compiled control structure matches x;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON ?SYS,qsys
		.dw qcomp
		.dw notequal,doif,1$
		.dw   dolit,-22,throw
1$:		.dw doret

; UNUSED	-- u
;		unused dictionary space
;
;    : UNUSED top @ HERE - ;

		CODE UNUSED,unused
		push de			; save TOS
		ld de,(here+3)		; HERE->de
		ld hl,(top)		; [top]->hl
		or a			; 0->cf
		sbc hl,de		; [top]-HERE->hl
		ex de,hl		; set new TOS to hl
		JP_NEXT			; continue

; ALLOT		n --
;		allocate n bytes starting from HERE in the dictionary;
;		undo the last ALLOT with negative n;
;		may throw -8 "dictionary overflow"

		CODE ALLOT,allot
		ld hl,(here+3)		; HERE->hl
		add hl,de		; hl+de->hl
allot_check:	ld de,(top)		; [top]->de
		or a			; 0->cf
		sbc hl,de		;
		add hl,de		; test hl<[top]
		ld a,-8			; -8 "dictionary overflow"
		jr nc,throw_a		;
		ld (here+3),hl		; hl->HERE
		pop de			; pop new TOS
		JP_NEXT			; continue

; COMPILE,	xt --
;		append execution token to dictionary;
;		may throw -8 "dictionary overflow"
;
;    : COMPILE, , ;

		CODE ^|COMPILE,|,compilecomma
		jr comma		; same as ,

; ,		x --
;		append cell to dictionary;
;		may throw -8 "dictionary overflow"

		CODE ^|,|,comma
		ld hl,(here+3)		; HERE->hl
comma_de:	ld (hl),e		;
		inc hl			;
		ld (hl),d		;
		inc hl			; de->[hl++]
		jr allot_check		; check and set hl->HERE

; C,		char --
;		append char to dictionary;
;		may throw -8 "dictionary overflow"

		CODE ^|C,|,ccomma
		ld hl,(here+3)		; HERE->hl
		ld (hl),e		;
		inc hl			; e->[hl++]
		jr allot_check		; check and set hl->HERE

; 2,		x1 x2 --
;		append double cell to dictionary;
;		may throw -8 "dictionary overflow"
;
;    : 2, , , ;

		COLON ^|2,|,twocomma
		.dw comma,comma
		.dw doret

; NFA,		"<spaces>name<space>" --
;		parse name and append dictionary entry with name;
;		set LASTXT to HERE;
;		may throw -8 "dictionary overflow"
;
;    : NFA, PARSE-NAME HERE CURRENT @ , CURRENT ! DUP C, HERE SWAP DUP ALLOT CMOVE HERE TO LASTXT ;

		COLON ^|NFA,|,nfacomma
		.dw parsename
		.dw here
		.dw current,fetch,comma
		.dw current,store
		.dw dup,ccomma
		.dw here,swap,dup,allot,cmove
		.dw here,doto,lastxt+3
		.dw doret

; CFA,		addr --
;		append cfa call addr to dictionary;
;		may throw -8 "dictionary overflow"

		CODE ^|CFA,|,cfacomma
		ld hl,(here+3)		; HERE->hl
		ld (hl),0xcd		; Z80 'call nn' opcode
		inc hl			; 0xcd->[hl++]
		jr comma_de		; append addr, check and set hl->HERE

; CFA:,		-- addr colon_sys
;		append cfa colon definition to dictionary;
;		make CONTEXT the CURRENT vocabulary;
;		start compiling;
;		may throw -8 "dictionary overflow"
;
;    : CFA:, ] HERE colon_sys ['] (:) CFA, CURRENT TO CONTEXT ;

		COLON ^|CFA:,|,cfacoloncomma
		.dw rightbracket
		.dw here
		.dw dolit,colon_sys
		.dw dolit,docol,cfacomma
		.dw current,doto,context+3
		.dw doret

; POSTPONE	"<spaces>name<space>" --
;		postpone compile action of name;
;		if name is immediate, then compile name instead of executing it;
;		otherwise compile name into the current colon definition;
;		can be used to create macros, e.g. : TRUE POSTPONE -1 ; IMMEDIATE;
;		may throw -13 "undefined word";
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM POSTPONE,postpone
		.dw qcomp
		.dw parsename,findword,qdup,zeroequal,doif,1$
		.dw   dolit,-13,throw
1$:		.dw zeroless,doif,2$
		.dw   literal
		.dw   dolit,compilecomma
2$:		.dw compilecomma
		.dw doret

.if FULL

;+ BUFFER:	n "<spaces>name<space>" -- ; -- addr
;		define buffer with n bytes;
;		executing name leaves address of n bytes
;
;    : BUFFER: CREATE ALLOT ;

		COLON ^|BUFFER:|,buffer
		.dw create
		.dw allot
		.dw doret

;+ :NONAME	-- xt
;		colon definition without name;
;		leaves execution token of definition to be used or saved

		COLON ^|:NONAME|,noname
		.dw here,dup,doto,lastxt+3
		.dw cfacoloncomma
		.dw doret

.endif

; :		-- ; C: "<spaces>name<space>" -- addr colon_sys
;		define name and start compiling
;
;    : : NFA, HIDE CFA:, ;

		COLON ^|:|,colon
		.dw nfacomma
		.dw hide
		.dw cfacoloncomma
		.dw doret

; ;		-- ; C: addr colon_sys --
;		end colon definition and stop compiling;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"
;
;    : ; ?COMP colon_sys <> IF -22 THROW THEN DROP POSTPONE (;) REVEAL [ ; IMMEDIATE

		COLON_IMM ^|;|,semicolon
		.dw dolit,colon_sys,qsys
		.dw drop
		.dw dolit,doret,compilecomma
		.dw reveal
		.dw leftbracket
		.dw doret

; EXIT		--
;		exit colon definition
;
;    : EXIT ?COMP POSTPONE (EXIT) ; IMMEDIATE

		COLON_IMM EXIT,exit
		.dw qcomp
		.dw dolit,doexit,compilecomma
		.dw doret

; CREATE	"<spaces>name<space>" -- ; -- addr
;		create name;
;		executing name leaves address (HERE addr after CREATE)
;
;    : NFA, ['] (VAR) CFA, ;

		COLON CREATE,create
		.dw nfacomma
		.dw dolit,dovar,cfacomma
		.dw doret

; DOES>		-- ; ... -- ...
;		change CREATE name behavior to execute code after DOES>
;
;    : DOES> ?COMP POSTPONE (;CODE) ['] (DOES) CFA, ; IMMEDIATE

		COLON_IMM DOES>,does
		.dw qcomp
		.dw dolit,doscode,compilecomma
		.dw dolit,dodoes,cfacomma
		.dw doret

; VARIABLE	"<spaces>name<space>" -- ; -- addr
;		define a variable;
;		executing name leaves address of value (initialized to zero)
;
;    : VARIABLE CREATE 0 , ;

		COLON VARIABLE,variable
		.dw create
		.dw zero,comma
		.dw doret

; 2VARIABLE	"<spaces>name<space>" -- ; -- addr
;		define a double variable;
;		executing name leaves address of double value (initialized to zero)
;
;    : 2VARIABLE CREATE 0 0 2, ;

		COLON 2VARIABLE,twovariable
		.dw create
		.dw zero,zero,twocomma
		.dw doret

; CONSTANT	x "<spaces>name<space>" -- ; -- x
;		define a constant;
;		executing name leaves x
;
;    : CONSTANT NFA, ['] (CON) CFA, , ;
;    : CONSTANT CREATE , DOES> @ ;

		COLON CONSTANT,constant
		.dw nfacomma
		.dw dolit,docon,cfacomma
		.dw comma
		.dw doret

; 2CONSTANT	x1 x2 "<spaces>name<space>" -- ; -- x1 x2
;		define a double constant;
;		executing name leaves x1 x2
;
;    : 2CONSTANT NFA, ['] (2CON) CFA, 2, ;
;    : 2CONSTANT CREATE 2, DOES> 2@ ;

		COLON 2CONSTANT,twoconstant
		.dw nfacomma
		.dw dolit,dotwocon,cfacomma
		.dw twocomma
		.dw doret

; VALUE		x "<spaces>name<space>" -- ; -- x
;		define a value;
;		executing name leaves x
;
;    : VALUE NFA, ['] (VAL) CFA, , ;

		COLON VALUE,value
		.dw nfacomma
		.dw dolit,doval,cfacomma
		.dw comma
		.dw doret

; 2VALUE	dx "<spaces>name<space>" -- ; -- dx
;		define a double value;
;		executing name leaves dx
;
;    : 2VALUE NFA, ['] (2VAL) CFA, 2, ;

		COLON 2VALUE,twovalue
		.dw nfacomma
		.dw dolit,dotwoval,cfacomma
		.dw twocomma
		.dw doret

; TO		"<spaces>name<space>" -- ; x --
;		assign value name;
;		may throw -32 "invalid name argument"
;
;    : TO
;      '
;      DUP VALUE? IF
;        >BODY
;        STATE @ IF
;          POSTPONE (TO)
;          ,
;          EXIT
;        THEN
;        !
;        EXIT
;      THEN
;      DUP 2VALUE? IF
;        >BODY
;        STATE @ IF
;          POSTPONE (2TO)
;          2,
;          EXIT
;        THEN
;        2!
;        EXIT
;      THEN
;      #-32 THROW ; IMMEDIATE

		COLON_IMM TO,to
		.dw tick
		.dw dup,valueq,doif,2$
		.dw   tobody
		.dw   state,fetch,doif,1$
		.dw     dolit,doto,compilecomma
		.dw     comma
		.dw     doexit
1$:		.dw   store
		.dw   doexit
2$:		.dw dup,twovalueq,doif,4$
		.dw   tobody
		.dw   state,fetch,doif,3$
		.dw     dolit,dotwoto,compilecomma
		.dw     twocomma
		.dw     doexit
3$:		.dw   twostore
		.dw   doexit
4$:		.dw dolit,-32,throw
		.dw doret

; +TO		"<spaces>name<space>" -- ; n --
;		increment value name;
;		may throw -32 "invalid name argument"
;
;    : +TO
;      '
;      DUP VALUE? IF
;        >BODY
;        STATE @ IF
;          POSTPONE (+TO)
;          ,
;          EXIT
;          THEN
;        +!
;        EXIT
;      THEN
;      #-32 THROW ; IMMEDIATE

		COLON_IMM +TO,plusto
		.dw tick
		.dw dup,valueq,doif,2$
		.dw   tobody
		.dw   state,fetch,doif,1$
		.dw     dolit,doplusto,compilecomma
		.dw     comma
		.dw     doexit
1$:		.dw   plusstore
		.dw   doexit
2$:		.dw dolit,-32,throw
		.dw doret

; DEFER		"<spaces>name<space>" -- ; ... -- ...
;		define a deferred name
;
;    : DEFER NFA, ['] (DEF) CFA, ['] UNDEF , ;

		COLON DEFER,defer
		.dw nfacomma
		.dw dolit,dodef,cfacomma
		.dw dolit,undef,comma
		.dw doret

; UNDEF		--
;		throw -256 "execution of an uninitialized deferred word"
;
;    : UNDEF -256 THROW ;

		CODE UNDEF,undef
		push de			; save TOS
		ld de,-256		; set new TOS
		jp throw		; THROW

; DEFER!	xt1 xt2 --
;		store xt1 in deferred xt2
;
;    : DEFER! >BODY ! ;

		CODE DEFER!,deferstore
		inc de			;
		inc de			;
		inc de			; >BODY
		jp store		; !

; DEFER@	xt1 -- xt2
;		fetch execution token from deferred xt1
;
;    : DEFER@ >BODY @ ;

		CODE DEFER@,deferfetch
		inc de			;
		inc de			;
		inc de			; >BODY
		jp fetch		; @

; IS		xt "<spaces>name<space>" --
;		assign execution token to deferred name;
;		may throw -32 "invalid name argument"
;
;    : IS
;      '
;      DUP DEFER? IF
;        STATE @ IF
;          LITERAL
;          POSTPONE DEFER!
;          EXIT
;        THEN
;        DEFER!
;        EXIT
;      THEN
;      #-32 THROW ; IMMEDIATE

		COLON_IMM IS,is
		.dw tick
		.dw dup,deferq,doif,2$
		.dw   state,fetch,doif,1$
		.dw     literal
		.dw     dolit,deferstore,compilecomma
		.dw     doexit
1$:		.dw   deferstore
		.dw   doexit
2$:		.dw dolit,-32,throw
		.dw doret

; ACTION-OF	"<spaces>name<space>" -- xt
;		fetch execution token of deferred name;
;		may throw -32 "invalid name argument"
;
;    : ACTION-OF
;      '
;      DUP DEFER? IF
;        STATE @ IF
;          LITERAL
;          POSTPONE DEFER@
;          EXIT
;        THEN
;        DEFER@
;        EXIT
;      THEN
;      #-32 THROW ; IMMEDIATE

		COLON_IMM ACTION-OF,actionof
		.dw tick
		.dw dup,deferq,doif,2$
		.dw   state,fetch,doif,1$
		.dw     literal
		.dw     dolit,deferfetch,compilecomma
		.dw     doexit
1$:		.dw   deferfetch
		.dw   doexit
2$:		.dw dolit,-32,throw
		.dw doret

; LITERAL	x -- ; -- x
;		compile a literal
;
;    : LITERAL ?COMP POSTPONE (LIT) , ; IMMEDIATE

		COLON_IMM LITERAL,literal
		.dw qcomp
		.dw dolit,dolit,compilecomma
		.dw comma
		.dw doret

; 2LITERAL	x1 x2 -- ; -- x1 x2
;		compile a double literal
;
;    : 2LITERAL ?COMP POSTPONE (2LIT) 2, ; IMMEDIATE

		COLON_IMM 2LITERAL,twoliteral
		.dw qcomp
		.dw dolit,dotwolit,compilecomma
		.dw twocomma
		.dw doret

; SLITERAL	c-addr u -- ; -- c-addr u
;		compile a string literal;
;		max literal string length is 255
;
;    : SLITERAL
;      ?COMP
;      DUP 255 U> IF -18 THROW THEN
;      POSTPONE (SLIT)
;      DUP C,
;      HERE OVER ALLOT SWAP CMOVE ; IMMEDIATE

		COLON_IMM SLITERAL,sliteral
		.dw qcomp
		.dw dup,dolit,255,ugreater,doif,1$
		.dw   dolit,-18,throw
1$:		.dw dolit,doslit,compilecomma
		.dw dup,ccomma
		.dw here,over,allot,swap,cmove
		.dw doret

; ."		"ccc<quote>" -- ; --
;		type "ccc" (compiled)
;
;    : ." '" PARSE SLITERAL POSTPONE TYPE ; IMMEDIATE

		COLON_IMM ^|."|,dotquote
		.dw dolit,'",parse
		.dw sliteral
		.dw dolit,type,compilecomma
		.dw doret

; S"		"ccc<quote>" -- ; -- c-addr u
;		leave string "ccc" (compiled and interpreted)
;
;    : S"
;      '" PARSE
;      STATE @ IF
;        SLITERAL
;        EXIT
;      THEN
;      TMP SWAP
;      2DUP 2>R
;      CMOVE
;      2R> ; IMMEDIATE

		COLON_IMM ^|S"|,squote
		.dw dolit,'",parse
		.dw state,fetch,doif,1$
		.dw   sliteral
		.dw   doexit
1$:		.dw tmp,swap
		.dw twodup,twotor
		.dw cmove
		.dw tworfrom
		.dw doret

.if FULL

;+ C"		"ccc<quote>" -- ; -- c-addr
;		leave counted string "ccc" (compiled);
;		may throw -18 "parsed string overflow"
;
;    : C" SLITERAL POSTPONE DROP POSTPONE 1- ;

		COLON_IMM ^|C"|,cquote
		.dw sliteral
		.dw dolit,drop,compilecomma
		.dw dolit,oneminus,compilecomma
		.dw doret

.endif

; VALUE?	xt -- flag
;		true if xt is a VALUE
;
;    : VALUE? DUP C@ $CD = SWAP 1+ @ ['] (VAL) = AND ;

		CODE VALUE?,valueq
		ld hl,doval		; (VAL)->hl
xt_is_hl:	ex de,hl		; hl->de, xt->hl
		ld a,0xcd		; Z80 'call nn' opcode
		cp (hl)			; test if [hl] Z80 'call nn' opcode
		jr nz,1$		; if [hl] Z80 'call nn' opcode then
		inc hl			;
		ld a,e			;
		cp (hl)			;
		jp nz,1$		;   if [++hl]=e then
		inc hl			;
		ld a,d			;
		cp (hl)			;     test [++hl]=d
1$:		ld a,0			; require a=0
		jp true_if_z_next	; set new TOS to TRUE if xt matches else FALSE

; 2VALUE?	xt -- flag
;		true if xt is a 2VALUE
;
;    : 2VALUE? DUP C@ $CD = SWAP 1+ @ ['] (2VAL) = AND ;

		CODE 2VALUE?,twovalueq
		ld hl,dotwoval		; (2VAL)->hl
		jr xt_is_hl		; set new TOS to TRUE if xt=(2VAL)

; DEFER?	xt -- flag
;		true if xt is a DEFER word
;
;    : DEFER? DUP C@ $CD = SWAP 1+ @ ['] (DEF) = AND ;

		CODE DEFER?,deferq
		ld hl,dodef		; (DEF)->hl
		jr xt_is_hl		; set new TOS to TRUE if xt=(DEF)

;-------------------------------------------------------------------------------
;
;		MARKER
;
;-------------------------------------------------------------------------------

.if FULL

;+ MARKER?	xt -- flag
;		true if xt is a MARKER word

		CODE MARKER?,markerq
		ld hl,marker_does	; marker_does->hl
		jr xt_is_hl		; set new TOS to TRUE if xt is a marker

;+ MARKER	"<spaces>name<space>" -- ; --
;		define a dictionary marker;
;		executing name deletes marker and all definitions made after;
;		beware of vocabulary definitions crossings

		COLON MARKER,marker
		.dw current,dup,fetch
		.dw here
		.dw create
		.dw comma
		.dw twocomma
		.dw doscode
marker_does:	call dodoes
		.dw dup,cellplus,twofetch
		.dw swap,doto,context+3
		.dw store
		.dw definitions
		.dw ltoname,namefrom,doto,lastxt+3
		.dw fetch,here,minus,allot
		.dw doret

;+ ANEW		"<spaces>name<space>" -- ; --
;		define a dictionary marker;
;		deletes previously defined name and all following definitions;
;		beware of vocabulary definitions crossings

		COLON ANEW,anew
		.dw toin,fetch,tor
		.dw parsename,findword
		.dw over,markerq
		.dw and,doif,1$
		.dw   execute
		.dw doahead,2$
1$:		.dw   drop
2$:		.dw rfrom,toin,store
		.dw marker
		.dw doret

.endif

;-------------------------------------------------------------------------------
;
;		FORGET
;
;-------------------------------------------------------------------------------

; FENCE		-- addr
;		only permit FORGET past the dictionary FENCE address
;
;    0 VALUE FENCE

		VALUE FENCE,fence
		.dw end

; FORGET	"<spaces>name<space>" --
;		delete name and all following definitions;
;		beware of vocabulary definitions crossings;
;		may throw -15 "invalid FORGET"

		COLON FORGET,forget
		.dw tick
		.dw dup,fence,uless,doif,1$
		.dw   dolit,-15,throw
1$:		.dw toname,twominus,context,current,umax
		.dw over,ugreater,doif,2$
		.dw   forth
2$:		.dw dup,fetch
		.dw dup,context,store
		.dw definitions
		.dw ltoname,namefrom,doto,lastxt+3
		.dw here,minus,allot
		.dw doret

;-------------------------------------------------------------------------------
;
;		BRACKET
;
;-------------------------------------------------------------------------------

; [']		"<spaces>name<space>" -- ; -- xt
;		compile xt of name as literal;
;		may throw -14 "interpreting a compile-only word"
;
;    : ['] ?COMP ' LITERAL ; IMMEDIATE

		COLON_IMM ['],brackettick
		.dw qcomp
		.dw tick,literal
		.dw doret

.if FULL

;+ [CHAR]	"<spaces>char" -- ; -- char
;		compile char as literal;
;		note that the syntax 'char is preferred instead of this legacy word;
;		may throw -14 "interpreting a compile-only word"
;
;    : [CHAR] ?COMP CHAR LITERAL ; IMMEDIATE

		COLON_IMM [CHAR],bracketchar
		.dw qcomp
		.dw char,literal
		.dw doret

;+ [COMPILE]	"<space>name<space>" -- ; ... -- ...
;		compile name;
;		note that POSTPONE is preferred instead of this legacy word;
;		may throw -14 "interpreting a compile-only word"
;
;    : [COMPILE] ?COMP ' COMPILE, ; IMMEDIATE

		COLON_IMM [COMPILE],bracketcompile
		.dw qcomp
		.dw tick,compilecomma
		.dw doret

.endif

;-------------------------------------------------------------------------------
;
;		CONTROL
;
;-------------------------------------------------------------------------------

; RECURSE	... -- ...
;		recursively call the currently defined word;
;		may throw -14 "interpreting a compile-only word"
;
;    : RECURSE ?COMP LASTXT COMPILE, ; IMMEDIATE

		COLON_IMM RECURSE,recurse
		.dw qcomp
		.dw lastxt,compilecomma
		.dw doret

; ?STACK	--
;		check parameter stack bounds;
;		may throw -3 "stack overflow";
;		may throw -4 "stack underflow"

		CODE ?STACK,qstack
		ld hl,(sp1)	; 16	; -1-[sp0]->hl
		add hl,sp	; 11	; test sp>sp0
		jr c,1$		;  7	; if sp>[sp0] then throw -4
		ld hl,(top)	; 16	; [top]->hl
		sbc hl,sp	; 15	; test sp<=[top]
		jr nc,2$	;  7	; if sp<=[top] then throw -3
		jp cont		; 10(82); continue with ON/BREAK check
1$:		ld a,-4			; throw -4 "stack underflow"
		jp throw_a
2$:		ld a,-3			; throw -3 "stack overflow"
		jp throw_a

; (UNTIL)	x --
;		branch if x=0;
;		runtime of the UNTIL compile-only word

		CODE (UNTIL),dountil
		ld a,e		;  4	;
		or d		;  4	; test if TOS=0
		pop de		; 10	; set new TOS
		jr z,doagain	; 12/7	; (AGAIN) if TOS=0
		inc bc		;    7	;
		inc bc		;    7	; ip+2->ip skip over jump target address
		jr qstack		; check stack and continue

; (IF)		x --
;		branch if x=0;
;		runtime of the IF and WHILE compile-only words

		CODE (IF),doif
		ld a,e		;  4	;
		or d		;  4	; test if TOS=0
		pop de		; 10	; set new TOS
		jr z,doahead	; 12/7 	; (AHEAD) if TOS=0
skip_jp_next:	inc bc		;    7	;
		inc bc		;    7	; ip+2->ip skip jump target address
		NEXT			; continue

; (AGAIN)	--
;		branch;
;		runtime of the AGAIN and REPEAT compile-only words

		CODE (AGAIN),doagain
		ld l,c		;  4	;
		ld h,b		;  4	;
		ld c,(hl)	;  7	;
		inc hl		;  6	;
		ld b,(hl)	;  7(28); [bc]->bc
		jr qstack		; check stack and continue

; (AHEAD)	--
;		branch;
;		runtime of the AHEAD, ELSE and ENDOF compile-only words

		CODE (AHEAD),doahead
		ld l,c		;  4	;
		ld h,b		;  4	;
		ld c,(hl)	;  7	;
		inc hl		;  6	;
		ld b,(hl)	;  7(28); [bc]->bc
		NEXT			; continue

; (OF)		x1 x2 -- x1 or x1 x2 --
;		branch if x1<>x2;
;		runtime of the OF compile-only word

		CODE (OF),doof
		pop hl			; pop x1->hl
		ex de,hl		; x2->hl,x1->de
		or a			; 0->cf
		sbc hl,de		; test x1=x2
		jr nz,doahead		; AHEAD if x1<>x2
		pop de			; pop new TOS
		inc bc			;
		inc bc			; ip+2->ip skip jump target address
		NEXT			; continue

; (LOOP)	--
;		repeat loop unless loop counter crosses the limit;
;		runtime of the LOOP compile-only word

		CODE (LOOP),doloop
		exx			; save bc with ip and de with TOS
		ld bc,1			; step size is 1 by default
loop_bc_step:	ld hl,(rp)		; [rp]->hl
		ld e,(hl)		;
		inc hl			; rp+1->hl
		ld d,(hl)		; [rp]->de sliced loop counter
		ex de,hl		; save [rp]->de, sliced loop counter in hl
		or a			; 0->cf
		adc hl,bc		; counter+step->hl set flags
		ex de,hl		; restore de->rp, counter+step->de
		jp pe,1$		; if overflow then exit loop
		ld (hl),d		;
		dec hl			; [rp]->hl
		ld (hl),e		; de->[rp] save updated counter
		exx			; restore bc with ip and de with TOS
		jr doagain		; AGAIN
1$:		ld bc,5			;
		add hl,bc		; rp+1+5->hl
		ld (rp),hl		; discard the loop parameters
		exx			; restore bc with ip and de with TOS
		inc bc			;
		inc bc			; ip+2->ip skip jump target address
		JP_NEXT			; continue
		
; (+LOOP)	--
;		increment counter and repeat loop unless counter crosses the limit;
;		runtime of the +LOOP compile-only word

		CODE (+LOOP),doplusloop
		pop hl			; pop new TOS to hl
		push de			; save old TOS with step size
		ex de,hl		; new TOS->de
		exx			; save bc with ip and de with new TOS
		pop bc			; pop TOS->bc with step size
		jr loop_bc_step		; (LOOP) with step

; (?DO)		n1|u1 n2|u2 --
;		begin loop with limit n1|u1 and initial value n2|u2;
;		skip loop when zero trip loop;
;		runtime of the ?DO compile-only word

		CODE (?DO),doqdo
		pop hl			;
		push hl			; 2OS->hl with loop limit
		or a			; 0->cf
		sbc hl,de		; test de=hl initial equals limit
		jr nz,dodo		; if de<>hl then (DO)
		pop hl			; discard 2OS
		pop de			; set new TOS
		jr doahead		; AHEAD

; (DO)		n1|u1 n2|u2 --
;		begin loop with limit n1|u1 and initial value n2|u2;
;		loop at least once;
;		runtime of the DO compile-only word

		CODE (DO),dodo
		ld a,(bc)		;
		ld l,a			;
		inc bc			;
		ld a,(bc)		;
		ld h,a			;
		inc bc			; [ip++]->hl LEAVE address
		ld ix,(rp)		; [rp]->ix
		dec ix			;
		ld (ix),h		;
		dec ix			;
		ld (ix),l		; save hl with the LEAVE address on the return stack
		ex de,hl		; excahange de with TOS to hl
		ex (sp),hl		; save TOS initial value
		ex de,hl		; set de to loop limit
		ld hl,0x8000		; slice the loop limit in de
		or a			; 0->cf
		sbc hl,de		; slice 0x8000-limit
		dec ix			;
		ld (ix),h		;
		dec ix			; save the sliced loop limit on the return stack
		ld (ix),l		;
		pop de			; restore TOS initial value
		add hl,de		; slice initial+0x8000-limit
		dec ix			;
		ld (ix),h		;
		dec ix			; save the sliced initial value on the return stack
		ld (ix),l		;
		ld (rp),ix		; ix->rp
		pop de			; pop new TOS
		JP_NEXT			; continue

; (UNLOOP)	R: ... --
;		remove loop parameters;
;		runtime of the UNLOOP compile-only word

		CODE (UNLOOP),dounloop
		exx			; save bc with ip
		ld hl,(rp)		; [rp]->hl
		ld bc,6			;
		add hl,bc		;
		ld (rp),hl		; rp+6->hl
		exx			; restore bc with ip
		JP_NEXT			; continue

; (LEAVE)	--
;		end loop by setting the loop counter to the limit;
;		runtime of the LEAVE compile-only word

		CODE (LEAVE),doleave
		ld hl,(rp)		;
		inc hl			;
		inc hl			;
		inc hl			;
		inc hl			; rp+4->hl
		ld c,(hl)		;
		inc hl			;
		ld b,(hl)		;
		inc hl			; [hl++]->ip
		ld (rp),hl		; rp+6->rp
		JP_NEXT			; continue

; AHEAD		-- ; C: -- addr orig
;		branch ahead to THEN;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM AHEAD,ahead
		.dw qcomp
		.dw dolit,doahead,compilecomma
		.dw here,dolit,orig
		.dw zero,comma
		.dw doret

; BEGIN		-- ; C: -- addr dest
;		begin WHILE REPEAT;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM BEGIN,begin
		.dw qcomp
		.dw here,dolit,dest
		.dw doret

; AGAIN		-- ; C: addr dest --
;		branch back to BEGIN;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM AGAIN,again
		.dw dolit,dest,qsys
		.dw dolit,doagain,compilecomma
		.dw comma
		.dw doret

; UNTIL		x -- ; C: addr dest --
;		branch back to BEGIN if x=0;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM UNTIL,until
		.dw dolit,dest,qsys
		.dw dolit,dountil,compilecomma
		.dw comma
		.dw doret

; IF		x -- ; C: -- addr orig
;		branch to closest ELSE or THEN if x=0;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM IF,if
		.dw qcomp
		.dw dolit,doif,compilecomma
		.dw here,dolit,orig
		.dw zero,comma
		.dw doret

; THEN		-- ; C: addr orig --
;		close AHEAD, IF, ELSE;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM THEN,then
		.dw dolit,orig,qsys
		.dw here,swap,store
		.dw doret

; ELSE		-- ; C: addr orig -- addr orig
;		close IF and branch to THEN;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM ELSE,else
		.dw ahead	; AHEAD
		.dw twoswap	; CS-ROLL
		.dw then	; POSTPONE THEN
		.dw doret

; WHILE		x -- ; C: addr sys -- addr orig addr sys
;		branch to exit REPEAT if x=0;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM WHILE,while
		.dw if		; POSTPONE IF
		.dw twoswap	; CS-ROLL
		.dw doret

; REPEAT	-- ; C: addr orig addr dest --
;		branch back to BEGIN after WHILE;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM REPEAT,repeat
		.dw again	; POSTPONE AGAIN
		.dw then	; POSTPONE THEN
		.dw doret

; DO		n1|u1 n2|u2 -- ; C: -- addr do_sys
;		begin loop from initial value n2|u2 to the limit n1|u1;
;		loop at least once;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM DO,do
		.dw qcomp
		.dw dolit,dodo,compilecomma
		.dw here
		.dw dolit,do_sys
		.dw zero,comma
		.dw doret

; ?DO		n1|u1 n2|u2 -- ; C: -- addr do_sys
;		begin loop from initial value n2|u2 to the limit n1|u1;
;		skip loop when zero trip loop;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM ?DO,qdo
		.dw qcomp
		.dw dolit,doqdo,compilecomma
		.dw here,dolit,do_sys
		.dw zero,comma
		.dw doret

; LOOP		-- ; C: addr do_sys --
;		repeat loop unless loop counter crosses the limit;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM LOOP,loop
		.dw dolit,do_sys,qsys
		.dw dolit,doloop,compilecomma
		.dw dup,twoplus,comma
		.dw here,swap,store
		.dw doret

; +LOOP		n|u -- ; C: addr do_sys --
;		increment counter and repeat loop unless counter crosses the limit;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM +LOOP,plusloop
		.dw dolit,do_sys,qsys
		.dw dolit,doplusloop,compilecomma
		.dw dup,twoplus,comma
		.dw here,swap,store
		.dw doret

; UNLOOP	--
;		remove loop parameters;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM UNLOOP,unloop
		.dw qcomp
		.dw dolit,dounloop,compilecomma
		.dw doret

; LEAVE		--
;		end loop by setting the loop counter to the limit;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM LEAVE,leave
		.dw qcomp
		.dw dolit,doleave,compilecomma
		.dw doret

; I		-- n
;		counter of innermost do loop

		CODE I,i
		push de			; save TOS
		ld hl,(rp)		; [rp]->hl
loop_counter:	ld e,(hl)		;
		inc hl			;
		ld d,(hl)		;
		inc hl			; [rp]->de with loop counter
		ld a,(hl)		;
		inc hl			;
		ld h,(hl)		;
		ld l,a			; [rp+2]->hl with loop limit
		ex de,hl		; exchange limit->de, counter->hl
		or a			; 0->cf
		sbc hl,de		; undo the do loop 'slice' counter-limit
		ex de,hl		; set new TOS to hl
		JP_NEXT			; continue

; J		-- n
;		counter of outer (second) do loop

		CODE J,j
		push de			; save TOS
		ld hl,(rp)		;
		ld de,6			;
		add hl,de		; rp+6->hl
		jr loop_counter		; execute I with rp+6

.if FULL

;+ K		-- n
;		counter of outer (third) do loop

		CODE K,k
		push de			; save TOS
		ld hl,(rp)		;
		ld de,12		;
		add hl,de		; rp+12->hl
		jr loop_counter		; execute I with rp+12

.endif

; CASE		x -- ; C: -- 0
;		begin CASE ENDCASE switch;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM CASE,case
		.dw qcomp
		.dw zero
		.dw doret

; OF		x1 x2 -- x1 or x1 x2 -- ; C: n1 -- orig n2
;		take CASE arm if x1=x2;
;		otherwise branch to next OF;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM OF,of
		.dw qcomp
		.dw oneplus,tor
		.dw dolit,doof,compilecomma
		.dw here,orig
		.dw zero,comma
		.dw rfrom
		.dw doret

; ENDOF		-- ; C: n -- orig n
;		branch to ENDCASE;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM ENDOF,endof
		.dw qcomp
		.dw tor
		.dw else
		.dw rfrom
		.dw doret

; ENDCASE	x -- ; C: n*orig n --
;		close CASE;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM ENDCASE,endcase
		.dw qcomp
		.dw dolit,drop,compilecomma
		.dw zero
		.dw doqdo,2$
1$:		.dw   then
		.dw doloop,1$
2$:		.dw doret

;-------------------------------------------------------------------------------
;
;		EXCEPTION HANDLING
;
;-------------------------------------------------------------------------------

; HANDLER	-- addr
;		variable with saved return stack pointer
;
;    VARIABLE HANDLER

		VARIABLE HANDLER,handler
		.dw 0

; EXECUTE	... xt -- ...
;		execute execution token xt

		CODE EXECUTE,execute
		ex de,hl		; xt->hl
		pop de			; set new TOS
		jp (hl)			; execute xt

; CATCH		... xt -- ... 0 or xt -- n
;		execute xt leaving nonzero exception code n or 0 when no exception occurred;
;		when an exception was caught, the parameter and return stacks are restored
;		to their state before execution of xt
;
;    : CATCH
;      SP@ >R
;      HANDLER @ >R
;      RP@ HANDLER !
;      EXECUTE
;      R> HANDLER !
;      RDROP
;      0 ;

		COLON CATCH,catch
		.dw spfetch,tor
		.dw handler,fetch,tor
		.dw rpfetch,handler,store
		.dw execute
		.dw rfrom,handler,store
		.dw rdrop
		.dw zero
		.dw doret

; THROW		0 -- or ... n -- ... n
;		throw exception n if nonzero
;
;    : THROW
;      ?DUP IF
;        HANDLER @ ?DUP IF
;          RP!
;          R> HANDLER !
;          R> SWAP >R
;          SP!
;          DROP
;          R>
;          EXIT
;        THEN
;        >R CLEAR R>
;        ERROR
;        REPL
;      THEN ;

		COLON THROW,throw
		.dw qdup,doif,2$
		.dw   handler,fetch,qdup,doif,1$
		.dw     rpstore
		.dw     rfrom,handler,store
		.dw     rfrom,swap,tor
		.dw     spstore
		.dw     drop
		.dw     rfrom
		.dw     doexit
1$:		.dw   tor,clear,rfrom
		.dw   error
		.dw   repl
2$:		.dw doret

; QUIT		... -- ; R: ... --
;		throw -56 "QUIT";
;		no exception error is displayed;
;		unlike ABORT, the parameter stack is not cleared
;
;    : QUIT -56 THROW ;

		CODE QUIT,quit
		ld a,-56		;
		jp throw_a		; throw -56 "QUIT"

; (ABORT")	... flag c-addr u -- ; R: ... --
;		if flag then abort with string message unless an active catch is present;
;		runtime of the ABORT" compile-only word;
;		throw -2 "ABORT""
;
;    : (ABORT")
;      ROT IF
;        HANDLER @ IF
;          2DROP
;        ELSE
;          TYPE
;        THEN
;        -2 THROW
;      THEN
;      2DROP ;

		COLON ^|(ABORT")|,doabortquote
		.dw rot,doif,3$
		.dw   handler,fetch,fetch,doif,1$
		.dw     twodrop
		.dw   doahead,2$
1$:		.dw     type
2$:		.dw   dolit,-2,throw
3$:		.dw twodrop
		.dw doret

; ABORT"	... flag -- ; C: "ccc<quote>" -- ; R: ... --
;		if flag then abort with string message unless an active catch is present;
;		throw -2 "ABORT"";
;		clears the parameter stack unless caught with CATCH;
;		may throw -14 "interpreting a compile-only word"
;
;    : ABORT" ?COMP POSTPONE S" POSTPONE (ABORT") ; IMMEDIATE

		COLON_IMM ^|ABORT"|,abortquote
		.dw qcomp
		.dw squote
		.dw dolit,doabortquote,compilecomma
		.dw doret

; ABORT		... -- ; R: ... --
;		throw -1 "ABORT";
;		clears the parameter stack unless caught with CATCH
;
;    : ABORT -1 THROW ;

		CODE ABORT,abort
		ld a,-1			;
		jp throw_a		; throw -1 "ABORT"

;-------------------------------------------------------------------------------
;
;		EVALUATION
;
;-------------------------------------------------------------------------------

; ERROR		n --
;		display exception n at the offending location in the input;
;		n=-1 ABORT and n=-2 ABORT" clear the stack;
;		n=-56 QUIT stays silent;
;		List of Forth850 errors:
;		
;		code | error
;		---- | ---------------------------------------------------------
;		-1   | ABORT
;		-2   | ABORT"
;		-3   | stack overflow
;		-4   | stack underflow
;		-8   | dictionary overflow
;		-10  | division by zero
;		-11  | result out of range
;		-13  | undefined word
;		-14  | interpreting a compile-only word
;		-15  | invalid FORGET
;		-16  | attempt to use zero-length string as a name
;		-18  | parsed string overflow
;		-19  | definition name too long
;		-22  | control structure mismatch
;		-24  | invalid numeric argument
;		-28  | user interrupt (BREAK was pressed)
;		-32  | invalid name argument (invalid TO name)
;		-56  | QUIT
;		-256 | execution of an uninitialized deferred word

		COLON ERROR,error
		.dw dup,mone,equal
		.dw over,dolit,-2,equal
		.dw or,doif,1$
		.dw   clear
		.dw   doexit
1$:		.dw dup,dolit,-56,equal,doif,2$
		.dw   drop
		.dw   doexit
2$:		.dw dup,dolit,-28,equal,doif,3$
		.dw   drop
		      SLIT ^|<BRK>|
		.dw   type
		.dw   doexit
3$:		.dw cr
		.dw source
		.dw toin,fetch
		.dw twodup,ugreater,plus,umin,type
		    SLIT ^|<ERR|
		.dw type
		.dw zero,dotr
		.dw dolit,'>,emit
		.dw doret

; NUMBER	c-addr u -- n|u|d|ud
;		convert string to number;
;		value DBL is set to -1 when the number is a double;
;		may throw -13 "undefined word" when string is not numeric

		COLON NUMBER,number
		.dw todouble,doif,4$
		.dw   dbl,invert,doif,1$
		.dw     dtos
1$:		.dw   state,fetch,doif,3$
		.dw     dbl,doif,2$
		.dw       twoliteral
		.dw       doexit
2$:		.dw     literal
3$:		.dw   doexit
4$:		.dw dolit,-13,throw
		.dw doret

; INTERPRET	--
;		interpret input while input is available

		COLON INTERPRET,interpret
1$:		.dw bl,parseword,qdup,doif,5$
		.dw   twodup
		.dw   findword,qdup,doif,3$
		.dw     twoswap,twodrop
		.dw     state,fetch,equal,doif,2$
		.dw       compilecomma
		.dw     doahead,4$
2$:		.dw       execute
		.dw   doahead,4$
3$:		.dw     drop
		.dw     number
4$:		.dw doagain,1$
5$:		.dw drop
		.dw doret

; EVALUATE	... c-addr u -- ...
;		evaluate string

.if 0 ; version with SAVE-INPUT and RESTORE-INPUT

		COLON EVALUATE,evaluate
		.dw saveinput,ntor
		.dw dotwoto,source+3
		.dw toin,off
		.dw mone,doto,sourceid+3
		.dw dolit,interpret,catch,dup,doif,1$
		.dw   dup,error
1$:		.dw nrfrom,restoreinput,drop
		.dw throw
		.dw doret

.else

		COLON EVALUATE,evaluate
		.dw sourceid,tor
		.dw source,twotor
		.dw toin,fetch,tor
		.dw dotwoto,source+3
		.dw toin,off
		.dw mone,doto,sourceid+3
		.dw dolit,interpret,catch,dup,doif,1$
		.dw   dup,error
1$:		.dw rfrom,toin,store
		.dw tworfrom,dotwoto,source+3
		.dw rfrom,doto,sourceid+3
		.dw throw
		.dw doret

.endif

.if FULL

;+ TEXT		--
;		read and evaluate TEXT editor area with Forth source code;
;		caveat: .( and ( in TEXT cannot span more than one line, they end at EOL
;
;    : TEXT
;      $7973 @ 1+ >R
;      BEGIN
;        R>                  \ -- addr
;      DUP C@ $FF <> WHILE
;        2+ DUP C@ SWAP 1+   \ -- len addr
;        2DUP + >R
;        SWAP 1- EVALUATE
;      REPEAT
;      DROP ;

		COLON TEXT,text
		.dw dolit,0x7973,fetch,oneplus,tor
1$:		.dw rfrom
		.dw dup,cfetch,dolit,0xff,notequal,doif,2$
		.dw   twoplus,dup,cfetch,swap,oneplus
		.dw   twodup,plus,tor
		.dw   swap,oneminus,evaluate
		.dw doagain,1$
2$:		.dw drop
		.dw doret

.endif

; REPL		--
;		read-evaluate-print loop
;
;    : REPL
;      rp0 @ RP!
;      HANDLER OFF
;      0 TO SOURCE-ID
;      CR
;      [
;      BEGIN
;        BEGIN ['] REFILL CATCH ?DUP WHILE
;          ERROR CR
;        REPEAT
;      WHILE
;        SPACE
;        ['] INTERPRET CATCH ?DUP IF
;          ERROR
;          REPL
;        THEN
;        STATE @ INVERT IF
;          ." OK["
;          DEPTH 0 U.R
;          '] EMIT
;        THEN
;        CR
;      REPEAT
;      BYE ;

		COLON REPL,repl
		.dw dolit,rp0,fetch,rpstore
		.dw handler,off
		.dw zero,doto,sourceid+3
		.dw cr
		.dw leftbracket
1$:		.dw   dolit,refill,catch,qdup,doif,9$
		.dw     error,cr
		.dw   doagain,1$
9$:		.dw doif,4$
		.dw   space
		.dw   dolit,interpret,catch,qdup,doif,2$
		.dw     error
		.dw     repl
2$:		.dw   state,fetch,invert,doif,3$
		        SLIT ^| OK[|
		.dw     type
		.dw     depth,zero,udotr
		.dw     dolit,'],emit
3$:		.dw   cr
		.dw doagain,1$
4$:		.dw bye
		.dw doret

; BYE		--
;		return to BASIC

		COLON BYE,bye
		; clean BASIC tokenized input buffer 0x7d00
		.dw dolit,0x7d00,dolit,256,dolit,0x0d,fill
		.dw page
		.dw halt
halt:		ld sp,(bsp)		; restore BASIC sp
		ret			; return to BASIC or Monitor

;-------------------------------------------------------------------------------
;
;		TESTS
;
;-------------------------------------------------------------------------------

.if TEST
.include "tests.asm"
.endif

;-------------------------------------------------------------------------------
;
;		VOCABULARY
;
;-------------------------------------------------------------------------------

; CONTEXT	-- addr
;		leaves address of link of the last vocabulary context definition
;
;    ' FORTH VALUE CONTEXT

		VALUE CONTEXT,context
		.dw forth+3

; CURRENT	-- addr
;		leaves address of link of the last current vocabulary definition
;
;    ' FORTH VALUE CURRENT

		VALUE CURRENT,current
		.dw forth+3

; DEFINITIONS	--
;		make CURRENT the CONTEXT vocabulary
;
;    : DEFINITIONS CONTEXT TO CURRENT ;

		COLON DEFINITIONS,definitions
		.dw context,doto,current+3
		.dw doret

; VOCABULARY	"<spaces>name<space>" --
;		define a new vocabulary
;
;    : VOCABULARY CREATE , fig_kludge , DOES> TO CONTEXT ;

		COLON VOCABULARY,vocabulary
		.dw current
		.dw create
		.dw comma
		.dw dolit,fig_kludge,comma
		.dw doscode
vocabulary_does:call dodoes
		.dw doto,context+3
		.dw doret

; FORTH		--
;		make FORTH the CONTEXT vocabulary
;
;    VOCABULARY FORTH

		CODE FORTH,forth
		call vocabulary_does
		.dw last_link
		.dw fig_kludge

end:

.end
