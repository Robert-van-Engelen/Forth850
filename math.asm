;-------------------------------------------------------------------------------
;
;		Z80 IEEE 754 FLOATING POINT MATH LIBRARY
;
; Author:
;   Dr. Robert van Engelen, Copyright 2022
;
; Features:
;   - IEEE 754 single precision floating point:
;     addition, subtraction, negation, multiplication, division,
;     integer <-> float conversion, string <-> float conversion
;   - "memoryless" using registers only (+shadow), at most one push+pop per flop
;   - optimized for speed and reduced code size (no loop unrolling)
;   - extensively tested
;
; Sacrifices:
;   - no INF/NAN values; routines return error condition (cf set)
;   - truncated 24 bit mantissa instead of IEEE 754 unbiased "banker's rounding"
;     (floating point string parsing and output do apply rounding though)
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
;		FLOATING POINT REPRESENTATION
;
;-------------------------------------------------------------------------------
;
;		IEEE 754 binary32 single precision floating point
;		assigned to CPU registers BCDE
;		 _____________________________________
;		|s|   exp   |        mantissa         |
;		|s|_________|_man2__|__man1__|__man0__|
;		|s eeeeeee|e mmmmmmm|mmmmmmmm|mmmmmmmm|
;		|____B____|____C____|____D___|____E___|
;
;		exponent bits:	8
;		exponent bias:	127 (0x7f) for IEEE 754 or 128 (0x80) symmetry
;		exponent zero:	indicates zero floating point value
;		mantissa bits:	24 (including implicit msb of 1)
;		infinity/nan:	no (errors are indicated with cf set)
;		range:		2^-126 to 2^+127 assuming bias = 127
;				2^-127 to 2^+127 assuming bias = 128
;
;		examples with exponent bias = 127:
;
;		   0 = 00 00 00 00
;		   1 = 3f 80 00 00
;		   2 = 40 00 00 00
;		   3 = 40 40 00 00
;		  -0 = 80 00 00 00
;		  -1 = bf 80 00 00
;		  -2 = c0 00 00 00
;		  -3 = c0 40 00 00
;		 inf = 7f 80 00 00   n/a (invalid value)
;		-inf = ff 80 00 00   n/a (invalid value)
;		 nan = s 11111111 xxxxxxx xxxxxxxx xxxxxxxx at least one x is 1
;		                     n/a (invalid value)
;
;		IEEE 754 binary floating point allows floating point values to
;		be compared as if comparing 32 bit signed integers:
;
;		 0 = 00 00 00 00 (although -0 = 80 00 00 00 is nonzero)
;		 1 = 3f 80 00 00 < 2 = 40 00 00 00
;		-1 = bf 80 00 00 < 2 = 40 00 00 00
;
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;
;		CONSTANTS
;
;-------------------------------------------------------------------------------

bias		.equ 127		; exponent bias 127 IEEE 754 or 128

;-------------------------------------------------------------------------------
;
;		ASSEMBLY MACROS
;
;-------------------------------------------------------------------------------

; Extract the exponent of float bcde

.macro		EXP
		ld a,c			;
		add a			; set cf to lowest order exponent bit
		ld a,b			;
		adc a			; exponent -> a, set z if zero, set cf if negative
.endm

; Test if float bcde is zero

.macro		ISZERO
		ld a,c			;
		and 0x80		;
		or b			; set z if float is zero (but not negative zero)
.endm

; Test if float bcde is positive or zero

.macro		ISPOSZ
		bit 7,b			; set z if float is positive or zero
.endm

;-------------------------------------------------------------------------------
;
;		FLOATING POINT NEGATION
;
;		fneg:	- bcde -> bcde
;			no errors (cf reset)
;			a,b modified
;
;-------------------------------------------------------------------------------

fneg:		ld a,b			; sign bit 7 and exponent
		xor 0x80		; invert sign bit 7
		ld b,a			; set new sign bit 7
		ret			; return bcde (cf reset)

;-------------------------------------------------------------------------------
;
;		FLOATING POINT SUBTRACTION
;
;		fsubx:	bcde - bcde' -> bcde
;		fsuby:	bcde' - bcde -> bcde
;			cf set on overflow
;			a,b,c,d,e,h,l,a',b',c',d',e',h',l' modified
;
;-------------------------------------------------------------------------------

fsubx:		exx			; swap bcde with bcde'
fsuby:		call fneg		; - bcde -> bcde
		; FALL through		; - bcde + bcde' -> bcde

;-------------------------------------------------------------------------------
;
;		FLOATING POINT ADDITION
;
;		fadd:	bcde + bcde' -> bcde
;			cf set on overflow
;			a,b,c,d,e,h,l,a',b',c',d',e',h',l' modified
;
;-------------------------------------------------------------------------------

fadd:		EXP			;
		ld h,a			; exponent -> h
		exx			; activate bcdehl'
		jr z,done		; if bcde is zero then return bcde'
		EXP			;
		ld h,a			; exponent' -> h'
		exx			; activate bcdehl
		jr z,done		; if bcde' is zero then return bcde

		; compare the exponents

		sub h			; exponent' - exponent -> a
		jr nc,1$		; if exponent' < exponent then
		neg			;   exponent - exponent' -> a
		exx			;   swap bcdehl with bcdehl' (commutative symmetry)
1$:		exx			; activate bcdehl' (symmetry allows ignoring the swap)
		cp 24			; if exponent' - exponent >= 24 then
		ret nc			;   return bcde' (cf reset)

		; save signs and set mantissas high bits

		ld l,b			; save sign' b' bit 7 to l'
		ld b,h			; save h' -> b' exponent'
		set 7,c			; set bit 7 of man2' c'
		exx			; activate bcdehl
		ld l,b			; save sign b bit 7 to l
		set 7,c			; set bit 7 of man2 c

		; align mantissa cde to cde' and set result exponent b = exponent'

		or a			;
		jr z,3$			; if exponent' <> exponent then
		ld b,a			;   exponent' - exponent -> b loop counter
2$:		srl c		;  8	;   loop
		rr d		;  8	;
		rr e		;  8	;     cde >> 1 -> cde
		djnz 2$		; 13(37);   repeat exponent' - exponent times
3$:		add h			;
		ld b,a			; exponent' - exponent + exponent -> b = exponent'

		; compare signs

		ld a,l			; sign -> a
		exx			; activate bcdehl'
		xor l			; sign xor sign' -> a
		jp m,subtract		; if signs differ then subtract

		; add mantissa cde to cde' to produce result mantissa ade

		ld a,c			; c' -> a
		push de			; push de'
		exx			; activate bcdehl
		pop hl			; pop hl with de'
		add hl,de		; de' + de -> hl
		adc c			; c' + c -> a with result man2
shiftoncarry:	ex de,hl		; hl -> de result mantissa ade
		jr nc,finalize		; if no carry then finalize

		; shift mantissa ade right if carry after addition

		inc b			; increment result exponent b
		ret z			; if exponent = 0 then return overflow error (cf set)
	        srl a			;
		rr d			;
		rr e			; shift right result mantissa ade

finalize:	; finalize bade to return bcde using sign' l' bit 7

.if bias - 128	; check overflow when bias = 127, for bias = 128 the result exponent <= bias + 127 = 255
		inc b			; check if result exponent overflowed
		scf			; set cf
		ret z			; if result exponent overflowed then return error (cf set)
		dec b			;
.endif

		add a			; shift left man2 a to assign man2 bit 7 below
		exx			;
		rl l			; set cf to the sign' l' bit 7
		exx			;
		rr b			; rotate right result exponent b and assign sign' bit
		rra			; rotate right result man2 a and assign exponent bit
		ld c,a			; a -> c to set result man2
done:		xor a			; 0 -> a, reset cf, reset v, set z
		ret			; return bcde (cf reset)

resubtract:	; redo subtract mantissa cde from cde' to produce result mantissa ade

		exx			;
		ld a,l			; sign l -> a
		exx			;
		cpl			; complement a with sign bit 7
		ld l,a			; restore sign' l' bit 7 to the complement of sign l bit 7

subtract:	; subtract mantissa cde from cde' to produce result mantissa ade

		ld a,c			; c' -> a
		push de			; push de'
		exx			; activate bcdehl
		pop hl			; pop hl with de'
		or a			; reset cf
		sbc hl,de		; de' - de -> hl
		sbc c			; c' - c -> a
		jr c,resubtract		; if cde' < cde then swap cde with cde' and redo subtract
		ex de,hl		; hl -> de with result mantissa ade
		jp m,finalize		; if man2 a bit 7 is set then finalize bade.cf to return bcde

normalize:	; normalize bade with cf when shifting to return bcde using sign' l' bit 7

		ld c,a			; save a -> c
		or d			;
		or e			;
		jr z,underflow		; if ade = 0 then underflow
		ld a,c			; restore c -> a
		ex de,hl		; de -> hl
1$:		dec b		;  4	; decrement result exponent b
		jr z,underflow	;  7	; if exponent = 0 then underflow
		add hl,hl	; 11	;
		adc a		;  4	; ahl << 1 + cf -> ahl to normalize mantissa, cf is reset
		jp p,1$		; 10(36); if man2 bit 7 not set yet then loop
		ex de,hl		; hl -> de
		jr finalize		; finalize

underflow:	; underflow to zero, no subnormal forms

fzero:		; zero result bcde

		xor a			; 0 -> a and reset cf
		ld b,a			;
		ld c,a			;
		ld d,a			; 0 -> bcde
		ld e,a			;
		ret			; return bcde (cf reset, v reset, z set)

;-------------------------------------------------------------------------------
;
;		FLOATING POINT MULTIPLICATION
;
;		fmul:	bcde * bcde' -> bcde
;			cf set on overflow
;			a,b,c,d,e,h,l,a',b',c',d',e',h',l' modified
;
;-------------------------------------------------------------------------------

fmul:		EXP			;
		jr z,fzero		; if bcde is zero then return zero
		sub bias		; subtract exponent bias
		ld h,a			; exponent - bias -> h
		exx			; activate bcdehl'
		EXP			;
		jr z,fzero		; if bcde' is zero then return zero
		sub bias		; subtract exponent bias
		exx			; activate bcdehl

		; add unbiased exponents to produce result exponent

		add h			; (exponent' - bias) + (exponent - bias) -> a
		jp po,1$		; if under/overflow then
		add a			;   carry if bit 7 set
		jr nc,underflow		;   if incorrect positive then underflow
		ret			;   return with overflow error (cf set)

		; save biased result exponent and sign

1$:		add bias		; bias the result exponent
		jr z,underflow		; if result exponent = 0 then underflow
		ex af,af'		; save result biased exponent to a'
		ld a,b			; b -> a with sign bit 7
		set 7,c			; set bit 7 of man2 c
		exx			; activate bcdehl'
		xor b			; sign xor sign' -> a with result sign bit 7
		ld l,a			; save result sign to l' bit 7
		set 7,c			; set bit 7 of man2' c'
		exx			; activate bcdehl

		; multiply mantissas cde * cde' -> ahl.cde'

		xor a			; 0 -> a and reset cf
		ld l,a			;
		ld h,a			; 0 -> hl
		ld b,24			; 24 -> b loop counter
2$:		rra		;  4	; loop
		rr h		;  8	;
		rr l		;  8	;
		exx		;  4	;
		rr c		;  8	;
		rr d		;  8	;
		rr e		;  8	;   cf + ahl.cde' >> 1 -> ahl.cde'
		exx		;  4	;
		jr nc,3$	; 12/7	;   if carry then
		add hl,de	;   11	;
		adc c		;    4	;     ahl + cde -> ahl
3$:		djnz 2$		; 13(87); until --b = 0

		; restore result exponent b and normalize bahl to return result bcde

		ex af,af'		; restore result exponent a', save a and flags
		ld b,a			; a' -> b with result exponent
		ex af,af'		; restore a and flags
		jp shiftoncarry		; normalize bahl to return result bcde

;-------------------------------------------------------------------------------
;
;		FLOATING POINT DIVISION
;
;		fdivx:	bcde / bcde' -> bcde
;		fdivy:	bcde' / bcde -> bcde
;			cf set on overflow or when dividing by zero
;			a,b,c,d,e,h,l,a',b',c',d',e',h',l' modified
;
;-------------------------------------------------------------------------------

fdivx:		exx			;
fdivy:		EXP			;
		scf			; set cf
		ret z			; if divisor bcde is zero then division by zero error (cf set)
		sub bias		; subtract exponent bias
		ld h,a			; exponent - bias -> h'
		exx			; activate bcdehl'
		EXP			;
		jr z,fzero		; if dividend bcde' is zero then return zero
		sub bias		; subtract exponent bias
		exx			; activate bcdehl

		; subtract unbiased exponents to produce result exponent

		sub h			; (exponent' - bias) - (exponent - bias) -> a
		jp po,1$		; if under/overflow then
		add a			;   cf = 1 if bit 7 set
		jr nc,underflow		;   if incorrectly positive then underflow
		ret			;   return with overflow error (cf set)

		; save biased result exponent to b' and result sign to l'

1$:		add bias		; bias the result exponent
		jr z,underflow		; if result exponent = 0 then underflow
		ex af,af'		; save a with result exponent
		ld a,b			; b -> a' with sign bit 7
		exx			; activate bcdehl'
		xor b			; sign xor sign' -> a' with result sign bit 7
		ex af,af'		; restore a with result exponent, save a' with result sign
		ld b,a			; a -> b' with result exponent
		set 7,c			; set bit 7 of man2' c'
		exx			; activate bcdehl
		set 7,c			; set bit 7 of man2 c

		; divide mantissas cde' / cde -> chl'

		xor a			;
		ld h,a			;
		ld l,a			;
		sbc hl,de		;
		ex de,hl		;
		sbc c			;
		ld c,a			; -cde -> cde we use -cde to add in the loop below
		exx			;
		ld a,c			; c' -> a
		push de			; save de'
		exx			;
		pop hl			; restore de' -> hl
		ld b,24			; 24 -> b loop counter
2$:		add hl,de	; 11	; loop
		adc c		;  4	;   ahl + -cde -> ahl
		jr c,3$		; 12/7	;   if cf = 0 then
		sbc hl,de	;   15	;
		sbc c		;    4	;     ahl - -cde -> ahl undo add, no carry
3$:		exx		;  4	;
		adc hl,hl	; 15	;
		rl c		;  8	;   chl' << 1 + cf -> chl' shift in carry
		exx		;  4	;
		add hl,hl	; 11	;
		rla		;  4	;   ahl << 1 -> ahl where rla carry means cf.ahl > cde
		jr c,6$		;  7/12	;   if cf = 1 then force add, shift carry, and loop again
		djnz 2$		; 13(107); until --b = 0

		; normalize result mantissa chl'

4$:		exx			; activate bcdehl'
		bit 7,c			; test c' bit 7
		jr nz,5$		; if zero then
		dec b			;   decrement result exponent b'
		exx			;
		jp z,underflow		;   if result exponent = 0 then underflow
		inc b			;   1 -> b loop counter
		jr nc,2$		;   loop without rla carry for final mantissa bit
		jr 3$			;   loop with rla carry for final mantissa bit

		; finalize bchl' to return bcde using sign a' bit 7

5$:		exx			; activate bcdehl
		ex af,af'		; restore a' with result sign
		ld l,a			; a -> l with result sign bit 7
		exx			; now make bchl' with sign l the active bchl with sign l'
		ex de,hl		; hl -> de
		ld a,c			; c -> a
		jp finalize		; finalize bade to return result bcde

		; when cf.ahl > cde then add -cde and shift a one into chl'

6$:		add hl,de	; 11	;
		adc c		;  4	;   ahl + -cde -> ahl
		scf		;  4	;   1 -> cf
		djnz 3$		; 13	; until --b = 0
		jr 4$			; normalize result mantissa chl'

;-------------------------------------------------------------------------------
;
;		CONVERT FLOAT TO INTEGER
;
;		ftoi:	int(bcde) -> signed integer bcde truncated towards zero
;			cf set when out of range (bcde unchanged)
;			a,b,c,d,e,h,l modified
;
;-------------------------------------------------------------------------------

ftoi:		EXP			;
		jp z,fzero		; if bcde is zero then return zero
		sub bias		; subtract exponent bias
		jp c,fzero		; if exponent is negative then return zero
		ld l,b			; save sign bit 7 to l
		ld h,a			; unbiased exponent -> h
		ld a,30			;
		sub h			; 30 - unbiased exponent -> a
		ret c			; if cf = 1 then out of range (cf set)
		set 7,c			; set bit 7 of man2 c

		; shift mantissa to remove fractional part

		inc a			;
		ld b,a			; loop counter = 31 - unbiased exponent
		xor a			; 0 -> a low order byte of the integer result
1$:		srl c			; loop
		rr d			;
		rr e			;
		rra			;   cdea >> 1 -> cdea
		djnz 1$			; until --b = 0

		; rearrange cdea to the result integer bcde

2$:		ld b,c			;
		ld c,d			;
		ld d,e			;
		ld e,a			;

		; return positive integer bcde if float is positive

3$:		rl l			; test l bit 7
		ret nc			; return positive bcde (cf reset)

inegate:	; negate integer bcde

		xor a			;
		sub e			;
		ld e,a			;
		ld a,0			;
		sbc d			;
		ld d,a			;
		ld a,0			;
		sbc c			;
		ld c,a			;
		sbc a			;
		sub b			;
		ld b,a			;
		or a			; reset cf
		ret			; return negated integer bcde (cf reset)

;-------------------------------------------------------------------------------
;
;		CONVERT INTEGER TO FLOAT
;
;		itof:	float(bcde) -> bcde
;			no errors (cf reset)
;			a,b,c,d,e,h,l modified
;
;-------------------------------------------------------------------------------

itof:		ld a,b			;
		or c			;
		or d			;
		or e			;
		ret z			; return if bcde is zero (cf reset)

		; save sign to l and negate integer bcde when negative

		ld l,b			; save sign b bit 7 to l
		bit 7,b			; if sign is negative then
		call nz,inegate		;   negate integer bcde

		; rearrange nonzero integer bcde to mantissa cde.a

		ld a,e			;
		ld e,d			;
		ld d,c			;
		ld c,b			;

		; set exponent b and normalize nonzero mantissa cde.a

		ld b,bias + 31		; set exponent b
		bit 7,c			;
		jr nz,2$		; if c bit 7 not set then
1$:		dec b			;   loop, decrement exponent b
		add a			;
		rl e			;
		rl d			;
		rl c			;     cde.a << 1 -> cde.a
		jp p,1$			;   until c bit 7 set

		; finalize bcde with sign l bit 7
		
2$:		rl c			; shift left man2 c to assign man2 bit 7 below
		rl l			; set cf to the sign l bit 7
		rr b			; rotate right result exponent b and assign sign bit
		rr c			; rotate right result man2 c and assign exponent bit
		or a			; reset cf
		ret			; return float bcde (cf reset)

;-------------------------------------------------------------------------------
;
;		FLOATING POINT MULTIPLICATION BY POWER OF 10
;
;		fpow10:	10**a * bcde -> bcde for -128 <= a < 39
;			cf set on overflow
;			a,b,c,d,e,h,l,a',b',c',d',e',h',l' modified
;
;-------------------------------------------------------------------------------

fpow10:		or a			; test a and reset cf
		ret z			; if a = 0 then return float bcde unchanged (cf reset)
		exx			;
		jp p,1$			; if a > 0 then multiply by 10th power
		neg			; -a -> a
		call pow10		; 10**a -> bcde'
		jp c,underflow		; if 10**a overflows then return underflow
		jp fdivy		; bcde' / bcde -> bcde
1$:		call pow10		; 10**a -> bcde'
		ret c			; if 10**a overflows then return error (cf set)
		jp fmul			; 10*a * bcde -> bcde

pow10:		; return float bcde = 10**a for 0 < a < 39

		cp 39			;
		ccf			;
		ret c			; if a > 38 return error (cf set)
		ld hl,powers - 4	; power - 4 -> hl minus 4 since indexed from 1
		add a			;
		add a			;
		ld c,a			;
		ld b,0			;
		add hl,bc		; hl + 4 * a -> hl
		ld c,(hl)		;
		inc hl			;
		ld b,(hl)		;
		inc hl			;
		ld e,(hl)		;
		inc hl			;
		ld d,(hl)		; [hl] -> bcde
		ret			; return (cf reset)

powers:		; table of powers of 10

		.dw 0x4120,0x0000	; = 10**1
		.dw 0x42c8,0x0000	; = 10**2
		.dw 0x447a,0x0000	; = 10**3
		.dw 0x461c,0x4000	; = 10**4
		.dw 0x47c3,0x5000	; = 10**5
		.dw 0x4974,0x2400	; = 10**6
		.dw 0x4b18,0x9680	; = 10**7
		.dw 0x4cbe,0xbc20	; = 10**8
		.dw 0x4e6e,0x6b28	; = 10**9
		.dw 0x5015,0x02f9	; = 10**10
		.dw 0x51ba,0x43b7	; = 10**11
		.dw 0x5368,0xd4a5	; = 10**12
		.dw 0x5511,0x84e7	; = 10**13
		.dw 0x56b5,0xe621	; = 10**14
		.dw 0x5863,0x5fa9	; = 10**15
		.dw 0x5a0e,0x1bca	; = 10**16
		.dw 0x5bb1,0xa2bc	; = 10**17
		.dw 0x5d5e,0x0b6b	; = 10**18
		.dw 0x5f0a,0xc723	; = 10**19
		.dw 0x60ad,0x78ec	; = 10**20
		.dw 0x6258,0xd727	; = 10**21
		.dw 0x6407,0x8678	; = 10**22
		.dw 0x65a9,0x6816	; = 10**23
		.dw 0x6753,0xc21c	; = 10**24
		.dw 0x6904,0x5951	; = 10**25
		.dw 0x6aa5,0x6fa6	; = 10**26
		.dw 0x6c4e,0xcb8f	; = 10**27
		.dw 0x6e01,0x3f39	; = 10**28
		.dw 0x6fa1,0x8f08	; = 10**29
		.dw 0x7149,0xf2ca	; = 10**30
		.dw 0x72fc,0x6f7c	; = 10**31
		.dw 0x749d,0xc5ae	; = 10**32
		.dw 0x7645,0x3719	; = 10**33
		.dw 0x77f6,0x84df	; = 10**34
		.dw 0x799a,0x130c	; = 10**35
		.dw 0x7b40,0x97ce	; = 10**36
		.dw 0x7cf0,0xbdc2	; = 10**37
		.dw 0x7e96,0x7699	; = 10**38

;-------------------------------------------------------------------------------
;
;		CONVERT STRING TO FLOAT
;
;		stof:	[hl..hl+a-1] -> bcde
;			cf set on parsing error and hl points after the char
;			a,b,c,d,e,h,l,a',b',c',d',e',h',l' modified
;			b   remaining string length counter
;			c   sign (bit 7), dp flag (bit 6), digit count (biased)
;			d   decimal exponent sign flag 0 or 1
;			e   (signed) decimal exponent
;			hl  string pointer
;			bc' unscaled float result
;			de' unscaled float result & mantissa accumulator
;			hl' mantissa accumulator
;
;-------------------------------------------------------------------------------

stof:		ld b,a			; a -> b string length
		cp 1			;
		ret c			; if b = 0 then return error (cf set)
		cp 32			;
		ccf			;
		ret c			; if b > 31 then return error (cf set)

		; initialize

		ld c,32			; 32 -> c sign (bit 7), no dp (bit 6), biased digit counter (bits 0 to 5)
		exx			;
		ld de,0			;
		ld h,d			;
		ld l,e			; 0 -> dehl' clear mantissa accumulator
		ld c,e			; 0 -> c no digit parsed yet
		exx			; so that we can check if any digits were parsed (no if subnormal)

		; begin parsing

		ld a,(hl)		;
		inc hl			; [hl++] -> a next char

		; check for a leading minus sign

		cp '-			;
		jr nz,1$		; if char is a '- then
		set 7,c			;   set sign c bit 7 (negative)
		jr 2$			;   jump into parsing the next character

		; check for a leading plus sign

1$:		cp '+			;
		jr nz,compare_char	;
2$:		dec b			; decrement length and check if zero
		scf			;
		ret z			; if remaining length b = 0 then error (cf set)

parse_float:	; loop to parse float

		ld a,(hl)		;
		inc hl			; [hl++] -> a next char

compare_char:	; compare next character in a

		cp '.			;
		jr nz,1$		; if char is '. then
		bit 6,c			;
		scf			;
		ret nz			;   if c bit 6 is set then return error (cf set)
		set 6,c			;   set c bit 6 to mark dp parsed
		jr parse_next		;   parse next char

		; check for the E and e signs

1$:		cp 'E			;
		jr z,parse_exponent	; if char is 'E then parse exponent
		cp 'e			;
		jr z,parse_exponent	; if char is 'e then parse exponent

		; check for a decimal digit

		sub '0			; convert a to digit
		ret c			; if digit < 0 then return not a number (cf set)
		cp '9-'0+1		;
		ccf			;
		ret c			; if digit > 9 then return not a number (cf set)

		; parse digit

		bit 6,c			; test bit 6 marker when placed after dp
		jr z,2$			; if digit placed after dp then
		inc c			;   increment c digit counter

		; multiply mantissa accumulator dehl' by 10

2$:		exx			;
		ex af,af'		; save a
		ld a,d			; d' -> a
		and 0xf0		;
		jr z,3$			; if dehl' upper nibble is nonzero then
		exx			;
		dec c			;   decrement c digit counter
		jr parse_next		; else
3$:		add hl,hl	; 11	;
		rl e		;  4	;
		rl d		;  8	;
		push de		; 11	;
		ld b,h		;  4	;
		ld c,l		;  4	;
		ld a,e		;  4	;
		add hl,hl	; 11	;
		adc a		;  4	;
		rl d		;  8	;
		add hl,hl	; 11	;
		adc a		;  4	;
		rl d		;  8	;
		add hl,bc	; 11	;
		pop bc		; 10	;
		adc c		;  4	;
		ld e,a		;  4	;
		ld a,d		;  4	;
		adc b		;  4	;
		ld d,a		;  4	;   10 * dehl' -> dehl'

		; add decimal digit to mantissa accumulator dehl'

		ex af,af'		;   restore a with decimal digit
		add l			;
		ld l,a			;   dehl' + a -> dehl'
		ld c,1			;   1 -> c' mark that a digit was parsed
		exx			;

parse_next:	; parse next char

		djnz parse_float	; until --b = 0 keep parsing if remaining length <> 0
		ld e,b			; 0 -> e no decimal exponent part

parse_done:	; parsing done

		exx			;
		dec c			; decrement c' to test if any digits were parsed
		scf			; set cf
		ret nz			; if no digit parsed then return error (cf set)
		ld a,d			;
		or e			;
		or h			;
		or l			;
		jp z,fzero		; if dehl' = 0 then return zero

		; normalize dehl' into mantissa cde' with corresponding biased exponent b'

		ld b,bias+31		; initialize exponent b
		ld a,d			; d' -> a
		or a			;
		jp m,2$			; if d' bit 7 is zero then
1$:		dec b		;  4	;   decrement biased exponent b' (cannot underflow)
		add hl,hl	; 11	;
		rl e		;  8	;
		adc a		;  4	;   aehl' << 1 -> aehl'
		jp p,1$		; 10(37);
2$:		ld c,a			;
		ld d,e			;
		ld e,h			; save result mantissa aeh' -> cde'
		exx			;

		; adjust decimal exponent e down by c (biased) places after dp

		ld a,32			; biased decimal exponent 0
		add e			; e + 32 -> a biased parsed decimal exponent or 0
		res 6,c			; clear c bit 6 marker
		sub c			; e - c -> a (cannot overflow since ranges of e and c are restricted)
		xor c			;
		and 0x7f		; clear sign bit 7
		xor c			; a & 0x7f | c bit 7 -> a sign bit 7 and 7 bit signed 10th power

		; finalize bcde' with sign a bit 7 and scale by 10**a with a bits 0 to 6

		exx			; activate bcde' as bcde
		rl c			; c << 1 -> c clear bit 7
		rla			; a << 1 -> a sign bit to cf
		rr b			; set sign b bit 7 with a bit 7
		rr c			; cf + c >> 1 -> c pick up lsb exponent bit
		sra a			; a >> 1 -> a (arithmetic)
		jp fpow10		; scale bcde by 10**a

parse_exponent:	; parse the decimal exponent part into e with sign d

		ld de,0			; clear decimal exponent, sign in d and value in e

		; check for and get next char

		dec b			; decrement remaining length b
		scf			; set cf
		ret z			; if b = 0 then return error (cf set)
		ld a,(hl)		;
		inc hl			; [hl++] -> a next char

		; check for decimal exponent minus sign

		cp '-			;
		jr nz,1$		; if char is '- then
		inc d			;   1 -> d marks negative decimal exponent
		jr 2$			;   get next char

		; check for decimal exponent plus sign

1$:		cp '+			;
		jr nz,4$		; if char is not '+ then jump into loop to parse digit

		; check and get next char after +/- sign

2$:		dec b			; decrement remaining length b
		scf			;
		ret z			; if b = 0 then return error (cf set)

		; loop to parse and accumulate decimal digits of the decimal exponent

3$:		ld a,(hl)		; loop
		inc hl			;   [hl++] -> a next char
		ex af,af'		;   save char
		ld a,e			;
		add a			;
		ld e,a			;
		add a			;
		add a			;
		add e			;
		ld e,a			;   10 * e -> e
		ex af,af'		;   restore char

		; convert digit and add to decimal exponent

4$:		sub '0			;   convert a to digit
		ret c			;   if digit < 0 then return error (cf set)
		cp '9-'0+1		;
		ccf			;
		ret c			;   if digit > 9 then return error (cf set)
		add e			;
		ld e,a			;   e + digit -> e with decimal exponent
		cp 64			;
		ccf			;
		ret c			;   if decimal exponent > 63 then return error (cf set)
		djnz 3$			; until --b = 0 keep parsing

		; apply sign to the decimal exponent and finalize

		dec d			; test if d is set (negative exponent)
		jr nz,parse_done	; if exponent sign is plus then return bcde' scaled by 10**a
		xor a			;
		sub e			;
		ld e,a			; negate e with decimal exponent
		jr parse_done		; return bcde' scaled by 10**a

;-------------------------------------------------------------------------------
;
;		CONVERT FLOAT TO STRING
;
;		ftos:	bcde -> [hl...hl+a-1] digits, exponent e and sign d bit 7
;			no errors (flags undefined)
;			a,b,c,d,e,h,l,a',b',c',d',e',h',l' modified
;			 a  nonzero buffer size
;			bc  float argument
;			de  float argument
;			 d  sign bit 7 result
;			 e  signed decimal exponent result
;			hl  buffer pointer
;			bc' mantissa workspace
;			de' mantissa workspace
;			hl' mantissa workspace
;
;-------------------------------------------------------------------------------

ftos:		push hl			; save buffer address hl
		push af			; save nonzero buffer size a

		; estimate decimal exponent = exp*77/256 ~= log10(2**exp) = exp * log10(2)

		EXP			;
		jr z,3$			; if bcde = 0 then populate buffer with 0s
		sub bias + 1		; exponent - bias - 1 -> a minus 1 to prevent overflow
		exx			;
		ld e,a			;
		rla			;
		sbc a			;
		ld d,a			; sign extend a -> de -> hl
		ld h,d			;
		ld l,e			; 1
		add hl,hl		; 0
		add hl,hl		; 0
		add hl,hl		; \1
		add hl,de		; /
		add hl,hl		; \1
		add hl,de		; /
		add hl,hl		; 0
		add hl,hl		; \1 = %1001101 = $4d = 77
		add hl,de		; /
		ld de,77 + 82		; 82 empirically determined optimal to prevent underestimation
		add hl,de		;
		ld a,h			; ((exponent - bias - 1) * 77 + 77 + 82) / 256 -> a
		cp 39			; correct overshoot 39 to 38
		jr nz,1$		;
		dec a			;
1$:		ld e,a			; a -> e decimal exponent
		push de			; save decimal exponent e
		exx			;

		; multiply bcde by 10**-a

		neg			;
		call fpow10		; bcde * 10**a -> bcde

		; sign and decimal exponent

		pop hl			; restore decimal exponent l
		ld h,b			; b -> h sign bit 7

		; init d.ehl' with mantissa cde using unbiased exponent 0 <= b <= 3 to shift

		pop af			; restore buffer size a
		ex (sp),hl		; restore buffer address hl, save decimal exponent l and sign h bit 7
		push de			;
		push bc			; save bcde
		ld b,a			; buffer size b
		ld c,a			; save buffer size copy c
		exx			;
		pop bc			;
		pop hl			; restore bcde -> bchl'
		EXP			; exponent -> a
		ld d,0			; 0 -> d' zero leading digit
		ld e,c			; c' -> e'
		set 7,e			; set e' bit 7
		sub bias		; exponent - bias -> a
		jr c,5$			; if exponent - bias < 0 then leading digit is zero
		ld b,a			;
		inc b			; exponent - bias + 1 -> loop counter b'
		ld a,d			; 0 -> a leading zero digit
2$:		add hl,hl	; 11	; loop
		rl e		;  8	;
		adc a		;  4	;   a.ehl' << 1 -> a.ehl'
		djnz 2$		; 13(36); until --b' = 0
		jr 7$			;

		; return buffer with 0s

3$:		pop bc			; pop buffer size b
		pop hl			; pop buffer address hl
4$:		ld (hl),'0		; loop
		inc hl			;   '0 -> [hl++]
		djnz 4$			; until --b = 0
		ld d,b			; 0 -> d positive sign
		ld e,b			; 0 -> e zero decimal exponent
		ret			; return

		; leading digit is zero, shift it away

5$:		exx			;
		pop de			;
		dec e			; decrement the saved decimal exponent e
		push de			;

		; loop over the buffer to populate

6$:		exx			; loop

		; 10 * d.ehl' -> a.ehl' with d' = 0, decimal digit a and fraction ehl'

		add hl,hl	; 11	;
		rl e		;  4	;
		rl d		;  8	;
		push de		; 11	;
		ld b,h		;  4	;
		ld c,l		;  4	;
		ld a,e		;  4	;
		add hl,hl	; 11	;
		adc a		;  4	;
		rl d		;  8	;
		add hl,hl	; 11	;
		adc a		;  4	;
		rl d		;  8	;
		add hl,bc	; 11	;
		pop bc		; 10	;
		adc c		;  4	;
		ld e,a		;  4	;
		ld a,d		;  4	;
		adc b		;  4	;   10 * d.ehl' -> aehl'

		; digit is now in a, clear digit d in d.ehl'

7$:		ld d,0			;   0 -> d' clear decimal digit

		; store digit d in the buffer

		exx			;
		add '0			;   convert digit to ascii
		ld (hl),a		;
		inc hl			;   digit -> [hl++]
		djnz 6$			; until --b = 0

		; round up decimals if the remaining fraction in ehl' is >= .5

		exx			;
		bit 7,e			; test e' bit 7
		exx			;
		pop de			; restore decimal exponent e and sign d
		ret z			; if remaining fraction >= 0.5 then return (cf reset)
		ld b,c			; c -> b restore buffer size b
8$:		dec hl			; loop
		ld a,(hl)		;
		inc a			;   [--hl] + 1 -> a ascii decimal digit
		ld (hl),a		;   a -> [hl] increment ascii decimal digit
		cp '9+1			;
		ret c			;   if digit <= '9 then return
		ld (hl),'0		;   '0 -> [hl]
		djnz 8$			; until --b = 0
		ld (hl),'1		; '1 -> [hl] make it a leading 1, rest all 0s
		inc e			; increment decimal exponent e
		ret			; return
