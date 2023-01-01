;-------------------------------------------------------------------------------
;
;	Z80 IEEE 754 FLOATING POINT WITH ROUNDING MODES, INF/NAN AND SIGNED ZERO
;
; Author:
;   Dr. Robert van Engelen, Copyright 2022
;
; Features:
;   - IEEE 754 single precision floating point:
;     addition, subtraction, multiplication, division, negation, absolute,
;     truncation, flooring, rounding, integer to/from float conversion,
;     string to/from float conversion
;   - choice of three IEEE 754 rounding modes: round to nearest - ties to even,
;     round to nearest - ties to away, and round to zero (truncate)
;   - "memoryless" using registers only (+shadow), at most one push+pop per flop
;   - optimized for speed and reduced code size (no loop unrolling)
;   - inf/nan with cf set when a function returns an inf or nan
;   - signed zero
;   - no subnormals
;   - extensively tested
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
;		exponent bias:	127 (0x7f)
;		exponent zero:	indicates zero floating point value
;		exponent max:	255 (0xff) indicates inf or nan
;		mantissa bits:	24 (including implicit msb of 1)
;		range:		2^-126 to 2^+127
;
;		examples:
;
;		   0 = 00 00 00 00
;		   1 = 3f 80 00 00
;		   2 = 40 00 00 00
;		   3 = 40 40 00 00
;		  -0 = 80 00 00 00
;		  -1 = bf 80 00 00
;		  -2 = c0 00 00 00
;		  -3 = c0 40 00 00
;		 inf = 7f 80 00 00
;		-inf = ff 80 00 00
;		 nan = s 11111111 xxxxxxx xxxxxxxx xxxxxxxx at least one x is 1
;
;		IEEE 754 binary floating point allows floating point values to
;		be compared as if comparing 32 bit signed integers with 'i<':
;
;		  zero:   +0 = 00 00 00 00
;		  zero:   -0 = 80 00 00 00 (negative zero)
;		For positive opposite signs:
;		 1 < 2:    1 = 3f 80 00 00 i< 2   = 40 00 00 00
;		-1 < 2:   -1 = bf 80 00 00 i< 2   = 40 00 00 00
;		-1 < inf: -1 = bf 80 00 00 i< inf = 7f 80 00 00
;		When both signs are negative the comparison is inverted:
;		-2   < -1: -2   = c0 00 00 00 i> -1 = bf 80 00 00
;		-inf < -1: -inf = ff 80 00 00 i> -1 = bf 80 00 00
;		Comparing nan always returns false (nan is incomparable)
;
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;
;		CONFIGURATION
;
;-------------------------------------------------------------------------------

; Round to nearest, ties away (1) ties to even (2) and truncate (0) modes

SUMROUND = 2	; fadd and fsub rounding mode (and itof and atof excess digits)
MULROUND = 2	; fmul rounding mode (and fpow10 and atof)
DIVROUND = 2	; fdiv rounding mode (and fpow10 and atof)

ROUND = SUMROUND+MULROUND+DIVROUND	; nonzero if rounding is requested

;-------------------------------------------------------------------------------
;
;		CONSTANTS
;
;-------------------------------------------------------------------------------

bias		.equ 127		; exponent bias 127 IEEE 754

;-------------------------------------------------------------------------------
;
;		ASSEMBLY MACROS
;
;-------------------------------------------------------------------------------

; Extract the exponent of float bcde -> a

.macro		EXPA
		ld a,c			;
		add a			; set cf to lowest order exponent bit
		ld a,b			;
		adc a			; exponent -> a, set z if zero, set cf if negative
.endm

; Extract the exponent of float bcde -> h, modifies l with c << 1 -> l

.macro		EXPH
		ld h,b			;
		ld l,c			;
		add hl,hl		; exponent -> h, set cf if negative
.endm

; Set z if float bcde is not numeric (i.e. inf or nan), modifies hl

.macro		ISNN
		EXPH			;
		inc h			; set z if float is inf or nan, set cf if negative
.endm

; Test if float bcde is zero, modifies a

.macro		ISZERO
		EXPA			; set z if float is zero (positive or negative zero)
.endm

; Test if float bcde is negative or zero, modifies a

.macro		ISNEGZ
		EXPA			; set z if float is zero, set cf if negative
.endm

; Test if float bcde is positive or zero

.macro		ISPOSZ
		bit 7,b			; set z if float is positive or zero
.endm

;-------------------------------------------------------------------------------
;
;		FLOATING POINT TYPE CHECK
;
;		ftype:	test bcde for inf and nan, bcde unchanged
;			cf set if bcde is nan (z undefined)
;			z set if bcde is +/-inf (cf reset)
;			a,hl modified
;
;		usage:	call ftest
;			jr c,bcde_is_nan
;			jr z,bcde_is_inf
;
;-------------------------------------------------------------------------------

ftype:		EXPH			; exponent -> h, c << 1 -> l
		xor a			; 0 -> a, reset cf
		inc h			; exponent + 1 -> h
		ret nz			; if not inf/nan then return cf reset and z reset
		sub l			; if l <> 0 then set cf
		ld l,h			; 0 -> l
		sbc hl,de		; hl - de - cf -> cf.hl
		ret			; return cf set if nan, cf reset and z set if inf

;-------------------------------------------------------------------------------
;
;		FLOATING POINT NEGATION
;
;		fneg:	-bcde -> bcde
;			no errors (cf reset)
;			a,b modified
;
;-------------------------------------------------------------------------------

fneg:		ld a,b			; sign bit 7 and exponent
		xor 0x80		; invert sign bit 7
		ld b,a			; set new sign bit 7
		ret			; return float bcde (cf reset)

;-------------------------------------------------------------------------------
;
;		FLOATING POINT ABSOLUTE VALUE
;
;		fabs:	|bcde| -> bcde
;			no errors (cf reset)
;			b modified
;
;-------------------------------------------------------------------------------

fabs:		res 7,b			; reset sign b bit 7
		or a			; reset cf
		ret			;

;-------------------------------------------------------------------------------
;
;		FLOATING POINT SUBTRACTION
;
;		fsubx:	bcde - bcde' -> bcde
;		fsuby:	bcde' - bcde -> bcde
;			cf set if result float bcde is inf or nan
;			a,b,c,d,e,h,l,a',b',c',d',e',h',l' modified
;
;-------------------------------------------------------------------------------

fsubx:		exx			; swap bcde with bcde'
fsuby:		call fneg		; -bcde -> bcde
		; FALL THROUGH		; -bcde + bcde' -> bcde

;-------------------------------------------------------------------------------
;
;		FLOATING POINT ADDITION
;
;		fadd:	bcde + bcde' -> bcde
;			cf set if result float bcde is inf or nan
;			a,b,c,d,e,h,l,a',b',c',d',e',h',l' modified
;
;		0+n -> n for any n
;		inf+inf -> inf
;		inf-inf -> nan
;		nan+n -> nan for any n
;
;-------------------------------------------------------------------------------

fadd:		EXPA			;
		ld h,a			; exponent -> h
		exx			; activate bcdehl'
		jr z,addzero		; if bcde is zero then return float bcde'
		EXPA			;
		ld h,a			; exponent' -> h'
		exx			; activate bcdehl
		jr z,addzero		; if bcde' is zero then return float bcde

		; compare the exponents

		sub h			; exponent' - exponent -> a
		jr nc,1$		; if exponent' < exponent then
		neg			;   exponent - exponent' -> a
		exx			;   swap bcdehl with bcdehl' (commutative symmetry)
1$:		exx			; activate bcdehl' (symmetry allows ignoring the swap above)
.if SUMROUND
		cp 25			; (SR=1|2) if exponent' - exponent >= 25 then
.else
		cp 24			; (SR=0) if exponent' - exponent >= 24 then
.endif;SUMROUND
		jr nc,addzero		;   return float bcde'

		; save signs

		ld l,b			; save sign' b' bit 7 to l'
		exx			; activate bcdehl
		ld l,b			; save sign b bit 7 to l

		; align mantissa cde to cde' and set result exponent b = exponent'

		or a			;
		jr z,aligned		; if exponent' = exponent then add or subtract the aligned mantissas

		; shift cde' left by one bit to remove implicit msb and clear lsb for rounding

		exx			;
		sla e			;
		rl d			;
		rl c			; cde' << 1 -> 1.cde'
		exx			;

		; shift cde right by exponent' - exponent - 1 bits to realign mantissas

		set 7,c			; set c bit 7 of man2
		ld b,a			; loop counter exponent' - exponent -> b
		add h			;
		ld h,a			; exponent' - exponent + exponent -> h = result exponent'
.if SUMROUND
.if SUMROUND - 1
		xor a			; (SR=2) 0 -> a mask to collect the sticky bits
.endif
.endif
		jr 3$			;
2$:		
.if SUMROUND
.if SUMROUND - 1
		or e		;  4	;   (SR=2) a | e -> a collect the sticky bits in a bit 0
.endif
.endif
		srl c		;  8	; loop exponent' - exponent - 1 times
		rr d		;  8	;
		rr e		;  8	;   cde >> 1 -> cde.cf
3$:		djnz 2$		; 13(37/41); until --b = 0
		ld b,h			; result exponent -> b
.if SUMROUND
.if SUMROUND - 1
		and 1			; (SR=2) set z if all sticky bits are zero
		ex af,af'		; (SR=2) save a and z, z is set when all sticky bits are zero
.endif
.endif

		; compare signs

		ld a,l			; sign -> a bit 7
		exx			; activate bcdehl'
		xor l			; sign xor sign' -> a bit 7
		ld a,c			; c' -> a
		push de			; push de'
		exx			; activate bcdehl
		pop hl			; pop hl with de'
		jp m,subtract		; if signs differ then subtract aligned mantissas

add:		; add realigned mantissa 0.cde to 1.cde' to produce result mantissa ahl

		add hl,de		;
		adc c			; 1.cde' + 0.cde -> 1+cf.ahl with result mantissa, cf is inverted
		ccf			; complement cf
		jr c,shiftrightmant	; if carry then shift right result mantissa cf.ahl
.if SUMROUND
.if SUMROUND - 1
		ex af,af'		; (SR=2) restore a, save cf = 0
		or l			; (SR=2) a | l -> a collect the sticky bits in a bit 0
		and 1			; (SR=2)
		ex af,af'		; (SR=2) save a and z, z is set when all sticky bits are zero, restore cf = 0
.endif
.endif
		rra			;
		rr h			;
		rr l			; 10.ahl >> 1 -> 1.ahl shift right
		jr shiftright		; shift right again result mantissa 1.ahl and increment exponent b

addzero:	; adding zero, return the other operand and set cf when inf or nan

		EXPA			; exponent -> a
		add 1			; set cf when bcde is inf or nan
		ret			; return float bcde (cf set when inf or nan)

subtract:	; subtract realigned mantissa 0.cde from 1.cde' to produce result mantissa ahl

		sbc hl,de		;
		sbc c			; 1.cde' - 0.cde -> 1-cf.ahl with result mantissa, cf is inverted
		jr nc,shiftrightmant	; if no carry then shift right result mantissa 1.ahl
		dec b			; decrement exponent
		jr z,zerol		; if exponent is zero then return zero (underflow, cf reset)
		jr normalize		; normalize result mantissa 0.ahl to return inexact result bcde

aligned:	; check if bcde and bcde' are inf/nan

.if SUMROUND
.if SUMROUND - 1
		ex af,af'		; (SR=2) save z, z is set (all sticky bits are zero) when mantissas are aligned
.endif
.endif;SUMROUND
		ld b,h			; save h -> b result exponent
		inc h			;
		jr nz,1$		; if bcde and bcde' are inf/nan then
		ld a,c			;
		exx			;
		add c			;
		or d			;
		or e			;
		exx			;
		or d			;
		or e			;
		jr nz,fnan		;   if bcde or bcde' is nan then return nan
		ld a,l			;
		exx			;
		xor l			;
		jp m,fnan		;   if sign <> sign' then return nan
		jr infl			;   return inf with sign l'

1$:		; add or subtract aligned mantissas cde and cde' after comparing signs

		ld a,l			; sign' l bit 7 -> a
		exx			; activate bcdehl'
		ld b,h			; save h' -> b' result exponent' = b result exponent
		xor l			; sign' xor sign -> a, reset cf
		jp m,subaligned		; if signs differ then subtract aligned mantissas

addaligned:	; add aligned mantissa cde to cde' to produce result mantissa 1.ahl

		ld a,c			; c' -> a
		push de			; push de'
		exx			; activate bcdehl
		pop hl			; pop hl with de'
		add hl,de		;
		adc c			; cde' + cde -> 1.ahl with result mantissa

shiftright:	; shift right result mantissa 1.ahl and increment exponent b

		inc b			; increment result exponent
		jr z,infl		; if result exponent is zero then overflow (cf set)

shiftrightmant:	; shift right result mantissa 1.ahl

		scf			;
		rra			;
		rr h			;
		rr l			; cf.ahl >> 1 -> ahl.cf

finalizerounda:	; finalize bahl.cf with cf for rounding to return float bcde with sign' l' bit 7

.if SUMROUND
		ld c,a			; (SR=1|2) mantissa ahl -> chl

finalizeroundc:	; finalize bchl.cf with cf for rounding to return float bcde with sign' l' bit 7

.if SUMROUND - 1
		jr nc,finalizec		; (SR=2) if carry then
		ex af,af'		; (SR=2)   check the sticky bits z
		call nz,roundtoaway	; (SR=2)   either round to nearest ties to away mantissa chl.1 (z reset afterwards)
		call z,roundtoeven	; (SR=2)   or round to nearest ties to even mantissa chl.1
.else
		call c,roundtoaway	; (SR=1) if carry then round result mantissa chl.cf
.endif

finalizec:	; finalize bchl to return float bcde with sign' l' bit 7

		ld a,c			; (SR=1|2) mantissa chl -> ahl
.endif;SUMROUND

finalizea:	; finalize bahl to return float bcde with sign' l' bit 7

.if bias - 128	; check overflow when bias = 127, for bias = 128 the result exponent <= bias + 127 = 255
		inc b			; check if result exponent overflowed (+/- infinity)
		jr z,infl		; if result exponent overflowed then return error (cf set)
		dec b			;
.endif

		add a			; shift left man2 a to assign man2 bit 7 below
		exx			;
		rl l			; set cf to the sign' l' bit 7
		exx			;
		rr b			; rotate right result exponent b and assign sign' bit
		rra			; rotate right result man2 a and assign exponent bit
		ld c,a			;
		ex de,hl		; ahl -> cde set result mantissa
		xor a			; 0 -> a, reset cf, reset v, set z
		ret			; return float bcde (cf reset)

.if ROUND	; round bchl.1 when the round bit is set

roundtoeven:	bit 0,l			; entry point to round to nearest ties to even
		ret z			; if guard bit is zero then return
roundtoaway:	inc l			; entry point to round to nearest ties to away (return with z reset)
		ret nz			; if okay then return (z reset)
		inc h			;
		ret nz			; if okay then return (z reset)
		inc c			;
		ret nz			; if okay then return (z reset)
		inc b			;
		ret nz			; if okay then return (z reset), otherwise biased exponent > 255
		pop af			; pop and discard return address
		jr infl			; return +/-inf
.endif;ROUND

resubtract:	; redo subtract mantissa cde' from cde after swap to produce result mantissa ahl

		exx			;
		sbc a			; 0xff -> a because cf is set
		sub l			; complement l -> a, reset cf
		exx			;
		ld l,a			; restore sign' l' bit 7 to the complement of sign l bit 7

subaligned:	; subtract aligned mantissa cde from cde' to produce result mantissa cf.ahl

		ld a,c			; c' -> a
		push de			; push de'
		exx			; activate bcdehl
		pop hl			; pop hl with de'
		sbc hl,de		; de' - de -> hl (cf was reset)
		sbc c			; c' - c -> a
		jr c,resubtract		; if cde' < cde then swap cde with cde' and redo subtract

normalize:	; normalize bahl to return inexact result bcde with sign' l' bit 7

		ld c,a			; save a -> c
		or h			;
		or l			;
		jr z,fzero		; if ahl = 0 then return zero (underflow, cf reset)
		ld a,c			; restore c -> a
		or a			;
1$:		jp m,finalizea	; 10	; loop while a bit 7 is clear
		add hl,hl	; 11	;
		adc a		;  4	;   ahl << 1 -> ahl
		djnz 1$		; 13(38); until --b = 0
		jr zerol		; return zero (underflow, cf reset)

;-------------------------------------------------------------------------------
;
;		FLOATING POINT CONSTANT ZERO
;
;		fzero:	0.0 -> bcde
;		fzeroa:	sign in a bit 7 + 0.0 -> bcde
;			cf reset
;			a,b,c,d,e modified
;
;-------------------------------------------------------------------------------

fzero:		xor a			; 0 -> a
		jr set_b_res_cde	; return zero (cf reset)

zerob:		; return zero with sign b xor b' bit 7 and cf reset

		ld a,b			; sign' -> a bit 7
		exx			; activate bcde = +/-0
		xor b			; sign' xor sign -> a bit 7
		.db 1			; skip 2 bytes to return zero with sign a bit 7 (cf reset)

zerol:		; return zero with result sign l' bit 7 and cf reset

		exx			;
		ld a,l			;

fzeroa:		; return zero with sign a bit 7 and cf reset

		and 0x80		;
set_b_res_cde:	ld b,a			;
res_cde:	xor a			; 0 -> a and reset cf
set_cde:	ld c,a			;
set_de:		ld d,a			;
		ld e,a			;
		ret			; return float bcde

;-------------------------------------------------------------------------------
;
;		FLOATING POINT QUIET NAN
;
;		fnan:	quiet nan 0x7fc00000 -> bcde
;			a,b,c,d,e modified
;
;-------------------------------------------------------------------------------

fnan:		ld bc,0x7fc0		;
		jr res_de		; return 0x7fc00000 quiet nan (cf set)

;-------------------------------------------------------------------------------
;
;		FLOATING POINT SIGNED INF
;
;		finfa:	sign in a bit 7 + inf 0x7f800000 -> bcde
;			cf set
;			a,b,c,d,e modified
;
;-------------------------------------------------------------------------------

infb:		; return inf with sign b xor b' bit 7 and cf set

		ld a,b			;
		exx			; activate bcde'
		xor b			; sign xor sign' -> a bit 7
		.db 1			; skip 2 bytes to return inf with sign a bit 7 (cf set)

infl:		; return inf with result sign l' bit 7 and cf set

		exx			;
		ld a,l			; l' -> a

finfa:		; return inf with sign a bit 7 and cf set

		or 0x7f			;
		ld b,a			;
		ld c,0x80		;
res_de:		xor a			;
		scf			; set cf
		jr set_de		; return 0x7f800000 inf with sign a bit 7 (cf set)

;-------------------------------------------------------------------------------
;
;		FLOATING POINT MULTIPLICATION
;
;		fmul:	bcde * bcde' -> bcde
;			cf set if result float bcde is inf or nan
;			a,b,c,d,e,h,l,a',b',c',d',e',h',l' modified
;
;		0*inf -> nan
;		n*inf -> inf for any n except 0, nan
;		n*nan -> nan for any n
;		0*n -> 0 for any n except inf, nan
;
;-------------------------------------------------------------------------------

fmul:		EXPA			; exponent -> a
		jr z,mulzero		; if bcde is zero then return signed zero or nan
		inc a			;
		jr z,mulinfnan		; if bcde is inf/nan then return inf or nan (cf set)
		sub bias+1		; subtract exponent bias + 1 to correct for inc a
		ld h,a			; exponent - bias -> h
		exx			; activate bcdehl'
		EXPA			; exponent' -> a
		jr z,mulzero		; if bcde' is zero then return signed zero or nan
		inc a			;
		jr z,infnan		; if bcde' is inf/nan then return inf or nan (cf set)
		sub bias+1		; subtract exponent bias + 1 to correct for inc a
		exx			; activate bcdehl

		; add unbiased exponents to produce result exponent

		add h			; (exponent' - bias) + (exponent - bias) -> a
		jp pe,outofrange	; if out of range then return zero (underflow, cf reset) or inf (overflow, cf set)

		; save biased result exponent and sign

		add bias		; bias the result exponent
		jr z,zerob		; if result exponent is zero then return zero (underflow, cf reset)
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
1$:		rra		;  4	; loop
		rr h		;  8	;
		rr l		;  8	;
		exx		;  4	;
		rr c		;  8	;
		rr d		;  8	;
		rr e		;  8	;   cf.ahl.cde' >> 1 -> ahl.cde'.cf
		exx		;  4	;
		jr nc,2$	; 12/7	;   if carry then
		add hl,de	;   11	;
		adc c		;    4	;     ahl + cde -> ahl
2$:		djnz 1$		; 13(87); until --b = 0

		; restore result exponent b

		ex af,af'		; restore result exponent a', save a and cf
		ld b,a			; a' -> b with result exponent
		ex af,af'		; restore a and cf

		; shift right on carry

.if MULROUND
.if MULROUND - 1
		jr nc,3$		; (MR=2) if carry then
		inc b			; (MR=2)   increment result exponent
		jr z,infl		; (MR=2)   if result exponent is zero then return inf (cf set)
		rra			; (MR=2)
		rr h			; (MR=2)
		rr l			; (MR=2)   cf.ahl >> 1 -> ahl.cf
		jr 4$			; (MR=2)   finalize and round bahl.cde' to return float bcde
.else
.if SUMROUND - MULROUND
		jr nc,3$		; (MR=1) if carry then
		inc b			; (MR=1)   increment result exponent
		jr z,infl		; (MR=1)   if result exponent is zero then return inf (cf set)
		rra			; (MR=1)
		rr h			; (MR=1)
		rr l			; (MR=1)   cf.ahl >> 1 -> ahl.cf
		jr 4$			; (MR=1)   finalize and round bahl.cde' to return float bcde
.else
		jp c,shiftright		; (MR=1) if carry then shift right mantissa 1.ahl, increment exponent, return float bcde
.endif
.endif
3$:		exx			; (MR=1|2)
		rl c			; (MR=1|2) c' << 1 + cf to produce cf to round result mantissa ahl
		exx			; (MR=1|2)
.else
		jp c,shiftright		; (MR=0) if carry then shift right mantissa 1.ahl, increment exponent, return float bcde
.endif;MULROUND

4$:		; finalize and round bahl.cde' to return float bcde

.if MULROUND
.if SUMROUND - MULROUND
.if MULROUND - 1
		ld c,a			; (MR=2) ahl -> chl set result mantissa
		jr nc,5$		; (MR=2) if carry then
		exx			; (MR=2)
		ld a,c			; (MR=2)
		or d			; (MR=2)
		or e			; (MR=2)   check if all sticky bits cde' are zero
		exx			; (MR=2)
		call nz,roundtoaway	; (MR=2)   either round to nearest ties to away mantissa chl.1 (z reset afterwards)
		call z,roundtoeven	; (MR=2)   or round to nearest ties to even mantissa chl.1
5$:		ld a,c			; (MR=2) chl -> ahl set result mantissa
		jp finalizea		; (MR=2) finalize bahl to return float bcde
.else
		ld c,a			; (MR=1) ahl -> chl set result mantissa
		call c,roundtoaway	; (MR=1) if carry then round result mantissa chl.cf
		jp finalizec		; (MR=1) finalize bchl to return float bcde
.endif
.else
.if MULROUND - 1
		ex af,af'		; (MR=2) save a and cf
		exx			; (MR=2)
		ld a,c			; (MR=2)
		or d			; (MR=2)
		or e			; (MR=2) check if all sticky bits cde' are zero
		exx			; (MR=2)
		ex af,af'		; (MR=2) restore a and cf, save z
.endif
		jp finalizerounda	; (MR=1|2) finalize and round bahl.cf to return float bcde
.endif
.else
		jp finalizea		; (MR=0) finalize bahl to return float bcde
.endif;MULROUND

outofrange:	; out of range, return zero (underflow, cf reset) or inf (overflow, cf set)

		add a			; carry if bit 7 set
		jp nc,zerob		; if incorrect positive then return signed zero (underflow, cf reset)
		jr infb			; return signed inf (overflow, cf set)

infnan:		; one nonzero operand of fmul and fdiv is inf/nan, return inf or nan (cf set)

		call ftype		; test bcde for nan
		ret c			; if bcde is nan then return nan (cf set)
		exx			; activate bcde'
		call ftype		; test bcde' for nan
		ret c			; if bcde' is nan then return nan (cf set)
		jr infb			; return signed inf (cf set)

mulinfnan:	; multiply by inf or nan, return inf or nan (cf set)

		call ftype		; test bcde for nan
		ret c			; if bcde is nan then return nan (cf set)
		jp divzero		; 

mulzero:	; multiply by zero, return zero (cf reset) or nan (cf set)

		exx			; activate bcde'
		ISNN			; test if bcde' is inf or nan
		jp z,fnan		; if bcde' is inf or nan then return nan (cf set)
		jp zerob		; return signed zero (cf reset)

;-------------------------------------------------------------------------------
;
;		FLOATING POINT DIVISION
;
;		fdivx:	bcde / bcde' -> bcde
;		fdivy:	bcde' / bcde -> bcde
;			cf set if result float bcde is inf or nan
;			a,b,c,d,e,h,l,a',b',c',d',e',h',l' modified
;
;		0/0 -> nan
;		n/0 -> inf for any n except 0, nan
;		0/n -> 0 for any n except 0, nan
;		inf/inf -> nan
;		n/inf -> 0 for any n except inf, nan
;		n/nan -> nan for any n
;		nan/n -> nan for any n
;
;-------------------------------------------------------------------------------

fdivx:		exx			;
fdivy:		EXPA			; exponent -> a
		jr z,divzero		; if divisor bcde is zero then division by zero
		inc a			;
		jr z,divinfnan		; if divisor bcde is inf/nan then division by inf/nan
		sub bias+1		; subtract exponent bias + 1 to correct for inc a
		ld h,a			; exponent - bias -> h'
		exx			; activate bcdehl'
		EXPA			; exponent' -> a
		jp z,zerob		; if dividend bcde' is zero then return signed zero
		inc a			;
		jr z,infnan		; if dividend bcde' is inf/nan then return inf or nan
		sub bias+1		; subtract exponent bias + 1 to correct for inc a
		exx			; activate bcdehl

		; subtract unbiased exponents to produce result exponent

		sub h			; (exponent' - bias) - (exponent - bias) -> a
		jp pe,outofrange	; if out of range then return zero (underflow, cf reset) or inf (overflow, cf set)

		; save biased result exponent to b' and result sign to l'

		add bias		; bias the result exponent
		jp z,zerob		; if result exponent is zero then return zero (underflow, cf reset)
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
1$:		add hl,de	; 11	; loop
		adc c		;  4	;   ahl + -cde -> cf.ahl
		jr c,2$		; 12/7	;   if no carry then
		sbc hl,de	;   15	;
		sbc c		;    4	;     ahl - -cde -> ahl undo add, no carry
2$:		exx		;  4	;
		adc hl,hl	; 15	;
		rl c		;  8	;   chl'.cf << 1 -> chl' shift in carry
		exx		;  4	;
		add hl,hl	; 11	;
		rla		;  4	;   ahl << 1 -> ahl where rla carry means cf.ahl > cde
		jr c,7$		;  7/12	;   if carry then force add, shift carry, and loop again
		djnz 1$		; 13(107); until --b = 0

3$:		; normalize result mantissa chl'

		exx			; activate bcdehl'
		bit 7,c			; test c' bit 7
		jr nz,4$		; if zero then
		dec b			;   decrement result exponent b'
		exx			;   activate bcdehl
		jp z,zerol		;   if result exponent is zero then return zero (underflow, cf reset)
		inc b			;   1 -> b loop counter
		jr nc,1$		;   loop once when no final rla carry for mantissa lsb
		jr 2$			;   loop once when final rla carry for mantissa lsb

4$:		; test to set cf for rounding

		exx			; activate bcdehl
.if DIVROUND
		jr c,5$			; (DR=1|2) if no final rla carry then
		add hl,de		; (DR=1|2)
		adc c			; (DR=1|2)   ahl + -cde -> cf.ahl test to set cf
5$:		ex de,hl		; (DR=1|2) remainder ahl -> ade
.endif;DIVROUND

		; restore sign l' bit 7

		ex af,af'		; restore a' with result sign, save a and cf
		ld l,a			; a -> l with result sign bit 7

		; finalize and round bchl.cf with remainder ade' to return float bcde with sign l' bit 7

.if DIVROUND
.if SUMROUND - DIVROUND
.if DIVROUND - 1
		or d			; (DR=2)
		or e			; (DR=2) check if all sticky bits in the remainder ade' are zero
		ex af,af'		; (DR=2) restore a and cf
		exx			; (DR=2) now make bchl' with sign l the active bchl with sign l'
		jr nc,6$		; (DR=2) if carry then
		ex af,af'		; (DR=2)   restore z
		call nz,roundtoaway	; (DR=2)   round to nearest ties to away mantissa chl.1 (z reset afterwards)
		call z,roundtoeven	; (DR=2)   or round to nearest ties to even mantissa chl.1
6$:		ld a,c			; (DR=2) chl -> ahl set result mantissa
		jp finalizea		; (DR=2) finalize bahl to return float bcde
.else
		exx			; (DR=1) now make bchl' with sign l the active bchl with sign l'
		ex af,af'		; (DR=1) restore a and cf
		call c,roundtoaway	; (DR=1) if carry then round result mantissa chl.cf
		jp finalizec		; (DR=1) finalize bchl.cf to return float bcde
.endif
.else
.if DIVROUND - 1
		or d			; (DR=2)
		or e			; (DR=2) check if all sticky bits in the remainder ade' are zero
		ex af,af'		; (DR=2) restore a and cf, save z
.endif
		exx			; (DR=1|2) now make bchl' with sign l the active bchl with sign l'
		jp finalizeroundc	; (DR=1|2) finalize and round bchl.cf to return float bcde
.endif
.else
		exx			; (DR=0) now make bchl' with sign l the active bchl with sign l'
		ld a,c			; (DR=0) chl -> ahl set result mantissa
		jp finalizea		; (DR=0) finalize bahl.cf to return float bcde
.endif;DIVROUND

7$:		; when cf.ahl > cde then add -cde and shift a one into chl'

		add hl,de	; 11	;
		adc c		;  4	;   ahl + -cde -> ahl
		scf		;  4	;   1 -> cf
		djnz 2$		; 13	; until --b = 0
		jr 3$			; normalize result mantissa chl'

divinfnan:	; division by inf or nan, return zero (cf reset) or nan (cf set)

		call ftype		; test bcde for nan
		ret c			; if bcde is nan then return nan (cf set)
		jp mulzero		; if bcde' is inf or nan then return nan (cf set) else return zero (cf reset)

divzero:	; division by zero, return inf or nan (cf set)

		exx			;
		EXPA			; exponent' -> a
		jp z,fnan		; if 0/0 then return nan (cf set)
		inc a			;
		jp nz,infb		; if n/0 with n not inf and nan then return signed inf (cf set)
		call ftype		; test bcde' for nan
		ret c			; if nan/0 then return nan (cf set)
		jp infb			; if inf/0 then return signed inf (cf set)

;-------------------------------------------------------------------------------
;
;		CONVERT FLOAT TO INTEGER
;
;		ftoi:	int(bcde) -> signed integer bcde truncated towards zero
;			cf set when out of range (bcde unchanged)
;			a,b,c,d,e,h,l modified
;
;-------------------------------------------------------------------------------

ftoi:		EXPA			; exponent -> a
		jp z,fzero		; if bcde is zero then return zero (cf reset)
		sub bias		; subtract exponent bias
		jp c,fzero		; if exponent is negative then return zero (cf reset)
		ld l,b			; save sign bit 7 to l
		ld h,a			; unbiased exponent -> h
		ld a,30			;
		sub h			; 30 - unbiased exponent -> a
		ret c			; if carry then out of range (cf set)
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

2$:		; rearrange cdea to the result integer bcde

		ld b,c			;
		ld c,d			;
		ld d,e			;
		ld e,a			;

3$:		; return positive integer bcde if float is positive

		rl l			; test l bit 7
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
;			a,b,c,d,e,h,l,l' modified
;
;-------------------------------------------------------------------------------

itof:		ld a,b			;
		or c			;
		or d			;
		or e			;
		ret z			; return if bcde is zero (cf reset)

		; save sign to l' and negate integer bcde when negative

		ld a,b			;
		exx			;
		ld l,a			; save sign b bit 7 to l'
		exx			;
		or a			; if sign is negative then
		call m,inegate		;   negate integer bcde

		; rearrange integer bcde to mantissa ahl.e

		ld a,b			;
		ld h,c			;
		ld l,d			;

		; set result exponent b and normalize nonzero mantissa ahl.e

		ld b,bias+31		; set exponent b
		or a			;
		jp m,2$			; if a bit 7 not set then
1$:		dec b		;  4	;   loop, decrement exponent b (cannot underflow)
		sla e		;  8	;
		adc hl,hl	; 15	;
		adc a		;  4	;     ahl.e << 1 -> ahl.e
		jp p,1$		; 10(51);   until a bit 7 set

2$:		; round ahl.e

.if SUMROUND
		rl e			; (SR=1|2) set cf to e bit 7
.if SUMROUND - 1
		ex af,af'		; (SR=2) save a and cf
		ld a,e			; (SR=2)
		or a			; (SR=2) set z if all sticky bits in e are zero
		ex af,af'		; (SR=2) restore a and cf, save z
.endif
		jp finalizerounda	; (SR=1|2) finalize and round bahl.cf to return float bcde with sign' l' bit 7
.else
		jp finalizea		; (SR=0) finalize bahl to return float bcde with sign' l' bit 7
.endif;SUMROUND

;-------------------------------------------------------------------------------
;
;		TRUNCATION
;
;		ftrunc:	trunc(bcde) -> bcde round towards zero
;			no errors (cf reset)
;			a,b,c,d,e,h,l modified
;
;		trunc(-bcde) = -trunc(bcde)
;		frac(bcde) = bcde - trunc(bcde)
;
;-------------------------------------------------------------------------------

ftrunc:		push bc			;
		push de			; save bcde
		call ftoi		; convert to integer if possible
		jr c,1$			; if converted then
		pop af			;
		pop af			;   discard old bcde
		jp itof			;   convert back to float and return
1$:		pop de			;
		pop bc			; restore old bcde
		or a			; reset cf
		ret			; return

;-------------------------------------------------------------------------------
;
;		FLOORING
;
;		ffloor:	floor(bcde) -> bcde round towards -infinity
;			cf set on overflow
;			a,b,c,d,e,h,l,a',b',c',d',e',h',l' modified
;
;		floor(|bcde|) = trunc(|bcde|)
;		ceil(bcde) = -floor(-bcde)
;
;-------------------------------------------------------------------------------

ffloor:		push bc			;
		push de			; save bcde
		call ftrunc		; trunc(bcde) -> bcde
		exx			;
		pop de			;
		pop bc			; pop old bcde -> bcde'
		exx			;
		push bc			; save trunc(bcde)
		push de			;
		call fsuby		; bcde' - trunc(bcde) -> frac(bcde)
		ISPOSZ			; test if frac(bcde) >= 0
		pop de			;
		pop bc			; restore trunc(bcde)
		ret z			; if frac(bcde) >= 0 then return trunc(bcde)
		exx			;
		ld bc,bias + 0x100 << 7	;
		ld de,0x0000		; -1.0e0 -> bcde'
		jp fadd			; return trunc(bcde) - 1.0e0

;-------------------------------------------------------------------------------
;
;		ROUNDING
;
;		fround:	round(bcde) -> bcde round to nearest, ties to away
;			cf set on overflow
;			a,b,c,d,e,h,l,a',b',c',d',e',h',l' modified
;
;		round(bcde) = floor(bcde + 0.5e0)
;
;-------------------------------------------------------------------------------

fround:		exx			;
		ld bc,bias - 1 << 7	;
		ld de,0x0000		; 0.5e0 -> bcde'
		call fadd		; bcde + 0.5e0 -> bcde (cannot overflow)
		jr ffloor		; return floor(bcde)

;-------------------------------------------------------------------------------
;
;		FLOATING POINT MULTIPLICATION BY POWER OF 10
;
;		fpow10:	10**a * bcde -> bcde
;			cf set if result float bcde is inf or nan
;			a,b,c,d,e,h,l,a',b',c',d',e',h',l' modified
;
;		four computational methods compared, minimum and mean number of
;		bits of accuracy (relative error) in 10 million random samples:
;
;		  method                                   min bits   mean bits
;		  iterative multiplication by 10:           20.2439     23.4988
;		  iterative multiplication by 10**4:        21.3502     24.4916
;		  iterative multiplication by 10**5:        21.3243     24.5500
;		  iterative multiplication by 10**6:        21.3297     24.5937
;		  iterative multiplication by 10**7:        21.3253     24.5619
;		  iterative multiplication by 10**8:        21.3004     24.5260
;		  iterative multiplication by 10**10:       21.0809     24.4483
;		  iterative multiplication by 10**16:       20.7694     24.0836
;		  table lookup with  4 powers of 10:        21.4136     24.6670
;		  table lookup with  8 powers of 10:        21.9131     25.2981
;		  table lookup with  9 powers of 10:        21.8907     25.4628
;		  table lookup with 10 powers of 10:        22.0177     25.5388
;		* table lookup with 12 powers of 10:        22.0482     25.7529
;		  table lookup with 16 powers of 10:        22.0004     25.4073
;		  exponentiation by squaring from bottom:   22.0004     25.3867
;		  exponentiation by squaring from top:      22.0023     25.3830
;		  table lookup with 38 powers of 10:        exact       exact
;
;		* = selected and enabled for small code size and high accuracy
;
;-------------------------------------------------------------------------------

.if 1		; table lookup with 12 powers of 10 for -128 <= a <= 127

fpow10:		or a			; test a and reset cf
		jp p,mulpow10		; if a > 0 then multiply bcde by 10**a
		neg			; -a -> a
		cp 39			;
		jr c,1$			; if a >= 39 then
		sub 38			;   a - 38 -> a
		call 1$			;   bcde / 10**a -> bcde
		ld a,38			;   38 -> a
1$:		push bc			;
		push de			;
		call pow10		; 10**a -> bcde
		exx			; bcde -> bcde'
		pop de			;
		pop bc			; restore bcde
		jp c,fzero		; if 10**a overflows then return zero (underflow, cf reset)
		jp fdivx		; bcde / 10**a -> bcde

pow10:		; return float bcde = 10**a for 0 < a <= 38

		ld bc,bias << 7		;
		ld de,0x0000		; 1.0e0 -> bcde

mulpow10:	; return float bcde * 10**a for 0 <= a <= 127

		ld l,11			; 11 -> l use power 10**12 from table
		jr 2$			; jump into loop to check and update counters
1$:		push hl			; loop, save counters
		ld a,l			;   l -> a ranging 0 to 11
		exx			;
		ld hl,powers		;   powers -> hl' table indexed by a from 0
		add a			;
		add a			;
		ld c,a			;
		ld b,0			;
		add hl,bc		;   hl' + 4 * a -> hl'
		ld c,(hl)		;
		inc hl			;
		ld b,(hl)		;
		inc hl			;
		ld e,(hl)		;
		inc hl			;
		ld d,(hl)		;   [hl'] -> bcde' with 10**2**(a-1)
		call fmul		;   10**(2**(a-1)) * bcde -> bcde
		pop hl			;   restore hl
		ret c			;   if overflow then return error (cf set)
		ld a,h			;   h -> a
2$:		sub 12			;
		ld h,a			;   a - 12 -> h
		jp p,1$			;   if h >= 0 then continue loop
		add 11			;   a + 11 -> a, cf = 0 if a < 0
		ret m			;   if a < 0 then return (cf reset)
		ld l,a			;   a -> l ranging 0 to 10
		jr 1$			; repeat

powers:		; table of powers 10**i for i = 1 to 12

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

.else
.if 1		; exponentiation by squaring from top with table lookup for -128 <= a <= 63

fpow10:		or a			; test a and reset cf
		jp p,mulpow10		; if a > 0 then multiply bcde by 10**a
		neg			; -a -> a
		cp 39			;
		jr c,1$			; if a >= 39 then
		sub 38			;   a - 38 -> a
		call 1$			;   bcde / 10**a -> bcde
		ld a,38			;   38 -> a
1$:		push bc			;
		push de			;
		call pow10		; 10**a -> bcde
		exx			; bcde -> bcde'
		pop de			;
		pop bc			; restore bcde
		jp c,fzero		; if 10**a overflows then return zero (underflow, cf reset)
		jp fdivx		; bcde / 10**a -> bcde

pow10:		; return float 10**a for 0 < a <= 38

		ld bc,bias << 7		;
		ld de,0x0000		; 1.0e0 -> bcde

mulpow10:	; return float bcde * 10**a for 0 <= a <= 63

		add a			;
		ret c			; if a >= 128 return error (cf set)
		add a			;
		ret c			; if a >= 64 return error (cf set)
		ld h,a			; a << 2 -> h to prep shifting h left 6 times
		ld l,7			; 7 -> l loop counter + 1
1$:		dec l			; for l = 6 to 1 step -1
		ret z			;   if l = 0 then break (cf reset)
		sla h			;   h << 1 -> cf,h
		jr nc,1$		;   if carry then
		push hl			;     save hl
		ld a,l			;
		exx			;
		ld hl,powers - 4	;     powers - 4 -> hl' table indexed by a from 1
		add a			;
		add a			;
		ld c,a			;
		ld b,0			;
		add hl,bc		;     hl' + 4 * a -> hl'
		ld c,(hl)		;
		inc hl			;
		ld b,(hl)		;
		inc hl			;
		ld e,(hl)		;
		inc hl			;
		ld d,(hl)		;     [hl'] -> bcde' with 10**2**(a-1)
		call fmul		;     10**(2**(a-1)) * bcde -> bcde
		pop hl			;     restore hl
		ret c			;     if overflow then return error (cf set)
		jr 1$			; next

powers:		; table of powers 10**2**i for i = 0 to 5

		.dw 0x4120,0x0000	; = 10**1
		.dw 0x42c8,0x0000	; = 10**2
		.dw 0x461c,0x4000	; = 10**4
		.dw 0x4cbe,0xbc20	; = 10**8
		.dw 0x5a0e,0x1bca	; = 10**16
		.dw 0x749d,0xc5ae	; = 10**32

.else		; direct table lookup with for -128 <= a <= 76

fpow10:		or a			; test a and reset cf
		ret z			; if a = 0 then return float bcde unchanged (cf reset)
		exx			;
		jp p,2$			; if a > 0 then multiply by 10th power
		neg			; -a -> a
		cp 39			;
		jr c,1$			; if a >= 39 then
		sub 38			;   a - 38 -> a
		call pow10		;   10**a -> bcde
		jp c,fzero		;   if 10**a overflows then return zero (underflow, cf reset)
		call fdivy		;   bcde' / 10**a -> bcde
		exx			;   bcde -> bcde'
		ld a,38			;   38 -> a
1$:		call pow10		; 10**a -> bcde'
		jp c,fzero		; if 10**a overflows then return zero (underflow, cf reset)
		jp fdivy		; bcde / 10**a -> bcde
2$:		cp 39			;
		jr c,3$			; if a >= 39 then
		sub 38			;   a - 38 -> a
		call pow10		;   10**a -> bcde
		ret c			;   if 10**a overflows then return error (cf set)
		call fmul		;   bcde' / 10**a -> bcde
		ret c			;   if overflow then return overflow (cf set)
		exx			;   bcde -> bcde'
		ld a,38			;   38 -> a
3$:		call pow10		; 10**a -> bcde'
		ret c			; if overflow then return overflow (cf set)
		jp fmul			; 10*a * bcde -> bcde

pow10:		; return float 10**a for 0 < a <= 38

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

powers:		; table of powers 10**i for i = 1 to 38

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

.endif
.endif

;-------------------------------------------------------------------------------
;
;		CONVERT STRING TO FLOAT
;
;		atof:	[hl..hl+a-1] -> bcde
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

atof:		ld b,a			; a -> b string length
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

		; check for a leading minus sign

		cp '-			;
		jr nz,1$		; if char is a '- then
		set 7,c			;   set sign c bit 7 (negative)
		jr 2$			;   jump into parsing the next character

1$:		; check for a leading plus sign

		cp '+			;
		jr nz,3$		;
2$:		inc hl			;
		dec b			; decrement length and check if zero
		scf			;
		ret z			; if remaining length b = 0 then error (cf set)

3$:		; check for inf and nan

		ld a,b			;
		cp 3			;
		jr nz,parse_float	; if remaining length b <> 3 then parse float
		ld a,(hl)		;
		or 0x20			;
		cp 'i			;
		jr z,4$			; if char is 'i or 'I then parse inf
		cp 'n			;
		jr nz,parse_float	; if char is not 'n or 'N then parse float

		; parse and return nan (cf reset)

		inc hl			;
		ld a,(hl)		;
		or 0x20			;
		cp 'a			;
		scf			;
		ret nz			;
		inc hl			;
		ld a,(hl)		;
		or 0x20			;
		cp 'n			;
		scf			;
		ret nz			; return error (cf set)
		call fnan		; return nan
		or a			;
		ret			;

4$:		; parse and return inf (cf reset)

		inc hl			;
		ld a,(hl)		;
		or 0x20			;
		cp 'n			;
		scf			;
		ret nz			;
		inc hl			;
		ld a,(hl)		;
		or 0x20			;
		cp 'f			;
		scf			;
		ret nz			; return error (cf set)
		ld a,c			; sign c -> a bit 7
		call finfa		;
		or a			;
		ret			;

parse_float:	; loop to parse float

		ld a,(hl)		;
		inc hl			; [hl++] -> a next char
		cp '.			;
		jr nz,1$		; if char is '. then
		bit 6,c			;
		scf			;
		ret nz			;   if c bit 6 is set then return error (cf set)
		set 6,c			;   set c bit 6 to mark dp parsed
		jr parse_next		;   parse next char

1$:		; check for the E and e signs

		cp 'E			;
		jp z,parse_exponent	; if char is 'E then parse exponent
		cp 'e			;
		jp z,parse_exponent	; if char is 'e then parse exponent

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

2$:		; multiply mantissa accumulator dehl' by 10

		exx			;
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
		ld b,0			;
		ld c,a			;
		add hl,bc		;
		jr nc,4$		;
		inc de			;   dehl' + a -> dehl'
4$:		ld c,1			;   1 -> c' mark that a digit was parsed
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
		jp z,fzero		; if dehl' = 0 then return zero (cf reset)

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

2$:		; round and rearrange aeh'.l' to cde'

.if SUMROUND
		rl l			; (SR=1|2) set cf to l bit 7, set z if all sticky bits are zero
		ld l,h			; (SR=1|2)
		ld h,e			; (SR=1|2)
		ld c,a			; (SR=1|2) aeh' -> chl'
.if SUMROUND - 1
		jr nc,3$		; (SR=2) if carry then
		call nz,roundtoaway	; (SR=2)   either round to nearest ties to away mantissa chl.1 (z reset afterwards)
		call z,roundtoeven	; (SR=2)   or round to nearest ties to even mantissa chl.1
.else
		call c,roundtoaway	; (SR=1) if carry then round result mantissa chl.cf
.endif
3$:		ex de,hl		; (SR=1|2) chl' -> cde' set result mantissa
.else
		ld c,a			; (SR=0)
		ld d,e			; (SR=0)
		ld e,h			; (SR=0) aeh' -> cde' set result mantissa
.endif;SUMROUND
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
		rr c			; cf.c >> 1 -> c pick up lsb exponent bit
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

1$:		; check for decimal exponent plus sign

		cp '+			;
		jr nz,4$		; if char is not '+ then jump into loop to parse digit

2$:		; check and get next char after +/- sign

		dec b			; decrement remaining length b
		scf			;
		ret z			; if b = 0 then return error (cf set)

3$:		; loop to parse and accumulate decimal digits of the decimal exponent

		ld a,(hl)		; loop
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

4$:		; convert digit and add to decimal exponent

		sub '0			;   convert a to digit
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
		jr nz,parse_done	; if exponent sign is plus then return float bcde' scaled by 10**a
		xor a			;
		sub e			;
		ld e,a			; negate e with decimal exponent
		jr parse_done		; return float bcde' scaled by 10**a

;-------------------------------------------------------------------------------
;
;		CONVERT FLOAT TO STRING
;
;		ftoa:	bcde -> [hl...hl+a-1] digits, exponent e and sign d bit 7
;			cf set when bcde is inf or nan (cannot convert)
;			a,b,c,d,e,h,l,a',b',c',d',e',h',l' modified
;			 a  nonzero buffer size argument
;			bc  float argument
;			de  float argument
;			hl  buffer pointer
;			 b  remaining buffer size
;			 c  remaining buffer size saved copy
;			 d  sign bit 7 result
;			 e  signed decimal exponent result
;			bc' mantissa workspace
;			de' mantissa workspace
;			hl' mantissa workspace
;
;-------------------------------------------------------------------------------

ftoa:		push hl			; save buffer address hl
		push af			; save nonzero buffer size a
		ISNN			; test if bcde is inf or nan
		jr nz,0$		; if bcde is inf or nan then
		pop af			;   restore buffer size a
		pop hl			;   restore buffer address hl
		scf			;   set cf
		ret			;   return inf/nan error (cf set)

0$:		; estimate decimal exponent = exp*77/256 ~= log10(2**exp) = exp * log10(2)

		EXPA			; exponent -> a
		jr z,3$			; if bcde is zero then populate buffer with 0s
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
		call fpow10		; bcde * 10**-a -> bcde

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
		EXPA			; exponent -> a
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

3$:		; return buffer with 0s

		pop bc			; pop buffer size b
		pop hl			; pop buffer address hl
4$:		ld (hl),'0		; loop
		inc hl			;   '0 -> [hl++]
		djnz 4$			; until --b = 0
		ld d,b			; 0 -> d positive sign
		ld e,b			; 0 -> e zero decimal exponent
		xor a			; reset cf and set z
		ret			; return (cf reset)

5$:		; leading digit is zero, shift it away

		exx			;
		pop de			;
		dec e			; decrement the saved decimal exponent e
		push de			;

6$:		; loop over the buffer to populate

		exx			; loop

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

7$:		; digit is now in a, clear digit d in d.ehl'

		ld d,0			;   0 -> d' clear decimal digit

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
		jr c,9$			;   if digit <= '9 then return (cf reset)
		ld (hl),'0		;   '0 -> [hl]
		djnz 8$			; until --b = 0
		ld (hl),'1		; '1 -> [hl] make it a leading 1, rest all 0s
		inc e			; increment decimal exponent e
9$:		xor a			; reset cf and set z
		ret			; return (cf reset)
