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

tests_begin:

.if 0

.str |8 CONSTANT RR\12|
.str |0 VALUE SS\12|
.str |0 VALUE XX\12|
.str |0 VALUE YY\12|
.str |CREATE AA RR 1+ ALLOT\12|
.str |: RCLAA POSTPONE AA POSTPONE + POSTPONE C@ ; IMMEDIATE\12|
.str |: STOAA POSTPONE AA POSTPONE + POSTPONE C! ; IMMEDIATE\12|
.str |: NQCORE\12|
.str |  0 TO SS\12|
.str |  0 TO XX\12|
.str |  BEGIN\12|
.str |    1 +TO XX RR XX STOAA\12|
.str |    BEGIN\12|
.str |      1 +TO SS\12|
.str |      XX TO YY\12|
.str |      BEGIN YY 1 > WHILE\12|
.str |        -1 +TO YY\12|
.str |        XX RCLAA YY RCLAA - DUP\12|
.str |        0= SWAP ABS XX YY - = OR IF\12|
.str |          0 TO YY\12|
.str |          BEGIN XX RCLAA 1- DUP XX STOAA 0= WHILE\12|
.str |            -1 +TO XX\12|
.str |          REPEAT\12|
.str |        THEN\12|
.str |      REPEAT\12|
.str |    YY 1 = UNTIL\12|
.str |  RR XX = UNTIL\12|
.str |;\12|
.str |: NQUEENS\12|
.str |  NQCORE\12|
.str |  ." S=" SS .\12|
.str |;\12|
.str |: TESTNQ 100 0 DO NQUEENS LOOP ;\12|

.endif

.str |: T 7 THROW ;\12|
.str |: C ['] T CATCH ;\12|

tests_end:

		COLON INSTALL,install
		.dw dolit,tests_begin,dolit,tests_end-tests_begin,evaluate
		.dw doret

.if 0

; CALL		af bc de hl addr -- af bc de hl
;		call subroutine at addr

		CODE CALL,call
		push de
		exx
		pop ix
		pop hl
		pop de
		pop bc
		pop af
		call 1$
		push af
		push bc
		push de
		push hl
		exx
		pop de
		JP_NEXT
1$:		jp (ix)

.endif

; REGS		--
;		display CPU registers, wait for key

		CODE REGS,regs
		call REGOUT
		JP_NEXT
