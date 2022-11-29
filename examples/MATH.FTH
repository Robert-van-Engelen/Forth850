\ MATH.FTH collection of math functions

: FACT  ( u -- r )
  1E0 ROT 1+ 1 ?DO
    I S>D D>F F*
  LOOP ;

\ Square root using IEEE 754 binary representation to compute
\ sqrt(x*2^n) = sqrt(x*2^(n%2))*2^(n/2) with Newton Raphson

: FSQRT ( r1 -- r2 )
  2DUP F0< IF -46 THROW THEN
  2DUP F0= IF EXIT THEN
  DUP 8 RSHIFT $3f - \ 2^(n/2) = exponent/2 - bias/2
  -ROT $ff AND $3f00 + \ remove exponent 2^(n/2)
  2DUP \ initial estimate
  10 0 DO
    2OVER 2OVER F/ F+ .5E0 F* \ Newton Raphson
  LOOP
  2SWAP 2DROP
  ROT $7f + 7 LSHIFT 0 SWAP \ 2^(n/2)
  F* ;

\ Trig words based on Jupiter ACE "FORTH Programming" p.93

: FSIN  ( r1 -- r2 ) \ only valid for -2pi <= r1 <= 2pi
  2DUP 2DUP 2DUP F* FNEGATE
  2ROT 2ROT
  15 2 DO
    5 PICK 5 PICK
    F* I I 1+ *
    S>D D>F F/
    2DUP 2ROT F+ 2SWAP
  2 +LOOP
  2DROP 2SWAP 2DROP ;

: FCOS  ( r1 -- r2 ) 1.5707962E0 2SWAP F- FSIN ;

: FTAN  ( r1 -- r2 ) 2DUP FSIN 2SWAP FCOS F/ ;
