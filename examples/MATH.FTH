\ MATH.FTH collection of math functions

: FTRUNC        ( r1 -- r2 ) 2DUP ['] F>D CATCH 0= IF D>F 2SWAP THEN 2DROP ;

: FLOOR         ( r1 -- r2 ) 2DUP F0< IF $3F7FFFFF. ( = .99999... ) F- THEN FTRUNC ;

: FROUND        ( r1 -- r2 ) .5E0 F+ FLOOR ;

\ Factorial of an integer returns float

: FACT          ( u -- r )
  1E0 ROT 1+ 1 ?DO
    I S>D D>F F*
  LOOP ;

\ Square root using IEEE 754 binary representation to compute
\ sqrt(x*2^n) = sqrt(x*2^(n%2))*2^(n/2) with Newton Raphson

: FSQRT         ( r1 -- r2 )
  2DUP F0< IF -46 THROW THEN
  2DUP F0= IF EXIT THEN
  DUP 8 RSHIFT $3f - \ 2^(n/2) = exponent/2 - bias/2
  -ROT $ff AND $3f00 + \ remove exponent 2^(n/2)
  2DUP \ initial estimate
  5 0 DO
    2OVER 2OVER F/ F+ .5E0 F* \ Newton Raphson
  LOOP
  2SWAP 2DROP
  ROT $7f + 7 LSHIFT 0 SWAP \ 2^(n/2)
  F* ;

\ Trig words based on Jupiter ACE "FORTH Programming" p.93
\ improved to cover a wider range of angles and for speed (fewer terms to sum)

: FSIN          ( r1 -- r2 )
  \ map r1 to [0,pi/2)
  1.5707962E0 F/
  2DUP F0< IF FABS 2E0 F+ THEN \ -r1 = pi+r1
  2DUP F>D \ truncate
  OVER >R \ to test four quadrants
  D>F F- \ frac(r1/(pi/2))
  R@ 1 AND IF 1E0 2SWAP F- THEN \ quadrant 1 and 3
  R> 2 AND IF FNEGATE THEN \ quadrant 0 and 2
  1.5707962E0 F*
  \ Taylor series
  2DUP 2DUP 2DUP F* FNEGATE
  2ROT 2ROT
  11 2 DO
    5 PICK 5 PICK
    F* I I 1+ *
    S>D D>F F/
    2DUP 2ROT F+ 2SWAP
  2 +LOOP
  2DROP 2SWAP 2DROP ;

: FCOS          ( r1 -- r2 ) 1.5707962E0 2SWAP F- FSIN ;

: FTAN          ( r1 -- r2 ) 2DUP FSIN 2SWAP FCOS F/ ;
