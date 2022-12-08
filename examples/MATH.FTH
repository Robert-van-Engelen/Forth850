\ MATH.FTH collection of floating point math functions
\ Based on IEEE 754 mathr.asm with rounding configuration SUM/MUL/DIV=2/2/2
\ Author: Dr. Robert van Engelen

: FMAX      ( r1 r2 -- r3 ) 2OVER 2OVER F< IF 2SWAP THEN 2DROP ;

: FMIN      ( r1 r2 -- r3 ) 2OVER 2OVER F< INVERT IF 2SWAP THEN 2DROP ;

\ Factorial of an integer returns float

: FACT      ( u -- r )
  1E0 ROT 1+ 1 ?DO
    I S>D D>F F*
  LOOP ;

\ Square root using IEEE 754 binary representation to compute
\ sqrt(x*2^n) = sqrt(x*2^(n%2))*2^(n/2) with Newton Raphson

: FSQRT     ( r1 -- r2 )
  2DUP F0< IF -46 THROW THEN
  2DUP F0= IF EXIT THEN
  \ map r1 to [0.5,2) using sqrt(x*2^n) = sqrt(x*2^(n%2))*2^(n/2)
  DUP 8 RSHIFT $3f - -ROT \ 2^(n/2) = 2^(exponent/2 - bias/2)
  $ff AND $3f00 + \ remove exponent 2^(n/2)
  \ Newton Raphson
  2DUP \ initial estimate
  5 0 DO
    2OVER 2OVER F/ F+ .5E0 F* \ Newton Raphson
  LOOP
  2SWAP 2DROP
  ROT $7f + 7 LSHIFT 0 SWAP F* ; \ times 2^(n/2)

\ Trig words based on Jupiter ACE "FORTH Programming" p.93
\ improved to cover a wider range of angles and for speed (fewer terms to sum)

3.1415928E0 2CONSTANT PI

: FSIN      ( r1 -- r2 )
  \ map r1 to [0,pi/2) and adjust for the quadrant 0, 1, 2, 3
  1.5707964E0 F/
  2DUP F0< IF FABS 2E0 F+ THEN \ -r1 = pi+r1
  2DUP F>D \ truncate
  OVER >R \ save to test four quadrants
  D>F F- \ frac(r1/(pi/2))
  R@ 1 AND IF 1E0 2SWAP F- THEN \ quadrant 1 and 3
  R> 2 AND IF FNEGATE THEN \ quadrant 2 and 3
  1.5707964E0 F*
  \ Maclaurin series sin(x) = x - x^3/3! + x^5/5! - x^7/7! + ...
  2DUP 2DUP 2DUP F* FNEGATE
  2ROT 2ROT \ -- -x*x x x
  11 2 DO
    5 PICK 5 PICK \ -- -x*x sum term -x*x 
    F* I I 1+ * \ -- -x*x sum -x*x*term i*(i+1)
    S>D D>F F/ \ -- -x*x sum -x*x*term/i*(i+1)
    2DUP 2ROT F+ 2SWAP \ -x*x sum-x*x*term/i*(i+1) -x*x*term/i*(i+1) 
  2 +LOOP
  2DROP 2SWAP 2DROP ;

: FCOS      ( r1 -- r2 ) 1.5707964E0 2SWAP F- FSIN ;

: FTAN      ( r1 -- r2 ) 2DUP FSIN 2SWAP FCOS F/ ;

\ Log and antilog

: FLN       ( r1 -- r2 )
  2DUP F0< IF -46 THROW THEN
  2DUP F0= IF -46 THROW THEN
  \ map r1 to [0.5,1) using ln(x*2^n) = ln(x) + ln(2^n) = ln(x) + n*ln(2)
  DUP 7 RSHIFT $7e - -ROT \ 2^(n+1) = 2^(exponent - bias + 1)
  $7f AND $3f00 + \ remove exponent 2^(n+1)
  1E0 2SWAP F-
  \ Maclaurin series -ln(1-x) = x + x^2/2 + x^3/3 + ... with x in (0,0.5]
  2DUP 2DUP \ -- x x x
  20 2 DO
    5 PICK 5 PICK \ -- x sum x^n x
    F* \ -- x sum x^(n+1)
    2DUP I S>D D>F F/ \ -- x sum x^(n+1) x^(n+1)/(n+1)
    2ROT F+ 2SWAP \ -- x sum+x^(n+1)/(n+1) x^(n+1)
  LOOP
  2DROP 2SWAP 2DROP FNEGATE
  ROT S>D D>F 0.6931468E0 F* F+ ; \ plus n*ln(2) = n*0.69314718055995

: FLOG      ( r1 -- r2 ) FLN 0.43429475E0 F* ; \ log(x) = ln(x)/ln(10) = ln(x)*0.43429448190325

: FEXP      ( r1 -- r2 )
  2DUP F0< -ROT
  FABS
  \ map |r1| to [0,0.5) using exp(x*2^n) = exp(x)^(2^n)
  DUP 7 RSHIFT $7e - \ 2^(n+1) = 2^(exponent - bias + 1)
  DUP 0< IF
    DROP 0 -ROT \ n+1<0 in exponent 2^(n+1)
  ELSE
    -ROT 
    $7f AND $3f00 + \ remove exponent 2^(n+1)
  THEN
  \ Maclaurin series exp(x) = 1 + x + x^2/2! + x^3/3! + ...
  2DUP 1E0 F+ 2OVER \ x 1+x x
  11 2 DO
    5 PICK 5 PICK \ -- x sum term x 
    F* I S>D D>F F/ \ -- x sum x*term/i
    2DUP 2ROT F+ 2SWAP \ x sum+x*term/i x*term/i
  LOOP
  2DROP 2SWAP 2DROP
  \ square exp(x) n+1 times when n+1>0 in exponent 2^(n+1)
  ROT 0 ?DO
    2DUP F*
  LOOP
  \ return reciprocal for negative r1
  ROT IF
    1E0 2SWAP F/
  THEN ;

: FALOG     ( r1 -- r2 ) 2.302586E0 F* FEXP ; \ exp(x*ln(10))

\ Exponentiation (simple version)

: F^        ( r1 r2 -- r3 ) 2SWAP FLN F* FEXP ;

\ A more powerful F** using exponentiation by squaring when r2 is integer

: F**       ( r1 r2 -- r3 )
  \ r1 and r2 cannot be both zero
  2DUP F0= IF 2OVER F0= IF -46 THROW THEN THEN
  2DUP 2DUP FTRUNC F= IF
    \ exponentiation by squaring
    2DUP F0< >R \ r2 is negative
    FABS F>D 1E0 2SWAP \ -- r1 1 ud
    IF -46 THROW THEN \ exponent d exceeds 16 bits
    >R
    BEGIN
      R@ 1 AND IF 2OVER F* THEN
      R> 1 RSHIFT \ -- r1^n product u>>1
    DUP WHILE
      >R
      2SWAP 2DUP F* 2SWAP \ -- r1^n^2 product u>>1
    REPEAT
    DROP 2SWAP 2DROP \ -- product
    R> IF 1E0 2SWAP F/ THEN \ r2 was negative
  ELSE
    F^
  THEN ;

\ Hyperbolics

: FCOSH     ( r1 -- r2 ) FEXP 2DUP 1E0 2SWAP F/ F+ 2E0 F/ ;

: FSINH     ( r1 -- r2 ) FEXP 2DUP 1E0 2SWAP F/ F- 2E0 F/ ;

: FTANH     ( r1 -- r2 ) 2DUP F+ FEXP 2DUP 1E0 F- 2SWAP 1E0 F+ F/ ;

: FACOSH    ( r1 -- r2 ) 2DUP 2DUP F* 1E0 F- FSQRT F+ FLN ;

: FASINH    ( r1 -- r2 ) 2DUP 2DUP F* 1E0 F+ FSQRT F+ FLN ;

: FATANH    ( r1 -- r2 ) 2DUP 1E0 F+ 2SWAP 1E0 2SWAP F- F/ FLN 2E0 F/ ;
