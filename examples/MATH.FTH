\ MATH.FTH collection of floating point math functions
\ Based on IEEE 754 mathr.asm rounding modes SUM/MUL/DIV=2/2/2
\ Author: Dr. Robert van Engelen

\ Factorial of an integer returns float

: FACT      ( u -- r )
  1E0 ROT 1+ 1 ?DO
    I S>F F*
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
  2DUP \ initial estimate is copy of x
  5 0 DO
    2OVER 2OVER F/ F+ .5E0 F*
  LOOP
  2SWAP 2DROP
  ROT $7f + 7 LSHIFT 0 SWAP F* ; \ times 2^(n/2)

\ FSIN word based on Jupiter ACE "FORTH Programming" p.93
\ improved to cover a wider range of angles and for speed (fewer terms to sum)

3.1415928E0 2CONSTANT PI

: FSIN      ( r1 -- r2 )
  \ map r1 to [0,pi/2) and adjust for quadrant 0, 1, 2, 3
  1.5707964E0 F/ \ rescale r1 to x in [0,1)
  2DUP F0< IF FABS 2E0 F+ THEN \ -x=2+x i.e. -r1=pi+r1
  2DUP F>D \ truncate x
  OVER >R \ save truncated low order x to test for quadrant
  D>F F- \  frac(x)=x-trunc(x) i.e. frac(r1/(pi/2))
  R@ 1 AND IF 1E0 2SWAP F- THEN \ quadrant 1 and 3: 1-x
  R> 2 AND IF FNEGATE THEN \ quadrant 2 and 3: -x
  1.5707964E0 F* \ revert rescaling to obtain x in (-pi/2,pi/2]
  \ Maclaurin series sin(x) = x - x^3/3! + x^5/5! - x^7/7! + ...
  2DUP 2DUP 2DUP F* FNEGATE 2ROT 2ROT \ -- -x*x x x
  11 2 DO
    5 PICK 5 PICK F* \ -- -x*x sum -x*x*term
    I DUP 1+ *       \ -- -x*x sum -x*x*term i*(i+1)
    S>F F/           \ -- -x*x sum -x*x*term/i*(i+1)
    2SWAP 2OVER F+   \ -- -x*x -x*x*term/i*(i+1) sum-x*x*term/i*(i+1)
    2SWAP            \ -- -x*x sum-x*x*term/i*(i+1) -x*x*term/i*(i+1) 
  2 +LOOP
  2DROP 2SWAP 2DROP ;

: FCOS      ( r1 -- r2 ) 1.5707964E0 2SWAP F- FSIN ;

: FTAN      ( r1 -- r2 ) 2DUP FSIN 2SWAP FCOS F/ ;

: FASIN     ( r1 -- r2 )
  1E0 2OVER FABS F< IF -46 THROW THEN
  \ map r1 to [-sqrt(1/2),sqrt(1/2)] using arcsin(x) = pi/2-arcsin(qrt(1-x^2))
  0.70710678E0 2OVER FABS F< IF \ if |r1| > sqrt(1/2) then
    2DUP F0<
    -ROT
    2DUP F* 1E0 2SWAP F- FSQRT 
    TRUE
  ELSE
    FALSE
  THEN
  -ROT
  \ Maclaurin series arcsin(x) = x + (1/2)x^3/3 + (1*3)/(2*4)x^5/5 + (1*3*5)/(2*4*6)x^7/7 + ...
  2DUP 2DUP 2DUP F* 2ROT 2ROT \ -- x*x x x
  34 3 DO
    5 PICK 5 PICK F*     \ -- x*x sum term*x*x
    I 2- S>F F*          \ -- x*x sum term*x*x*(i-2)
    I 1- S>F F/          \ -- x*x sum term*x*x*(i-2)/(i-1)
    2SWAP 2OVER I S>F F/ \ -- x*x term*x*x*(i-2)/(i-1) sum term*x*x*(i-2)/(i-1)/i
    F+                   \ -- x*x term*x*x*(i-2)/(i-1) sum+term*x*x*(i-2)/(i-1)/i
    2SWAP                \ -- x*x sum+term*x*x*(i-2)/(i-1)/i term*x*x*(i-2)/(i-1)
  2 +LOOP
  2DROP 2SWAP 2DROP
  ROT IF 1.5707964E0 2SWAP F- ROT IF FNEGATE THEN THEN ; \ sign(x)*(pi/2-arcsin(sqrt(1-x^2)))

: FACOS     ( r1 -- r2 ) FASIN 1.5707964E0 2SWAP F- ; \ = pi/2 - arcsin(x)

: FATAN     ( r1 -- r2 )
  \ map r1 to [-1,1] using arctan(x) = sign(x) * (pi/2-arctan(1/abs(x)))
  1E0 2OVER FABS F< IF \ if |r1| > 1 then
    2DUP F0< -ROT
    1E0 2SWAP FABS F/
    TRUE
  ELSE
    FALSE
  THEN
  -ROT
  \ map r1 in [-1,1] to [-sqrt(2)+1,sqrt(2)-1] using arctan(x) = 2*arctan(x/(1+sqrt(1+x^2)))
  .41423562E0 2OVER FABS F< IF \ if |r1| > sqrt(2)-1 then
    2DUP 2DUP F* 1E0 F+ FSQRT 1E0 F+ F/
    TRUE
  ELSE
    FALSE
  THEN
  -ROT
  \ Maclaurin series arctan(x) = x - x^3/3 + x^5/5 - x^7/7 + ... with x in (-1,1)
  2DUP 2DUP 2DUP F* FNEGATE 2ROT 2ROT \ -- -x*x x x
  16 3 DO
    5 PICK 5 PICK F* \ -- -x*x sum -x*x*term
    2DUP I S>F F/    \ -- -x*x sum -x*x*term -x*x*term/i
    2ROT F+          \ -- -x*x -x*x*term/i sum-x*x*term/i
    2SWAP            \ -- -x*x sum-x*x*term/i -x*x*term/i
  2 +LOOP
  2DROP 2SWAP 2DROP
  ROT IF 2E0 F* THEN \ 2*arctan(x/(1+sqrt(1+x^2)))
  ROT IF 1.5707964E0 2SWAP F- ROT IF FNEGATE THEN THEN ; \ sign(x) * (pi/2-arctan(1/abs(x)))

\ correctly rounded (*) FLN by summing in reverse order using the stack as temporary storage (40 cells)
\ *) but loses accuracy close and above 1.0 such as 1.001 gives 5 digits accuracy instead of exact

: FLN       ( r1 -- r2 )
  2DUP F0< IF -46 THROW THEN
  2DUP F0= IF -46 THROW THEN
  \ map r1 to [0.5,1) using ln(x*2^n) = ln(x) + ln(2^n) = ln(x) + n*ln(2)
  DUP 7 RSHIFT $7e - -ROT \ 2^(n+1) = 2^(exponent - bias + 1)
  $7f AND $3f00 + \ remove exponent 2^(n+1)
  1E0 2SWAP F- \ 1-x
  \ Maclaurin series -ln(1-x) = x + x^2/2 + x^3/3 + ... with x in (0,0.5]
  2DUP 2DUP \ -- x x x
  22 2 DO
    2OVER F*      \ -- x x^2/2 ... x x^n*x
    2DUP I S>F F/ \ -- x x^2/2 ... x x^(n+1) x^(n+1)/(n+1) 
    2ROT 2ROT     \ -- x x^2/2 ... x^(n+1)/(n+1) x x^(n+1)
  LOOP
  2DROP 2DROP     \ -- x x^2/2 ... x^19/19
  22 2 DO F+ LOOP
  FNEGATE
  ROT S>F 0.69314724E0 F* F+ ; \ + n*ln(2) with approx ln(2) such that 1E0 FLN = 0

\ slightly less accurate FLN without temporary storage on the stack

\ : FLN       ( r1 -- r2 )
\   2DUP F0< IF -46 THROW THEN
\   2DUP F0= IF -46 THROW THEN
\   \ map r1 to [0.5,1) using ln(x*2^n) = ln(x) + ln(2^n) = ln(x) + n*ln(2)
\   DUP 7 RSHIFT $7e - -ROT \ 2^(n+1) = 2^(exponent - bias + 1)
\   $7f AND $3f00 + \ remove exponent 2^(n+1)
\   1E0 2SWAP F- \ 1-x
\   \ Maclaurin series -ln(1-x) = x + x^2/2 + x^3/3 + ... with x in (0,0.5]
\   2DUP 2DUP \ -- x x x
\   22 2 DO
\     5 PICK 5 PICK F*     \ -- x sum x^n*x
\     2SWAP 2OVER I S>F F/ \ -- x x^(n+1) sum x^(n+1)/(n+1)
\     F+                   \ -- x x^(n+1) sum+x^(n+1)/(n+1)
\     2SWAP                \ -- x sum+x^(n+1)/(n+1) x^(n+1)
\   LOOP
\   2DROP 2SWAP 2DROP FNEGATE
\   ROT S>F 0.693147245E0 F* F+ ; \ + n*ln(2) approx ln(2) such that 1E0 FLN = 0

: FLOG      ( r1 -- r2 ) FLN 0.4342945E0 F* ; \ = ln(x)/ln(10) appeox ln(10) such that 10E0 FLOG = 1E0

\ correctly rounded exp(x) by summing in reverse order using the stack as temporary storage (18 cells)

: FEXP      ( r1 -- r2 )
  2DUP F0< -ROT
  FABS
  \ map |r1| to [0,ln(2)) using exp(x+k*ln(2)) = exp(x)*2^k
  2DUP 0.69314724E0 F/ F>D D>S DUP>R
  S>F 0.69314724E0 F* F-
  \ Maclaurin series exp(x) = 1 + x + x^2/2! + x^3/3! + ...
  1E0 2SWAP 2DUP 2DUP \ -- 1 x x x
  10 2 DO
    2OVER F*    \ -- 1 x x^2/2! ... x term*x
    I S>F F/    \ -- 1 x x^2/2! ... x term*x/i
    2SWAP 2OVER \ -- 1 x x^2/2! ... term*x/i x term*x/i
  LOOP
  2DROP 2DROP   \ -- 1 x x^2/2! ... x^9/9!
  \ sum the 10 terms in reverse order
  10 1 DO F+ LOOP
  \ multiply exp(x) by 2^k
  R> 7 LSHIFT +
  \ return reciprocal for negative r1
  ROT IF
    1E0 2SWAP F/
  THEN ;

\ slightly less accurate FEXP without temporary storage on the stack

\ : FEXP      ( r1 -- r2 )
\   2DUP F0< -ROT
\   FABS
\   \ map |r1| to [0,ln(2)) using exp(x+k*ln(2)) = exp(x)*2^k
\   2DUP 0.693147245E0 F/ F>D D>S DUP>R
\   S>F 0.693147245E0 F* F-
\   \ Maclaurin series expm1(x) = exp(x) - 1 = x + x^2/2! + x^3/3! + ...
\   2DUP 2OVER \ -- x x x
\   10 2 DO
\     5 PICK 5 PICK F* \ -- x sum term*x
\     I S>F F/         \ -- x sum term*x/i
\     2SWAP 2OVER F+   \ -- x term*x/i sum+term*x/i
\     2SWAP            \ -- x sum+term*x/i term*x/i
\   LOOP
\   2DROP 2SWAP 2DROP
\   \ exp(x) = expm1(x) + 1
\   1E0 F+
\   \ multiply exp(x) by 2^k
\   R> 7 LSHIFT +
\   \ return reciprocal for negative r1
\   ROT IF
\     1E0 2SWAP F/
\   THEN ;

: FALOG     ( r1 -- r2 ) 2.3025853E0 F* FEXP ; \ = exp(x*ln(10)) approx ln(10) such that 1E0 ALOG = 10E0

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
    IF -46 THROW THEN \ error when exponent ud exceeds 16 bits
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
