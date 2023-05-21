\ MATH.FTH collection of floating point math functions
\ Use TRIG.FTH and LOGS.FTH for stripped-down implementations
\ Based on IEEE 754 mathr.asm rounding modes SUM/MUL/DIV=2/2/2
\ Author: Dr. Robert van Engelen

ANEW _MATH_

DECIMAL

\ Correctly rounded FSQRT using IEEE 754 binary representation to compute
\ sqrt(x*2^n) = sqrt(x*2^(n%2))*2^(n/2) with Newton Raphson

: FSQRT     ( r1 -- r2 )
  2DUP F0< IF -46 THROW THEN
  2DUP F0= IF EXIT THEN
  \ map r1 to [0.5,2) using sqrt(x*2^n) = sqrt(x*2^(n%2))*2^(n/2)
  DUP 8 RSHIFT $3f - -ROT \ 2^(n/2) = 2^(exponent/2 - bias/2)
  $ff AND $3f00 + \ remove exponent 2^(n/2) from x
  \ Newton Raphson
  2DUP \ initial estimate is copy of x
  5 0 DO
    2OVER 2OVER F/ F+ .5E0 F*
  LOOP
  2SWAP 2DROP
  ROT $7f + 7 LSHIFT 0 SWAP F* ; \ times 2^(n/2)

\ Constants

  3.1415928E0 2CONSTANT PI
  1.5707964E0 2CONSTANT PI/2
  .69314724E0 2CONSTANT LN2  \ approx ln(2) such that 1E0 FLN = 0
  2.3025853E0 2CONSTANT LN10 \ approx ln(10) such that 1E0 ALOG = 10E0

\ Accurate sine and cosine by summing in reverse order using the stack as temporary storage (8 cells)

: FCOSI     ( r1 flag -- r2 ) \ r2=sin(r1) if flag=-1 else r2=cos(r1) if flag=0
  >R
  \ map r1 to x in [-pi/4,pi/4]
  PI/2 F/
  \ floor(x + .5)
  2DUP .5E0 F+ F>D
  \ save (floor(x + .5) + flag) mod 4 quadrant 0,1,2,3 where flag is -1 (sin) or 0 (cos)
  OVER R> + 3 AND >R
  \ pi/2 * (x - floor(x + .5))
  D>F F- PI/2 F*
  2DUP 2DUP F* FNEGATE 2SWAP \ -- -x*x x
  R> CASE
  0 OF 2DROP  1E0 2SWAP 1 ENDOF \  cos(x) =  1 - x^2/2! + x^4/4! - x^6/6! + ...
  1 OF FNEGATE    2SWAP 2 ENDOF \ -sin(x) = -x + x^3/3! - x^5/5! + x^7/7! - ...
  2 OF 2DROP -1E0 2SWAP 1 ENDOF \ -cos(x) = -1 + x^2/2! - x^4/4! + x^6/6! - ...
  3 OF            2SWAP 2 ENDOF \  sin(x) =  x - x^3/3! + x^5/5! - x^7/7! + ...
  ENDCASE
  \ Maclaurin series with 5 terms 0,2,4,6,8 (cos) or 1,3,5,7,9 (sin)
  9 SWAP DO
    2OVER 2OVER F* \ -- ... term -x*x -x*x*term
    I DUP 1+ *     \ -- ... term -x*x -x*x*term i*(i+1)
    S>F F/         \ -- ... term -x*x -x*x*term/(i*(i+1))
    2SWAP          \ -- ... term -x*x*term/(i*(i+1)) -x*x
  2 +LOOP
  2DROP
  \ sum the 5 terms in reverse order
  F+ F+ F+ F+ ;

: FSIN      ( r1 -- f2 ) TRUE FCOSI ;

: FCOS      ( r1 -- f2 ) FALSE FCOSI ;

\ Slightly less accurate FSIN based on Jupiter ACE "FORTH Programming" p.93
\ improved to cover a wider range of angles and for speed (fewer terms to sum)

\ : FSIN      ( r1 -- r2 )
\   \ map r1 to [0,pi/2) and adjust for quadrant 0, 1, 2, 3
\   PI/2 F/ \ rescale r1 to x in [0,1)
\   2DUP F0< IF FABS 2E0 F+ THEN \ -x=2+x i.e. -r1=pi+r1
\   2DUP F>D \ truncate x
\   OVER >R \ save truncated low order x to test for quadrant
\   D>F F- \  frac(x)=x-trunc(x) i.e. frac(r1/(pi/2))
\   R@ 1 AND IF 1E0 2SWAP F- THEN \ quadrant 1 and 3: 1-x
\   R> 2 AND IF FNEGATE THEN \ quadrant 2 and 3: -x
\   PI/2 F* \ revert rescaling to obtain x in (-pi/2,pi/2]
\   \ Maclaurin series sin(x) = x - x^3/3! + x^5/5! - x^7/7! + ...
\   2DUP 2DUP 2DUP F* FNEGATE 2ROT 2ROT \ -- -x*x x x
\   11 2 DO
\     5 PICK 5 PICK F* \ -- -x*x sum -x*x*term
\     I DUP 1+ *       \ -- -x*x sum -x*x*term i*(i+1)
\     S>F F/           \ -- -x*x sum -x*x*term/i*(i+1)
\     2SWAP 2OVER F+   \ -- -x*x -x*x*term/i*(i+1) sum-x*x*term/i*(i+1)
\     2SWAP            \ -- -x*x sum-x*x*term/i*(i+1) -x*x*term/i*(i+1) 
\   2 +LOOP
\   2DROP 2SWAP 2DROP ;
\ 
\ : FCOS      ( r1 -- r2 ) PI/2 2SWAP F- FSIN ;

: FTAN      ( r1 -- r2 ) 2DUP FSIN 2SWAP FCOS F/ ;

\ Accurate FASIN by summing in reverse order using the stack as temporary storage (34 cells)

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
  2DUP 2DUP 2DUP F* 2SWAP \ -- x x*x x
  34 3 DO
    2OVER F*      \ -- x (1/2)x^3/3 ... x*x term*x*x
    I 2- S>F F*   \ -- x (1/2)x^3/3 ... x*x term*x*x*(i-2)
    I 1- S>F F/   \ -- x (1/2)x^3/3 ... x*x term*x*x*(i-2)/(i-1)
    2DUP I S>F F/ \ -- x (1/2)x^3/3 ... x*x term*x*x*(i-2)/(i-1) term*x*x*(i-2)/(i-1)/i
    2ROT 2ROT     \ -- x (1/2)x^3/3 ... term*x*x*(i-2)/(i-1)/i x*x term*x*x*(i-2)/(i-1)
  2 +LOOP
  2DROP 2DROP
  \ sum the 17 terms in reverse order
  16 0 DO F+ LOOP
  ROT IF PI/2 2SWAP F- ROT IF FNEGATE THEN THEN ; \ sign(x)*(pi/2-arcsin(sqrt(1-x^2)))

\ Slightly less accurate FASIN without temporary storage on the stack

\ : FASIN     ( r1 -- r2 )
\   1E0 2OVER FABS F< IF -46 THROW THEN
\   \ map r1 to [-sqrt(1/2),sqrt(1/2)] using arcsin(x) = pi/2-arcsin(qrt(1-x^2))
\   0.70710678E0 2OVER FABS F< IF \ if |r1| > sqrt(1/2) then
\     2DUP F0<
\     -ROT
\     2DUP F* 1E0 2SWAP F- FSQRT 
\     TRUE
\   ELSE
\     FALSE
\   THEN
\   -ROT
\   \ Maclaurin series arcsin(x) = x + (1/2)x^3/3 + (1*3)/(2*4)x^5/5 + (1*3*5)/(2*4*6)x^7/7 + ...
\   2DUP 2DUP 2DUP F* 2ROT 2ROT \ -- x*x x x
\   34 3 DO
\     5 PICK 5 PICK F*     \ -- x*x sum term*x*x
\     I 2- S>F F*          \ -- x*x sum term*x*x*(i-2)
\     I 1- S>F F/          \ -- x*x sum term*x*x*(i-2)/(i-1)
\     2SWAP 2OVER I S>F F/ \ -- x*x term*x*x*(i-2)/(i-1) sum term*x*x*(i-2)/(i-1)/i
\     F+                   \ -- x*x term*x*x*(i-2)/(i-1) sum+term*x*x*(i-2)/(i-1)/i
\     2SWAP                \ -- x*x sum+term*x*x*(i-2)/(i-1)/i term*x*x*(i-2)/(i-1)
\   2 +LOOP
\   2DROP 2SWAP 2DROP
\   ROT IF PI/2 2SWAP F- ROT IF FNEGATE THEN THEN ; \ sign(x)*(pi/2-arcsin(sqrt(1-x^2)))

: FACOS     ( r1 -- r2 ) FASIN PI/2 2SWAP F- ; \ = pi/2 - arcsin(x)

\ Accurate FATAN by summing in reverse order using the stack as temporary storage (16 cells)

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
  2DUP 2DUP 2DUP F* FNEGATE 2SWAP \ -- x -x*x x
  16 3 DO
    2OVER F*      \ -- x -x^3/3 ... -x*x -x*x*term
    2DUP I S>F F/ \ -- x -x^3/3 ... -x*x -x*x*term -x*x*term/i
    2ROT 2ROT     \ -- x -x^3/3 ... -x*x*term/i -x*x -x*x*term
  2 +LOOP
  2DROP 2DROP     \ -- x -x^3/3 ... x^15/15
  \ sum the 8 terms in reverse order
  F+ F+ F+ F+ F+ F+ F+
  ROT IF 2E0 F* THEN \ 2*arctan(x/(1+sqrt(1+x^2)))
  ROT IF PI/2 2SWAP F- ROT IF FNEGATE THEN THEN ; \ sign(x) * (pi/2-arctan(1/abs(x)))

\ Slightly less accurate FATAN without temporary storage on the stack

\ : FATAN     ( r1 -- r2 )
\   \ map r1 to [-1,1] using arctan(x) = sign(x) * (pi/2-arctan(1/abs(x)))
\   1E0 2OVER FABS F< IF \ if |r1| > 1 then
\     2DUP F0< -ROT
\     1E0 2SWAP FABS F/
\     TRUE
\   ELSE
\     FALSE
\   THEN
\   -ROT
\   \ map r1 in [-1,1] to [-sqrt(2)+1,sqrt(2)-1] using arctan(x) = 2*arctan(x/(1+sqrt(1+x^2)))
\   .41423562E0 2OVER FABS F< IF \ if |r1| > sqrt(2)-1 then
\     2DUP 2DUP F* 1E0 F+ FSQRT 1E0 F+ F/
\     TRUE
\   ELSE
\     FALSE
\   THEN
\   -ROT
\   \ Maclaurin series arctan(x) = x - x^3/3 + x^5/5 - x^7/7 + ... with x in (-1,1)
\   2DUP 2DUP 2DUP F* FNEGATE 2ROT 2ROT \ -- -x*x x x
\   16 3 DO
\     5 PICK 5 PICK F* \ -- -x*x sum -x*x*term
\     2DUP I S>F F/    \ -- -x*x sum -x*x*term -x*x*term/i
\     2ROT F+          \ -- -x*x -x*x*term/i sum-x*x*term/i
\     2SWAP            \ -- -x*x sum-x*x*term/i -x*x*term/i
\   2 +LOOP
\   2DROP 2SWAP 2DROP
\   ROT IF 2E0 F* THEN \ 2*arctan(x/(1+sqrt(1+x^2)))
\   ROT IF PI/2 2SWAP F- ROT IF FNEGATE THEN THEN ; \ sign(x) * (pi/2-arctan(1/abs(x)))

: FATAN2    ( r1 r2 -- r3 )
  2DUP FNEGATE F0< IF
    F/ FATAN
  ELSE
    2SWAP
    2DUP F0= IF
      2DROP F0< IF PI ELSE PI/2 THEN
    ELSE
      PI/2 2OVER F0< IF FNEGATE THEN
      2ROT 2ROT F/ FATAN F-
    THEN
  THEN ;

\ Correctly rounded (*) FLN by summing in reverse order using the stack as temporary storage (42 cells)
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
    2OVER F*      \ -- x x^2/2 ... x term*x
    2DUP I S>F F/ \ -- x x^2/2 ... x term*x term*x/i
    2ROT 2ROT     \ -- x x^2/2 ... term*x/i x term*x
  LOOP
  2DROP 2DROP     \ -- x x^2/2 ... x^19/19
  \ sum the 21 terms in reverse order
  20 0 DO F+ LOOP
  FNEGATE
  ROT S>F LN2 F* F+ ; \ + n*ln(2) with approx ln(2) such that 1E0 FLN = 0

\ Slightly less accurate FLN without temporary storage on the stack

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

\ Correctly rounded exp(x) by summing in reverse order using the stack as temporary storage (20 cells)

: FEXP      ( r1 -- r2 )
  2DUP F0< -ROT
  FABS
  \ map |r1| to [0,ln(2)) using exp(x+k*ln(2)) = exp(x)*2^k
  2DUP LN2 F/ F>S DUP>R
  S>F LN2 F* F-
  \ Maclaurin series exp(x) = 1 + x + x^2/2! + x^3/3! + ...
  1E0 2SWAP 2DUP \ -- 1 x x
  10 2 DO
    2OVER 2OVER F* \ -- 1 x x^2/2! ... term x term*x
    I S>F F/       \ -- 1 x x^2/2! ... term x term*x/i
    2SWAP          \ -- 1 x x^2/2! ... term term*x/i x
  LOOP
  2DROP \ -- 1 x x^2/2! ... x^9/9!
  \ sum the 10 terms in reverse order
  9 0 DO F+ LOOP
  \ multiply exp(x) by 2^k
  R> 7 LSHIFT +
  \ return reciprocal for negative r1
  ROT IF
    1E0 2SWAP F/
  THEN ;

\ Slightly less accurate FEXP without temporary storage on the stack

\ : FEXP      ( r1 -- r2 )
\   2DUP F0< -ROT
\   FABS
\   \ map |r1| to [0,ln(2)) using exp(x+k*ln(2)) = exp(x)*2^k
\   2DUP 0.693147245E0 F/ F>S DUP>R
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

: FALOG     ( r1 -- r2 ) LN10 F* FEXP ; \ = exp(x*ln(10))

\ Exponentiation, simple version requires r1 > 0

: F^        ( r1 r2 -- r3 ) 2SWAP FLN F* FEXP ;

\ A complete F**, uses exponentiation by squaring when r2 is a small integer

: F**       ( r1 r2 -- r3 )
  2DUP F0= IF \ r2 = 0
    2OVER F0= IF -46 THROW THEN \ error if r1 = 0 and r2 = 0
    2DROP 2DROP 1E0 EXIT \ return 1.0
  THEN
  2OVER F0= IF \ r1 = 0
    2DUP F0< IF -42 THROW THEN \ error if r1 = 0 and r2 < 0
    2DROP 2DROP 0E0 EXIT \ return 0.0
  THEN
  \ exponentiation by squaring r1^n when n is a small integer |n|<=16
  2DUP 2DUP FTRUNC F= IF \ r2 has no fractional part
    2DUP ['] F>D CATCH 0= IF \ r2 is convertable to a double n
      2DUP DABS 17. DU< IF \ |n| <= 16
        DROP \ drop high order of n
        DUP 0< >R \ save sign of n
        ABS >R \ save |n|
        2DROP \ drop old r2
        1E0 \ -- r1 1.0
        BEGIN
          R@ 1 AND IF 2OVER F* THEN
          R> 1 RSHIFT \ -- r1^n product u>>1
        DUP WHILE
          >R
          2SWAP 2DUP F* 2SWAP \ -- r1^n^2 product u>>1
        REPEAT
        DROP 2SWAP 2DROP \ -- product
        R> IF 1E0 2SWAP F/ THEN \ reciprocal when exponent was negative
        EXIT
      THEN
      OVER 1 AND IF \ n is odd
        2OVER F0< IF \ r1 is negative
          2DROP 2SWAP FABS 2SWAP F^ FNEGATE EXIT \ return -(|r1|^n)
        THEN
      THEN
      2DROP 2SWAP FABS 2SWAP \ we want to return |r1|^r2
    ELSE
      2DROP \ drop copy of r2
    THEN
  THEN
  F^ ;

\ Hyperbolics

: FCOSH     ( r1 -- r2 ) FEXP 2DUP 1E0 2SWAP F/ F+ 2E0 F/ ;

: FSINH     ( r1 -- r2 ) FEXP 2DUP 1E0 2SWAP F/ F- 2E0 F/ ;

: FTANH     ( r1 -- r2 ) 2DUP F+ FEXP 2DUP 1E0 F- 2SWAP 1E0 F+ F/ ;

: FACOSH    ( r1 -- r2 ) 2DUP 2DUP F* 1E0 F- FSQRT F+ FLN ;

: FASINH    ( r1 -- r2 ) 2DUP 2DUP F* 1E0 F+ FSQRT F+ FLN ;

: FATANH    ( r1 -- r2 ) 2DUP 1E0 F+ 2SWAP 1E0 2SWAP F- F/ FLN 2E0 F/ ;
