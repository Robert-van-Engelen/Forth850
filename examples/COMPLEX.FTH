\ COMPLEX.FTH complex arithmetic
\ A subset of functions based on Forth Scientific Library Algorithm #60
\ Requires MATH.FTH FSQRT for |Z|
\ Requires MATH.FTH FATAN2 (FATAN) for ZARG

\ Note: floats are double cells, so fdup is 2DUP etc.
\ Note: -frot is 2ROT 2ROT
\ Note: complex numbers are pairs of two floats r (real) and i (imag)
\       zdup is 2OVER 2OVER
\       zdrop is 2DROP 2DROP

ANEW _COMPLEX_

DECIMAL

\ Temporaries
0E0 2VALUE FTMP1
0E0 2VALUE FTMP2

\ Negation
: ZNEGATE       ( r1 i1 -- r2 i2 ) 2SWAP FNEGATE 2SWAP FNEGATE ;

\ Addition
: Z+    ( r1 i1 r2 i2 -- r3 i3 ) 2ROT F+ 2ROT 2ROT F+ 2SWAP ;

\ Subtraction, Kahan: to conserve signed zero, write -y+b for b-y instead of -[y-b]
: Z-    ( r1 i1 r2 i2 -- r3 i3 ) FNEGATE 2ROT F+ 2ROT 2ROT F- 2SWAP ;

\ Multiplication by a float
: Z*F   ( r1 i1 r2 -- r3 i3 ) 2ROT 2OVER F* 2ROT 2ROT F* ;

\ Multiplication
: Z*    ( r1 i1 r2 i2 -- r3 i3 )
  2OVER 2OVER F+ TO FTMP1
  2OVER F- TO FTMP2
  2ROT 2DUP FTMP2 F* TO FTMP2
  2ROT 2DUP FTMP1 F* TO FTMP1
  F+ F* 2DUP FTMP1 F-
  2SWAP FTMP2 F+ ;

\ Absolute value, preserves precision (versus naive distance), requires MATH.FTH FSQRT
: |Z|   ( r1 i1 -- r2 )
  FABS 2SWAP FABS
  2OVER 2OVER FMAX
  2DUP F0= IF
    2DROP 2DROP 2DROP 0E0
  ELSE
    2ROT 2ROT FMIN
    2OVER F/ 2DUP F* 1E0 F+ FSQRT F*
  THEN ;

\ Principal argument, requires MATH.FTH FATAN2
: ZARG  ( r1 i1 -- r2 ) 2SWAP FATAN2 ;

\ Division by a float
: Z/F   ( r1 i1 r2 -- r3 i3 ) 1E0 2SWAP F/ Z*F ;

\ Reciprocal
: 1/Z   ( r1 i1 -- r2 i2 )
  FNEGATE 2OVER 2OVER |Z| 1E0 2SWAP F/ 2DUP TO FTMP1
  Z*F FTMP1 Z*F ;

\ Division
: Z/    ( r1 i1 r2 i2 -- r3 i3 ) 1/Z Z* ;

\ Square
: Z^2   ( r1 i1 -- r2 i2 ) 2OVER 2OVER Z* ;

\ Display a complex number
: Z.    ( r1 i1 -- r2 i2 )
  2SWAP F.
  2DUP F0< INVERT IF '+ EMIT THEN
  F. 'i EMIT ;
