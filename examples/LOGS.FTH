\ LOGS.FTH defines LN2, LN10, FSQRT, FLN, FLOG, FEXP, FALOG, F^, F**, FCOSH, FSINH, FTANH, FACOSH, FASINH, FATANH
\ See MATH.FTH for a detailed implementation

ANEW _LOGS_

DECIMAL

  .69314724E0 2CONSTANT LN2
  2.3025853E0 2CONSTANT LN10

: FSQRT     ( r1 -- r2 )
  2DUP F0< IF -46 THROW THEN
  2DUP F0= IF EXIT THEN
  DUP 8 RSHIFT $3f - -ROT
  $ff AND $3f00 +
  2DUP
  5 0 DO
    2OVER 2OVER F/ F+ .5E0 F*
  LOOP
  2SWAP 2DROP
  ROT $7f + 7 LSHIFT 0 SWAP F* ;

: FLN       ( r1 -- r2 )
  2DUP F0< IF -46 THROW THEN
  2DUP F0= IF -46 THROW THEN
  DUP 7 RSHIFT $7e - -ROT
  $7f AND $3f00 +
  1E0 2SWAP F-
  2DUP 2DUP
  22 2 DO
    2OVER F*
    2DUP I S>F F/
    2ROT 2ROT
  LOOP
  2DROP 2DROP
  20 0 DO F+ LOOP
  FNEGATE
  ROT S>F LN2 F* F+ ;

: FLOG      ( r1 -- r2 ) FLN 0.4342945E0 F* ;

: FEXP      ( r1 -- r2 )
  2DUP F0< -ROT
  FABS
  2DUP LN2 F/ F>S
  DUP $7f > IF -43 THROW THEN
  DUP>R
  S>F LN2 F* F-
  2DUP
  10 2 DO
    2OVER 2OVER F*
    I S>F F/
    2SWAP
  LOOP
  2DROP
  F+ F+ F+ F+ F+ F+ F+ F+
  1E0 F+
  R> 7 LSHIFT +
  ROT IF
    1E0 2SWAP F/
  THEN ;

: FALOG     ( r1 -- r2 ) LN10 F* FEXP ;

: F^        ( r1 r2 -- r3 ) 2SWAP FLN F* FEXP ;

: F**       ( r1 r2 -- r3 )
  2DUP F0= IF
    2OVER F0= IF -46 THROW THEN
    2DROP 2DROP 1E0 EXIT
  THEN
  2OVER F0= IF
    2DUP F0< IF -42 THROW THEN
    2DROP 2DROP 0E0 EXIT
  THEN
  2DUP 2DUP FTRUNC F= IF
    2DUP ['] F>D CATCH 0= IF
      2DUP DABS 17. DU< IF
        DROP
        DUP 0< >R
        ABS >R
        2DROP
        1E0
        BEGIN
          R@ 1 AND IF 2OVER F* THEN
          R> 1 RSHIFT
        DUP WHILE
          >R
          2SWAP 2DUP F* 2SWAP
        REPEAT
        DROP 2SWAP 2DROP
        R> IF 1E0 2SWAP F/ THEN
        EXIT
      THEN
      OVER 1 AND IF
        2OVER F0< IF
          2DROP 2SWAP FABS 2SWAP F^ FNEGATE EXIT
        THEN
      THEN
      2DROP 2SWAP FABS 2SWAP
    ELSE
      2DROP
    THEN
  THEN
  F^ ;

: FCOSH     ( r1 -- r2 ) FEXP 2DUP 1E0 2SWAP F/ F+ 2E0 F/ ;

: FSINH     ( r1 -- r2 ) FEXP 2DUP 1E0 2SWAP F/ F- 2E0 F/ ;

: FTANH     ( r1 -- r2 ) 2DUP F+ FEXP 2DUP 1E0 F- 2SWAP 1E0 F+ F/ ;

: FACOSH    ( r1 -- r2 ) 2DUP 2DUP F* 1E0 F- FSQRT F+ FLN ;

: FASINH    ( r1 -- r2 ) 2DUP 2DUP F* 1E0 F+ FSQRT F+ FLN ;

: FATANH    ( r1 -- r2 ) 2DUP 1E0 F+ 2SWAP 1E0 2SWAP F- F/ FLN 2E0 F/ ;
