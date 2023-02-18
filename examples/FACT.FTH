\ FACT.FTH factorial

ANEW _FACT

DECIMAL

\ Factorial of an integer returns float

: FACT      ( u -- r )
  1E0 ROT 1+ 1 ?DO
    I S>F F*
  LOOP ;
