\ SINE.FTH plot a sine curve
\ requires MATH.FTH FSIN
\ requires POINT.FTH POINT

  3.14159265E0 2CONSTANT PI

  PI 72E0 F/ 2VALUE SCALE \ free to change e.g.: PI 144E0 F/ TO SCALE

: SINE
  PAGE
  144 0 DO
    I DUP S>D D>F SCALE F*
    FSIN -24E0 F*
    F>D D>S 24 + POINT
  LOOP
  GETKEY DROP ;
