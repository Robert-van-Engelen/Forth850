\ set a point at xy on the display

\ When using POINT interactively, use CR to advance the cursor
\ Otherwise the pixel is overwritten by a space

: POINT         ( x y -- )
  DUP 3 RSHIFT  \ -- x y y/8
  1 ROT 7 AND   \ -- x y/8 1 y%8
  LSHIFT        \ -- x y/8 pix
  -ROT SWAP     \ -- patt y/8 x
  0 6 UM/MOD    \ -- patt y/8 x%6 x/6
  ROT           \ -- patt x%6 x/6 y/8
  AT-XY         \ -- patt x%6
  PAD 6 VIEW    \ -- patt x%6
  PAD +         \ -- patt c-addr
  DUP C@        \ -- patt c-addr patt'
  ROT OR SWAP C!
  PAD 6 DRAW ;
