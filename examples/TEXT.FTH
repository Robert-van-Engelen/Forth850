\ read Forth source code from the TEXT editor area
\ - you can write Forth code with line numbers in the TEXT editor
\ - in Forth enter TEXT to read the source code
\ note: .( and ( cannot span multiple lines, i.e. a ) must be on the same line

\ start of TEXT area [$7973] end [$7975]
\ $FF marker at TEXT start and end
\ BASIC/TEXT line format:
\   hi lo len data... $0D
\ where
\   hi is high order byte of line number, hi is $FF at end
\   lo is low order byte  of line number
\   len is length of the line up to and including the $0D byte

: TEXT
  $7973 @ 1+ >R
  BEGIN
    R>                  \ -- addr
  DUP C@ $FF <> WHILE
    2+ DUP C@ SWAP 1+   \ -- len addr
    2DUP + >R
    SWAP 1- EVALUATE
  REPEAT
  DROP ;
