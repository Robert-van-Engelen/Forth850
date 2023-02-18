\ TEXT.FTH read Forth source code from the TEXT editor area
\ The TEXT word is built-in the "full version" of Forth850

\ - you can write Forth code with line numbers in the TEXT editor
\ - in Forth enter TEXT to read the source code
\ Note: .( and ( cannot span multiple lines, i.e. a ) must be on the same line

\ Hint: to read Forth source code and overwrite the old version of the code,
\ use the ANEW word with a name to give to your program or module.  The ANEW
\ word creates a MARKER.  For example, loading and reloading the following
\ program written in the TEXT editor replaces the old rather than adding
\ multiple inaccessible versions to the dictionary:

\ 10 ANEW _myprogram_
\ 20 .( Loading my program...)
\ 30 : square ( n1 -- n2 )
\ 40   DUP * ;

\ Start of TEXT area pointer [$7973/4] end pointer [$7975/6]
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
