\ set a point at xy on the display using LCD control via ports 40h and 41h

\ Note that LCD scroll offset may not be zero, which makes the screen scroll
\ up or down when plotting points when text is also output

\ A machine language example

\ A way to specify code is in hex, like in the example further below
\ Use NFA, to create words with Z80 machine code compiled with C, (and ,)
\ NFA, adds a named dictionary entry without a code field
\ The code field is where the code goes
\ Machine code should terminate with a jump to the "next routine" with jp (iy)

\ The built-in assembler can be used as follows:
\   1. define the word in Forth850 with NFA, <name>
\   2. HERE HEX . displays the ORG xxxx address to specify in the assembly
\   3. write the assembly in TEXT editor with this ORG address xxxxH
\   4. run the assembler, which adds the code to <name> in Forth850
\   5. write down the number of bytes assembled, this is important
\   6. in Forth850, DECIMAL <number> ALLOT makes the code part of <name>
\      so that HERE HEX . is the next address after the code
\      (use negative -nnnn ALLOT to undo ALLOT when making code changes)
\   7. IMPORTANT: rerun the assembler, because the first assembly was
\      overwritten by Forth850 (the hold area is HERE) before you ALLOTed

\ As always when writing assembly, if something is wrong with it then we may
\ crash and have to start over.  In the worst case we have to install Forth850
\ again when the dictionary is damaged by "random POKEs".  It is often not
\ necessary to reset the machine when asked for MEMORY CLEAR (Y/N) say N (NO).

\ Important points to remember when writing machine code:
\ - register BC holds the IP and should not be modified
\ - register IY holds the address of "next" and should not be modified
\ - register DE holds the TOS and can be used and set
\ - the stack can be used to push/pop Forth parameters
\ - return from the code with JP (iy), never use RET

NFA, POINT      ( x y -- )
  HEX
  E1 C,         \       pop hl          ; x->hl, de=y
  F3 C,         \       di              ;
  F3 C,         \       di              ; disable interrupts
  7D C,         \       ld a,l          ;
  E6 C, 0F C,   \       and 0fh         ;
  D3 C, 40 C,   \       out (40h),a     ; out lower nibble of coordinate x
  7D C,         \       ld a,l          ;
  0F C,         \       rrca            ;
  0F C,         \       rrca            ;
  0F C,         \       rrca            ;
  0F C,         \       rrca            ;
  E6 C, 0F C,   \       and 0fh         ;
  F6 C, 10 C,   \       or 10h          ;
  D3 C, 40 C,   \       out (40h),a     ; out upper nibble of coordinate x
  3E C, 40 C,   \       ld a,40h        ;
  D3 C, 40 C,   \       out (40h),a     ; zero y offset
  7B C,         \       ld a,e          ;
  0F C,         \       rrca            ;
  0F C,         \       rrca            ;
  0F C,         \       rrca            ;
  E6 C, 07 C,   \       and 07h         ;
  F6 C, B0 C,   \       or 0b0h         ;
  D3 C, 40 C,   \       out (40h),a     ; 0b0h+y/8->(40h) coordinate
  3E C, E0 C,   \       ld a,0e0h       ;
  D3 C, 40 C,   \       out (40h),a     ; disable move on read mode
  7B C,         \       ld a,e          ;
  E6 C, 07 C,   \       and 07h         ;
  5F C,         \       ld e,a          ;
  1C C,         \       inc e           ; (x%8)+1->e
  AF C,         \       xor a           ; 0->a
  37 C,         \       scf             ; 1->cf
  17 C,         \shlpt: rla             ; loop
  1D C,         \       dec e           ;   a<<1+cf->a
  20 C, FC C,   \       jr nz,shlpt     ; until --e=0
  5F C,         \       ld e,a          ;
  DB C, 41 C,   \       in a,(41h)      ; dummy read (backs up x?)
  DB C, 41 C,   \       in a,(41h)      ;
  B3 C,         \       or e            ; a|(41h)->a
  D3 C, 41 C,   \       out (41h),a     ; a->(41h)
  3E C, EE C,   \       ld a,0eeh       ; enable move on read mode
  D3 C, 40 C,   \       out (40h),a
  FB C,         \       ei              ; enable interrupts
  D1 C,         \       pop de          ; set new TOS
  FD C, E9 C,   \       jp (iy)         ; continue
  DECIMAL       \ 64 bytes
