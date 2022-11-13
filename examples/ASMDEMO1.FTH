\ A machine language example to beep the speaker at about 2KHz

\ A way to specify code is in hex, like in the example further below
\ Use NFA, to create words with Z80 machine code compiled with C, (and also ,)
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

NFA, BEEP       ( -- )
  HEX
  F3 C,         \       di              ;
  F3 C,         \       di              ; disable interrupts
  21 C, 0 ,     \       ld hl,0000h     ;
  AF C,         \       xor a           ;
  D3 C, 18 C,   \ loop: out (18h),a     ; loop, out audio port
  D2 C,         \ wait: dec l           ;   loop
  20 C, FD C,   \       jr nz,wait      ;   until --l=0
  2F C,         \       cpl             ; switch on/off
  25 C,         \       dec h           ;
  20 C, F7 C,   \       jr nz,loop      ; until --h=0
  FB C,         \       ei              ; enable interrupts
  FD C, E9 C,   \       jp (iy)         ; nextxxxxxxxxxxxxxxxxxxxxxx
  DECIMAL       \ 18 bytes



