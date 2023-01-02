\ BREAK.FTH turn BREAK key on or off
\ ONLY WORKS WITH Forth859 v1, OTHER Forth850 VERSIONS MAY DAMAGE THE DICTIONARY!

\ BREAK-ON restores instruction '016D: add a'
: BREAK-ON	( -- ) $87 $016D C! ;

\ BREAK-OFF modify instruction '016D: or a' which is essentially a nop
: BREAK-OFF	( -- ) $B7 $016D C! ;
