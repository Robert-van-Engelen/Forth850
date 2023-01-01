# tools

ASM=./asz80
ASMFLAGS=-lops

LNK=./aslink
LNKFLAGS=-imwu

IHX2BIN=ihx/ihx2bin

RM=rm -f

# files

WAV=forth850.wav
BIN=forth850.bin
HEX=forth850.ihx
REL=forth850.rel

# rules

all:	$(WAV)

$(WAV):	$(BIN)
	bin2wav --pc=G850 --type=bin --addr=0x0100 $(BIN)

$(BIN): $(HEX)
	$(IHX2BIN) $(HEX) $(BIN)

$(HEX): $(REL)
	$(LNK) $(LNKFLAGS) $(REL)

%.rel: %.asm math.asm mathr.asm
	$(ASM) $(ASMFLAGS) $<

clean:
	$(RM) *.rel *.sym *.hlr *.map *.lst *.rst

distlean:
	$(RM) *.rel *.sym *.hlr *.map *.lst *.rst $(HEX) $(BIN) $(WAV)

.PHONY: all clean distclean
