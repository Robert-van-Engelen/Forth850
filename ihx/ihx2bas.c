// ihx2bas for SHARP pocket computers by Robert van Engelen
// BSD-3 license, last updated May 10, 2021
// build: cc -o ihx2bas ihx2bas.c
// usage: ihx2bas [file.ihx|-] [file.bas]
// no gaps allowed in ihx, helps to catch assembly/link errors

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define BUF_SIZE 1024
#define ERR -999

static char ihxbuffer[BUF_SIZE];
static unsigned int ihxaddress;
static unsigned int ihxlinelen;
static unsigned int ihxlinepos;
static unsigned int baslinenum;
static unsigned int baslineinc;
static unsigned int ihxlinenum;

// convert char to 4-bit nibble
unsigned int tonibble(unsigned char ch)
{
  return ch < 0x3a ? ch - 0x30 : ch - 0x37;
}

// convert char to 8-bit byte
unsigned int tobyte(unsigned char ch1, unsigned char ch2)
{
  return tonibble(ch1) << 4 | tonibble(ch2);
}

// convert char to 16-bit word (big endian)
unsigned int toword(unsigned char ch1, unsigned char ch2, unsigned char ch3, unsigned char ch4)
{
  return tobyte(ch1, ch2) << 8 | tobyte(ch3, ch4);
}

// read next byte from ihx file, return EOF on end of file and ERR on error
int getbyte(FILE *ihx) {
  unsigned int address, byte;
  while (ihxlinepos >= ihxlinelen) {
    if (fgets(ihxbuffer, BUF_SIZE-1, ihx) == NULL)
      return EOF;
    ++ihxlinenum;
    if (ihxbuffer[0] != ':')
      continue;
    if (strlen(ihxbuffer) < 11) {
      fprintf(stderr, "line %d too short: %s\n", ihxlinenum, ihxbuffer);
      return ERR;
    }
    ihxlinepos = 0;
    ihxlinelen = tobyte(ihxbuffer[1], ihxbuffer[2]);
    if (strlen(ihxbuffer) < 2*ihxlinelen + 11) {
      fprintf(stderr, "line %d size error: %s\n", ihxlinenum, ihxbuffer);
      return ERR;
    }
    if (ihxlinelen == 0)
      continue;
    address = toword(ihxbuffer[3], ihxbuffer[4], ihxbuffer[5], ihxbuffer[6]);
    if (ihxaddress == 0) {
      ihxaddress = address;
    }
    else if (ihxaddress != address) {
      fprintf(stderr, "line %d address discontinuity %x but %x expected: %s\n", ihxlinenum, address, ihxaddress, ihxbuffer);
      return ERR;
    }
  }
  byte = tobyte(ihxbuffer[2*ihxlinepos+9], ihxbuffer[2*ihxlinepos+10]);
  ++ihxlinepos;
  ++ihxaddress;
  return byte;
}

// convert ihx file to BASIC bootloader file
int ihx2bas(const char *ihxfile, FILE *ihx, FILE *bas) {
  int columns = 0;
  int address = 0;
  int length  = 0;
  int chksum  = 0;
  int byte    = 0;
  byte = getbyte(ihx);
  if (byte == ERR)
    return 1;
  if (byte != EOF)
    address = ihxaddress - 1;
  fprintf(bas, "%d CLEAR:V=1:REM %s\n", baslinenum, ihxfile != NULL ? ihxfile : "ihx");
  baslinenum += baslineinc;
  fprintf(bas, "%d \"P\" A=%d\n", baslinenum, address);
  baslinenum += baslineinc;
  while (byte != EOF && byte != ERR) {
    if (columns == 0) {
      fprintf(bas, "%d POKE A", baslinenum);
      if (ihxaddress > address + 1)
        fprintf(bas, "+%d", ihxaddress - address - 1);
      baslinenum += baslineinc;
    }
    fprintf(bas, ",%d", byte);
    chksum += byte;
    ++columns;
    if (columns >= 16) {
      fprintf(bas, "\n");
      columns = 0;
    }
    byte = getbyte(ihx);
  }
  if (columns != 0)
    fprintf(bas, "\n");
  if (byte == ERR)
    return 1;
  length = ihxaddress - address;
  fprintf(bas, "%d IF V LET C=0:FOR I=0 TO %d:C=C+PEEK(A+I):NEXT I:IF C<>%d PRINT \"ERR\":END\n", baslinenum, length - 1, chksum);
  baslinenum += baslineinc;
  return 0;
}

// usage: ihx2bas [file.ihx|-] [file.bas]
int main(int argc, char **argv) {
  const char *ihxfile = NULL;
  FILE *ihx = stdin, *bas = stdout;
  if (argc > 1) {
    if (strcmp(argv[1], "-") != 0)
      ihx = fopen(ihxfile = argv[1], "r");
    if (ihx == NULL) {
      perror(argv[EXIT_FAILURE]);
      exit(EXIT_FAILURE);
    }
    if (argc > 2) {
      bas = fopen(argv[2], "w");
      if (bas == NULL) {
	perror(argv[2]);
	exit(EXIT_FAILURE);
      }
    }
    else if (ihxfile != NULL) {
      char *basfile = (char*)malloc(strlen(ihxfile) + 5);
      char *dot;
      strcpy(basfile, ihxfile);
      dot = strrchr(basfile, '.');
      if (dot != NULL)
        strcpy(dot, ".bas");
      else
        strcat(basfile, ".bas");
      bas = fopen(basfile, "w");
      if (bas == NULL) {
	perror(basfile);
	exit(EXIT_FAILURE);
      }
    }
  }
  baslinenum = 100;
  baslineinc = 1;
  ihxlinelen = 0;
  ihxlinepos = 0;
  ihxlinenum = 0;
  ihxaddress = 0;
  return ihx2bas(ihxfile, ihx, bas);
}
