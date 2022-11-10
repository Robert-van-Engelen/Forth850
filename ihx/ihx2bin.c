// ihx2bin for SHARP pocket computers by Robert van Engelen
// BSD-3 license, last updated May 10, 2021
// build: cc -o ihx2bin ihx2bin.c
// usage: ihx2bin [file.ihx|-] [file.bin]
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

// read next byte from ihx file, return EOF on error or end of file
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

// convert ihx file to bin file
int ihx2bin(FILE *ihx, FILE *bin) {
  int byte;
  while ((byte = getbyte(ihx)) != EOF && byte != ERR) {
    char ch[1] = { byte };
    if (fwrite(ch, 1, 1, bin) != 1) {
      perror("cannot write to bin file");
      return 1;
    }
  }
  return byte == ERR;
}

// usage: ihx2bin [file.ihx|-] [file.bin]
int main(int argc, char **argv) {
  const char *ihxfile = NULL;
  FILE *ihx = stdin, *bin = stdout;
  if (argc > 1) {
    if (strcmp(argv[1], "-") != 0)
      ihx = fopen(ihxfile = argv[1], "r");
    if (ihx == NULL) {
      perror(argv[EXIT_FAILURE]);
      exit(EXIT_FAILURE);
    }
    if (argc > 2) {
      bin = fopen(argv[2], "w");
      if (bin == NULL) {
	perror(argv[2]);
	exit(EXIT_FAILURE);
      }
    }
    else if (ihxfile != NULL) {
      char *binfile = (char*)malloc(strlen(ihxfile) + 5);
      char *dot;
      strcpy(binfile, ihxfile);
      dot = strrchr(binfile, '.');
      if (dot != NULL)
        strcpy(dot, ".bin");
      else
        strcat(binfile, ".bin");
      bin = fopen(binfile, "w");
      if (bin == NULL) {
	perror(binfile);
	exit(EXIT_FAILURE);
      }
    }
  }
  ihxlinelen = 0;
  ihxlinepos = 0;
  ihxlinenum = 0;
  ihxaddress = 0;
  return ihx2bin(ihx, bin);
}
