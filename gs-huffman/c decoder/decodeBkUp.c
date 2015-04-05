// The following program dumps text from the original ROM.

#include <stdio.h>
#include <stdlib.h>

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;

typedef signed char s8;
typedef signed short s16;
typedef signed int s32;

#define READU8(x) \
  *((u8 *)(((x) & 0x007FFFFF) + rom))
#define READU16(x) \
  *((u16 *)(((x) & 0x007FFFFF) + rom))
#define READU32(x) \
  *((u32 *)(((x) & 0x007FFFFF) + rom))
#define WRITEU8(x,v) \
  *((u8 *)(((x) & 0x007FFFFF) + rom)) = (v)
#define WRITEU16(x,v) \
  *((u16 *)(((x) & 0x007FFFFF) + rom)) = (v)
#define WRITEU32(x,v) \
  *((u32 *)(((x) & 0x007FFFFF) + rom)) = (v)

#define BIT(x) \
  (1 << (x))

#define CBIT(x) \
  (C << (x))

u16 GetNextChararacter(u16 * last, u32 * src, u32 * flags);
void SetInitials(int id, u16 * last, u32 * rptrSrc, u32 * flags);

char *rom;
FILE *fRom;
int count = 0;

u32 gsrc, gflags;
u16 glast;

char C, C_;
u32 iterPtr, srcPtr, srcBits, iterBits;

//2 helper functions to check bit and load new word if neccessary
void checkSrcBit()
{

	C = srcBits & 1;	//check next bit in source                                     
	srcBits = srcBits >> 1;
	if (srcBits == 0) {	//load new word at source bitstream                         
		srcBits = READU32(srcPtr);
		srcPtr += 4;
		C = srcBits & 1;
		srcBits = srcBits >> 1 | BIT(31);	//carry set, bcc above would skip otherwise   
	}
}

void checkIterBit()
{
	C = iterBits & 1;
	iterBits = iterBits >> 1;	//check next bit in iterbitstream  

	if (iterBits == 0) {
		iterBits = READU32(iterPtr);
		iterPtr += 4;
		C = iterBits & 1;
		iterBits = iterBits >> 1 | (BIT(31));
	}
}

int main(int argc, char *argv[])
{
	int len;
	int i;

	u16 t;

	char buffer[1024];
	char *pBuffer;

	fRom = fopen("0171 - Golden Sun (UE).gba", "rb");
	if (!fRom) {
		printf("Failed to open ROM");
		exit(-1);
	}
	fseek(fRom, 0, SEEK_END);
	len = ftell(fRom);

	rom = (char *)malloc(len);
	if (!rom) {
		printf("Failed to allocate memory for ROM.");
		exit(-1);
	}

	rewind(fRom);

	fread(rom, 1, len, fRom);
	fclose(fRom);

	for (i = 0; i < 0x29e2; i++) {

		count = 0;
		SetInitials(i, &glast, &gsrc, &gflags);
		//printf("%04X %08X %08X ", i, gsrc, gflags);
		printf("#%04X ", i);
		t = 0xff;
		pBuffer = buffer;
		while (t != 0) {
			t = GetNextChararacter(&glast, &gsrc, &gflags);
			if (t >= 0x20) {
				sprintf(pBuffer, "%c", t);
				pBuffer++;
			} else {
				sprintf(pBuffer, "{%02X}", t);
				pBuffer += 4;
			}

		}
		pBuffer[0] = 0;
		printf("%s\n", buffer);
	}

	return 0;
}

u16 GetNextChararacter(u16 * last, u32 * src, u32 * flags)
{

	u32 lastChar, nextCharIndex = 0, padBitCount, iterTreeStart;
	int balance;
	u8 nextChar;
	u32 entryAddress;

	srcPtr = *src;
	srcBits = *flags;
	lastChar = *last;

	iterPtr = READU32(0x0803842C);
	iterTreeStart = READU32(0x08038430);

	iterPtr += READU16(iterTreeStart + lastChar * 2);
	iterTreeStart = iterPtr;	//currently pointer is to start of tree
	iterBits = 1;

	padBitCount = iterPtr & 3;

	if (padBitCount != 0) {	//align, load word of iterBits and shift out unused bits

		iterPtr &= (~3);
		iterBits = READU32(iterPtr);	//load word
		iterPtr += 4;
		iterBits = (iterBits >> 1) | BIT(31);	//rize hi bit to detect if word is shifted-out
		iterBits = iterBits >> (padBitCount * 8 - 1);	//shift out unused bits 
	}

	for (checkIterBit(); C == 0; checkIterBit()) {

		checkSrcBit();
		if (C != 0) {

			for (balance = 0; balance >= 0;) {
				checkIterBit();
				if (C != 0) {
					nextCharIndex++;
					balance--;
					continue;
				}

				checkIterBit();
				if (C != 0) {
					nextCharIndex++;
					continue;
				}

				checkIterBit();
				if (C != 0) {
					balance++;
					nextCharIndex++;
					continue;
				}

				checkIterBit();
				if (C != 0) {
					balance += 2;
					nextCharIndex++;
					continue;
				};

				balance += 4;
			}
		}
	}

	//emit symbol
        printf("got index %04X \n", nextCharIndex);
	entryAddress = iterTreeStart - (nextCharIndex + (nextCharIndex >> 1));	// offset-back before iteration tree and each entry is 1,5 bytes long                                                                                                                                                    
	u8 b1 = READU8(entryAddress - 1);
	u8 b2 = READU8(entryAddress - 2);
	if (nextCharIndex % 2)
		nextChar = b2;	//depending on odd/even position, we use different nybbles of 1,5 bytes scheme                            
	else
		nextChar = (b1 << 4) | b2 >> 4;

	*last = nextChar;
	*src = srcPtr;
	*flags = srcBits;
	return nextChar;

}

void SetInitials(int id, u16 * last, u32 * rptrSrc, u32 * flags)
{
	u32 rptrStringLengthTable;

	u8 len, shift;

	*flags = 1;

	*rptrSrc = READU32(0x080736B8 + (id >> 8) * 8);
	rptrStringLengthTable = READU32(0x080736B8 + (id >> 8) * 8 + 4);

	while ((id & 0xFF) != 0) {
		len = READU8(rptrStringLengthTable++);
		*rptrSrc += len;
		if (len != 0xFF)
			id--;
	}

	if ((*rptrSrc & 3) != 0)	//stringPointer not word-aligned
	{
		shift = ((*rptrSrc & 3) * 8) - 1;
		*rptrSrc &= (~3);

		*flags = (READU32(*rptrSrc) >> 1) | BIT(31);
		*flags = *flags >> shift;

		*rptrSrc += 4;
	}

	*last = 0;
}
