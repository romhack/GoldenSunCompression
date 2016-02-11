echo off
::-d to decode from ROM, -e to encode and paste to ROM in the same offset, comparing original encoded size
set romName="0171 - Golden Sun (UE).gba"
set encodedGfxSize=0xEF
set changedName=gfxChanged.bin
set gfxOffset=0xF38BC

IF "%1"=="-d" (
	echo Decoding at %gfxOffset%...
	gsLzssByteFlags -d %romName% %gfxOffset%
	pause
) ELSE ( IF "%1"=="-e" (
	:loop
		echo Encoding %changedName% in loop... 
		gsLzssByteFlags -e %changedName%
		insertBin encoded.bin %romName% -o %gfxOffset% -s %encodedGfxSize%
		
		goto :loop

	) 
)
