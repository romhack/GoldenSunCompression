echo off
::-d to decode from ROM, -e to encode and paste to ROM in the same offset, comparing original encoded size
set romName="0171 - Golden Sun (UE).gba"
set encodedGfxSize=0x124F
set changedName=gfxChanged.bin
set gfxOffset=0x434CA8

IF "%1"=="-d" (
	echo Decoding at %gfxOffset%...
	gsLzssBitFlag-exe -d %romName% %gfxOffset%
	pause
) ELSE ( IF "%1"=="-e" (
	:loop
		echo Encoding %changedName% in loop... 
		gsLzssBitFlag-exe -e %changedName%
		insertBin encoded.bin %romName% -o %gfxOffset% -s %encodedGfxSize%
		
		goto :loop

	) 
)
