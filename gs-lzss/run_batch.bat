echo off
set romName="0171 - Golden Sun (UE).gba"
set tileEditor="d:\Emulation\Tools\Graphics\Tile Moelester\tm018.jar"
set outDir="out/"
set tableOffset=0x43c024
set entryCount=0x31

IF EXIST %outDir% rd /s /q %outDir%
gs-lzss -b %romName% %tableOffset% %entryCount% %outDir%
::echo "Press any button to open all files in tile editor"
pause
::for %%v in (%outDir%) do start %tileEditor% "%%~v"