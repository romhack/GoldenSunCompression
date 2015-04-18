:start

gcc decode.c -Wall -ggdb -o decode.exe
decode.exe > log.txt
fc log.txt logMaster.txt

pause
goto start