gsLzssByteFlags
=========
GBA Golden Sun LZSS byte flags tool. Some graphics in this game is encoded with LZSS scheme with flags (raw/code) are clustered in byte before 8 LZ entries.

Usage:
```
gsLzssByteFlags [-d | -b | -e] file_name [offset]
```   
See also included batch files for usage examples.


***-d*** - Decode from ROM to binary file. Input ROM name and offset in it must be specified: -d file_name offset

***-b*** - Batch decode from ROM. -b <file_name table_start_offset entries_count output_folder>

***-e*** - Encode from raw binary. Input binary file with raw data must be specified: -e file_name   

Installation:
```
stack build
stack exec -- gsLzssByteFlags -v
```
