gsLzssBitFlag
=========
GBA Golden Sun LZSS byte flags tool. Some graphics in this game is encoded with LZSS scheme with bit flag (raw/code) preceiding corresponding LZ entry.

Usage:
```
gsLzssBitFlag [-d | -b | -e] file_name [offset]
```   
See also included batch files for usage examples.


***-d*** - Decode from ROM to binary file. Input ROM name and offset in it must be specified: -d file_name offset

***-b*** - Batch decode from ROM. -b <file_name table_start_offset entries_count output_folder>

***-e*** - Encode from raw binary. Input binary file with raw data must be specified: -e file_name   

Installation:
```
stack build
stack exec -- gsLzssBitFlag -v
```
