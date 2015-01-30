gs-lzss
=========
GBA Golden Sun LZSS tool. Some graphics in this game is encoded with LZSS

Usage:
```
gs-lzss [-d | -b | -e] file_name [offset]
```


***-d*** - Decode from ROM to binary file. Input ROM name and offset in it must be specified: -d file_name offset

***-b*** - Batch decode from ROM. -b <file_name table_start_offset entries_count output_folder>

***-e*** - Encode from raw binary. Input binary file with raw data must be specified: -e file_name