gs-lzss
=========
GBA Golden Sun Huffman tool. Text in "Golden Sun" and "Golden Sun - The Lost Age" is compressed with modified Huffman, based on dual chars combination encoding.

Usage:
```
gs-huffman [-d | -b | -e] file_name [offset]
```


***-d*** - Decode message by index from ROM. Input ROM name and offsets for tree pointers, text pointers and message index to decode must be specified: -d <file_name tree_ptrs_offset text_ptrs_offset message_index>

***-b*** - Batch decode messages from ROM. -b file_name tree_ptrs_offset text_ptrs_offset message_count

***-e*** - Encode from raw binary. Input binary file with raw data, offsets in ROM of trees and tree pointers must be specified: -e file_name trees_offset tree_prs_offset

***-o[FILE], --output[=FILE]*** - Output to binary FILE; if option is not specified, -d and -s are output to stdout

***-h, --help*** - Display help

***-v, --version*** - Output version information

See additional files in [release](https://github.com/romhack/GoldenSunCompression/releases/latest) archive. Usage examples are in *.bat files. One bat file is setup for "Golden Sun", ther other is for "Golden Sun - The Lost Age".  Recommended translation scheme:  

1. "run_gs-huffman_GS1.bat -d"  
  
2. modify script.bin as you like  
  
3. "run_gs-huffman_GS1.bat -e" - this will also paste modified encoded files in separate ROM
  
4. go to 2


Build:
```
$ cabal sandbox init
$ cabal install -j