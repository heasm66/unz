```
UnZ 0.13 (2024-12-20 11:03:51) by Henrik Åsman, (c) 2021-2024
Usage: unz [option] [file]
Unpack Z-machine file format information.

 -a                 Show the abbreviation sections.
 -d                 Show the dictionary section.
 -f                 Show all sections (default).
 -g                 Show the grammar section.
 --gametext         Output only a'gametext.txt' format of all text in the file.
 -h, --help, /?     Show this help.
 --hexdump          Show raw hexdump before each section.
 --hide             Don't show the abbreviation insertion points in the strings.
 -i                 Show the header section.
 -m                 Show the memory map.
 -o                 Show the objects sections.
 -s                 Show the strings section.
 --syntax 0/txd     Use TXD default syntax for the z-code decompilation. (default)
          1/inform  Use Inform syntax for the z-code decompilation. (txd -a)
          2/ZAP     Use ZAP syntax for the z-code decompilation.
 -u                 Show the unidentified sections.
 -v                 Show the variable section.
 -x                 Show miscellaneous other sections.
 -z                 Show the z-code section.
 -z <hexaddress>    Show the single decompiled z-code routine at <hexaddress>

Report bugs/suggestions to: heasm66@gmail.com
UnZ homepage: https://github.com/heasm66/UnZ
```
