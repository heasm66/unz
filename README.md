## UnZ

Precompiled binariers at: https://drive.google.com/drive/folders/1jl8Ym-6GzLiJgfdY8QORERw9Krvdk_-k
```
UnZ 0.13 (2024-12-27 09:30:03) by Henrik Åsman, (c) 2021-2024
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
 --invisiclues      Show Object Tree in InvisiClues format.
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


Changelog:

	0.13 2024-12-27
	---------------
	  * Fixed bug that couldn't handle "no-verb" with 0 number_of_grammar_lines
		in Inform_GV2.
	  * Inform6: Support for grammar version 3.
	  * Support for unicode translation table.
	  * Identify old compiled files right as Zilch(old Infocom files with oddly formed serials).
	  * Accept files with no abbreviation tables.
	  * Accept files with no objects.
	  * Print object tree, special output for InvisiClues-files.
	  * Dialog: Identify memory areas for heaps, predicates and scratch area.
	  * Dialog: Mark data, that fills out so static mem - global start 
				always is at least 480 bytes, as padding. 

	0.12 2024-02-24
	---------------
	  * Call without options or file prints help (same as -h).
	  * Cosmetic changes to printing in the objects section.
	  * List calls from property, action and preaction/parsing in 
		z-code routine header.
	  * List possible startingpoint for arrays in "undentified data",
		collected from opcodes loadb, loadw, storeb, storew and globals.

	0.11 2024-02-18
	---------------
	  * Minor bug fixes and error handling.
	  * Handle corrupt adjective table (Curses_r7) for Inform5_ver1 grammar.

	0.10 2024-02-17
	---------------
	  * Initial commit.
```
