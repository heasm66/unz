'MIT License

'Copyright(c) 2021-2025 Henrik Åsman

'Permission Is hereby granted, free Of charge, to any person obtaining a copy
'of this software And associated documentation files (the "Software"), to deal
'in the Software without restriction, including without limitation the rights
'to use, copy, modify, merge, publish, distribute, sublicense, And/Or sell
'copies of the Software, And to permit persons to whom the Software Is
'furnished to do so, subject to the following conditions:

'The above copyright notice And this permission notice shall be included In all
'copies Or substantial portions of the Software.

'THE SOFTWARE Is PROVIDED "AS IS", WITHOUT WARRANTY Of ANY KIND, EXPRESS Or
'IMPLIED, INCLUDING BUT Not LIMITED To THE WARRANTIES Of MERCHANTABILITY,
'FITNESS FOR A PARTICULAR PURPOSE And NONINFRINGEMENT. IN NO EVENT SHALL THE
'AUTHORS Or COPYRIGHT HOLDERS BE LIABLE For ANY CLAIM, DAMAGES Or OTHER
'LIABILITY, WHETHER In AN ACTION Of CONTRACT, TORT Or OTHERWISE, ARISING FROM,
'OUT OF Or IN CONNECTION WITH THE SOFTWARE Or THE USE Or OTHER DEALINGS IN THE
'SOFTWARE.

Imports System
Imports System.Globalization
Imports System.Net
Imports System.Reflection
Imports System.Reflection.Metadata
Imports System.Runtime.InteropServices.JavaScript.JSType
Imports Microsoft

' Set this to false before release
#Const _IN_DEVELOPMENT = False

' ZILCH, grammar version 1
'   All Infocom except Mini-Zork 1, Sherlock, Abyss, Zork Zero, Shogun & Arthur
' ZILCH, grammar version 1 with COMPACT-SYNTAXES?
'   Mini-Zork 1
' ZILCH, grammar version 1 with COMPACT-SYNTAXES? and COMPACT-VOCABULARY?
'   Sherlock
' ZILCH, grammar version 2
'   Abyss, Zork Zero, Shogun & Arthur
' Inform5, grammar version 1
'   T ex Curses_r16, Jigsaw
' Inform6, grammar version 1 with compacted parsing routine table (ver. 6.0-)
' Inform6, grammar version 2 (ver. 6.10-)

' Inform6 - Memory layout                               Zilf/Zapf - Memory layout                       Dialog - Memory layout (backend_z..bacend_z)
'    Header                                                Header                                       Header                      (64 bytes)
'    Low Strings and Abbreviations                         Abbreviations                                Main heap         (0x1F)
'       Abbreviation Strings                                  Abbreviation strings                      Aux heap          (0x3F)
'       Abbreviation Table                                    Abbreviation Table                        Long-term heap    (0x3F)
'    Header extension table                                Global Variables                             Objects and Properties
'    Z-character set table                                 Objects and Properties                          object property defaults (126 bytes)
'    Unicode translation table                                property defaults                            object structures
'    Objects and Properties                                   object structures                            property tables
'       default_value                                         property tables                           seltable
'       object_tree                                        Impure Tables                                Object extra flag array (0 or # obj)
'       properties_table                                   Vocabulary Tables                            Global Variables (only used)
'    Table of Class Prototype Object Numbers               Charset Table                                Predicates
'    Table of Identifier Names                             Pure Tables (among are the following)        Abbreviation strings (2 bytes)
'       individual_name    length word array                  Preposition Table                         Abbreviation Table (192 bytes)
'       attribute_name     48 word array                      Verb Table                                Scratch (12 bytes)
'       action_name        word array                         Action Table                              worldtable
'       array_name         word array                         Preaction Table                           datatable
'       named_routine                                         Syntax Table                              Dictionary
'    Table of Indiv Property Values                           Header Extension Table                    Code Area
'    Variables and Dynamic Arrays                             Terminating Characters Table              Strings Area
'       Dynamic Arrays                                        User Tables
'       Global Variables
'    Terminating Characters Table
'    Grammar Table
'    Grammar Table Data
'    Actions Table
'    Preaction Table (parsing routines) (GV1 & GV3)
'    Adjectives Table                   (GV1 & GV3)
'    Dictionary
'    (Module Map), no longer used
'    Static Arrays
'    Code Area
'    Strings Area

Module Program
    Private byteStory() As Byte
    Private ReadOnly sAbbreviations(95) As String
    Private iZVersion As Integer
    Private compilerSource As EnumCompilerSource
    Private grammarVer As EnumGrammarVer = EnumGrammarVer.UNKNOWN
    Private ReadOnly memoryMap As New List(Of MemoryMapEntry)
    Private ReadOnly alphabet(2) As String
    Private showHeader As Boolean = False
    Private showAbbreviations As Boolean = False
    Private showObjects As Boolean = False
    Private showGrammar As Boolean = False
    Private showDictionary As Boolean = False
    Private showZCode As Boolean = False
    Private showZCodeVerbose As Boolean = False
    Private showStrings As Boolean = False
    Private showMemoryMap As Boolean = False
    Private showHex As Boolean = False
    Private zcodeSyntax As SyntaxType = SyntaxType.TXD
    Private showUnidentified As Boolean = False
    Private showOnlyAddress As Integer = 0
    Private createGametext As Boolean = False
    Private showExtra As Boolean = False
    Private showVariables As Boolean = False
    Private showAbbrevsInsertion As Boolean = True
    Private ReadOnly objectNames As New List(Of String)
    Private ReadOnly inlineStrings As New List(Of InlineString)
    Private buildDateTimeLocal As Date
    Private showInvisiClues As Boolean = False

    Private Enum SyntaxType As Integer
        TXD = 0
        Inform6 = 1
        ZAP = 2
        Pseudo = 3
    End Enum

    Private Class GrammarScanResult
        Public grammarVer As EnumGrammarVer = EnumGrammarVer.UNKNOWN
        Public addrGrammarTableStart As Integer = 0
        Public addrGrammarTableEnd As Integer = 0
        Public addrGrammarDataStart As Integer = 0
        Public addrGrammarDataEnd As Integer = 0
        Public NumberOfActions As Integer = 0
        Public NumberOfVerbs As Integer = 0
        Public NumberOfPrepositions As Integer = 0
        Public NumberOfParsingRoutines As Integer = 0
        Public CompactSyntaxes As Boolean = False
    End Class

    Sub Main(args As String())
        Try
            ' ***** Open story-fil *****
            Dim sFilename As String = ""

            ' Get date of build
            buildDateTimeLocal = Helper.GetBuildDateUtc(Assembly.GetExecutingAssembly()).ToLocalTime()

            ' Use UTF8 for output
            Console.OutputEncoding = System.Text.Encoding.UTF8

            'sFilename = "games\AMFV.dat"
            'sFilename = "games\Infidel.dat"
            'sFilename = "games\Starcross.dat"
            'sFilename = "games\Starcross-r15-s820901.z3"
            'sFilename = "games\Balances.z5"
            'sFilename = "games\sherbet.z5"
            'sFilename = "games\minster.z5"
            'sFilename = "games\Planetfall.dat"
            'sFilename = "games\nord and bert.dat"
            'sFilename = "games\Wishbringer.dat"
            'sFilename = "games\Starcross-r18-s830114.z3"
            'sFilename = "games\craverlyheights_zil.z5"
            'sFilename = "games\Enchanter.dat"
            'sFilename = "games\Trinity.dat"
            'sFilename = "games\craverlyheights_dg.z5"
            'sFilename = "games\seastalker.dat"
            'sFilename = "games\the_job_R5.z5"
            'sFilename = "games\craverlyheights_zil.z5"
            'sFilename = "games\Trinity.dat"
            'sFilename = "games\zilch_ver1\Zork1.dat"
            'sFilename = "games\Beyond zork.dat"
            'sFilename = "games\zork_285.z5"
            'sFilename = "games\Tangle.z5"
            'sFilename = "games\Inform5_ver1\Jigsaw.z8"
            'sFilename = "games\zilch_ver2\arthur.zip"
            'sFilename = "games\zilch_ver2\zork0.zip"
            'sFilename = "games\zilch_ver2\shogun.zip"
            'sFilename = "games\zilch_ver2\shogun.z6"
            'sFilename = "games\zilf_ver1\craverlyheights_zil_ver_0_10.z5"
            'sFilename = "games\Inform6_ver2\craverlyheights_puny.z5"
            'sFilename = "games\Inform6_ver2\hibernated1.z5"
            'sFilename = "games\the_job_R5.z5"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\Inform6\Source\Hibernated1\hibernated1_test.z5"
            'sFilename = "games\zilf_ver1\craverlyheights_zil.z3"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\Inform6\Source\Galatea\galatea.z8"
            'sFilename = "games\zilf_ver1\craverlyheights_zil_ver_0_9.z5"
            'sFilename = "games\zilf_ver1\craverlyheights_zil_ver_0_10.z5"
            'sFilename = "games\craverlyheights_dg.z5"
            'sFilename = "games\Dorm Game.z3"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\ZAbbrevMaker_benchmarks\Craverly Heights\bin\craverlyheights_zil.z5"

            ' ZILF, grammar version 1 with COMPACT-SYNTAXES?
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\ZAbbrevMaker_benchmarks\minizork2-renovated-rel15\mini2.z3"

            ' ZILF, grammar version 1
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\ZAbbrevMaker_benchmarks\heart-of-ice-master\src\heartice.z5"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\ZAbbrevMaker_benchmarks\zork1\zork1.z3"
            'sFilename = "games\zilf_ver1\craverlyheights_zil_ver_0_10.z5"

            ' ZILCH, grammar version 1
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Infocom\Wishbringer\wishbringer.dat"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Infocom\Ballyhoo\Ballyhoo.dat"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Infocom\The Witness\Witness.dat"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Infocom\Suspect\Suspect.dat"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Infocom\deadline\deadline.dat"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Infocom\suspended\suspended.dat"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Other\Nelson, Graham\Curses!\curses-r7.z3"

            ' ZILCH, grammar version 1 with COMPACT-SYNTAXES?
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Infocom\Minizork\minizork.z3"

            ' ZILCH, grammar version 1 with COMPACT-SYNTAXES? and COMPACT-VOCABULARY?
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Infocom\Sherlock\sherlock.dat"

            ' ZILCH, grammar version 2
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Infocom\arthur\arthur.zip"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Infocom\Shogun\shogun.zip"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Infocom\Zork Zero\Zork0.zip"

            ' ZILF, grammar version 2
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\ZIL\Source\new_parser\milliways_np\h2.z6"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\ZIL\Source\new_parser\zork0\zork0.z6"

            ' Inform5, grammar version 1
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\ZAbbrevMaker_benchmarks\Curses_i6\bin\Curses!_r16.z5"

            ' Inform6, grammar version 2
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\ZAbbrevMaker_benchmarks\dorm_test\dorm_test.z3"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\ZAbbrevMaker_benchmarks\tristam-island-main\tristam-en.z3"
            'sFilename = "C:\Users\heasm\source\repos\UnZ\UnZ\games\Inform6_ver2\gostak.z5"
            'sFilename = "games\Inform6_ver2\craverlyheights_puny.z5"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\ZAbbrevMaker_benchmarks\Curses_i6\bin\Curses!_r18_642.z5"

            ' Inform6, grammar version 3
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\ZAbbrevMaker_benchmarks\Curses_i6\bin\Curses!_r18_643b_gv3.z5"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\GV3_Tests\PunyInform-5_3_1\minimal_643_gv3.z5"

            ' Dialog
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\Dialog\games\the-impossible-bottle.z8"

            ' Error
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\the_obsessively_complete_infocom_catalog_241210\eblong.com\infocom\gamefiles\amfv-first-r1-s841226.z3"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\the_obsessively_complete_infocom_catalog_241210\eblong.com\infocom\gamefiles\leathergoddesses-r59-s000001.z3"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\the_obsessively_complete_infocom_catalog_241210\eblong.com\infocom\gamefiles\moonmist-beta-r65-sXXXXXX.z3"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\the_obsessively_complete_infocom_catalog_241210\eblong.com\infocom\gamefiles\wishbringer-rX165-s880609.z3"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\the_obsessively_complete_infocom_catalog_241210\eblong.com\infocom\gamefiles\zork1-r15-sUG3AU5.z2"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\the_obsessively_complete_infocom_catalog_241210\eblong.com\infocom\gamefiles\zork1-r2-sAS000C.z1"

            ' Corrupted files
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\the_obsessively_complete_infocom_catalog_241210\eblong.com\infocom\gamefiles\moonmist-beta-r65-sXXXXXX.z3"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\the_obsessively_complete_infocom_catalog_241210\eblong.com\infocom\gamefiles\spellbreaker-r63-sXXXXXX.z3"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\the_obsessively_complete_infocom_catalog_241210\eblong.com\infocom\gamefiles\wishbringer-rX165-s880609.z3"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\the_obsessively_complete_infocom_catalog_241210\eblong.com\infocom\gamefiles\unicode.z5"

            ' Unicode translation table
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Other\Francis, Garry\search-for-the-lost-ark.z5"

            'sFilename = "C:\Users\heasm\Downloads\test.z5"

            'Hints
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Infocom\Leather Godesses of Phobos\Clues\LGOP_InvisiClues.z5"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Infocom\Trinity\Clues\Trinity_InvisiClues.z5"

            ' Special
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Source\the_obsessively_complete_infocom_catalog_241210\eblong.com\infocom\gamefiles\zork0-r242-s880901.z6"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Scott Adams\02 Pirate Adventure.z5"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Games\Infocom\Bureaucracy\Bureaucracy.dat"
            'sFilename = "C:\Users\heasm\OneDrive\Dokument\Interactive Fiction\Projects\zil\Zork_770614\bin\zork_285.z5"

            ' ***** Unpack parameters *****
            ' A bit of a poor mans GetOpt. Should probable be replaced by a NuGet-libary...
            Dim unpackedArgs As New List(Of String)
            For i As Integer = 0 To args.Length - 1
                If args(i).StartsWith("-"c) And Not args(i).StartsWith("--") Then
                    Dim argsOk As Boolean = True
                    For j As Integer = 1 To args(i).Length - 1
                        If Not "adfgimosuvxz".Contains(args(i).Substring(j, 1)) Then
                            argsOk = False
                        End If
                    Next
                    If argsOk Then
                        For j As Integer = 0 To args(i).Length - 1
                            If args(i).Substring(j, 1) <> "-" Then
                                unpackedArgs.Add(String.Concat("-", args(i).Substring(j, 1)))
                            End If
                        Next
                    Else
                        unpackedArgs.Add(args(i))
                    End If
                Else
                    unpackedArgs.Add(args(i))
                End If
            Next
            If unpackedArgs.Count = 0 And sFilename = "" Then unpackedArgs.Add("-h")

            ' Get date in format "1st January 2025"
            Dim buildTimestamp As String = Helper.GetBuildDateUtc(Assembly.GetExecutingAssembly()).ToLocalTime.ToString("dxx xMM yyyy")
            buildTimestamp = buildTimestamp.Replace("x01", "January")
            buildTimestamp = buildTimestamp.Replace("x02", "February")
            buildTimestamp = buildTimestamp.Replace("x03", "March")
            buildTimestamp = buildTimestamp.Replace("x04", "April")
            buildTimestamp = buildTimestamp.Replace("x05", "May")
            buildTimestamp = buildTimestamp.Replace("x06", "June")
            buildTimestamp = buildTimestamp.Replace("x07", "July")
            buildTimestamp = buildTimestamp.Replace("x08", "August")
            buildTimestamp = buildTimestamp.Replace("x09", "September")
            buildTimestamp = buildTimestamp.Replace("x10", "October")
            buildTimestamp = buildTimestamp.Replace("x11", "November")
            buildTimestamp = buildTimestamp.Replace("x12", "December")
            Select Case buildTimestamp.Substring(0, 2)
                Case "1x", "21", "31" : buildTimestamp = buildTimestamp.Replace("xx", "st")
                Case "2x", "22" : buildTimestamp = buildTimestamp.Replace("xx", "nd")
                Case "3x", "23" : buildTimestamp = buildTimestamp.Replace("xx", "rd")
                Case Else : buildTimestamp = buildTimestamp.Replace("xx", "th")
            End Select
#If _IN_DEVELOPMENT = True Then
            buildTimestamp += ", in development"
#End If

            ' Parse arguments
            Dim allSections As Boolean = True
            For i As Integer = 0 To unpackedArgs.Count - 1
                Select Case unpackedArgs(i)
                    Case "-a"
                        showAbbreviations = True
                        allSections = False
                    Case "-d"
                        showDictionary = True
                        allSections = False
                    Case "-f"
                    ' Default
                    Case "-g"
                        showGrammar = True
                        allSections = False
                    Case "-h", "--help", "\?"
                        Console.Error.WriteLine("UnZ 0.16 ({0}) by Henrik Åsman, (c) 2021-2025", buildTimestamp)
                        Console.Error.WriteLine("Usage: unz [option]... [file]")
                        Console.Error.WriteLine("Unpack Z-machine file format information.")
                        Console.Error.WriteLine()
                        Console.Error.WriteLine(" -a                 Show the abbreviation sections.")
                        Console.Error.WriteLine(" -d                 Show the dictionary section.")
                        Console.Error.WriteLine(" -f                 Show all sections (default).")
                        Console.Error.WriteLine(" -g                 Show the grammar section.")
                        Console.Error.WriteLine(" --gametext         Output only a 'gametext.txt' format of all text in the file.")
                        Console.Error.WriteLine(" -h, --help, /?     Show this help.")
                        Console.Error.WriteLine(" --hexdump          Show raw hexdump before each section.")
                        Console.Error.WriteLine(" --hide             Don't show the abbreviation insertion points in the strings.")
                        Console.Error.WriteLine(" -i                 Show the header section.")
                        Console.Error.WriteLine(" --invisiclues      Show Object Tree in InvisiClues format.")
                        Console.Error.WriteLine(" -m                 Show the memory map.")
                        Console.Error.WriteLine(" -o                 Show the objects sections.")
                        Console.Error.WriteLine(" -s                 Show the strings section.")
                        Console.Error.WriteLine(" --syntax 0/txd     Use TXD default syntax for the z-code decompilation. (default)")
                        Console.Error.WriteLine("          1/inform  Use Inform syntax for the z-code decompilation. (txd -a)")
                        Console.Error.WriteLine("          2/ZAP     Use ZAP syntax for the z-code decompilation.")
                        Console.Error.WriteLine("          3/pseudo  Use pseudo-code (Informish) syntax for the z-code decompilation.")
                        Console.Error.WriteLine(" -u                 Show the unidentified sections.")
                        Console.Error.WriteLine(" -v                 Show the variable section.")
                        Console.Error.WriteLine(" -x                 Show miscellaneous other sections.")
                        Console.Error.WriteLine(" -z                 Show the z-code section.")
                        Console.Error.WriteLine(" -z <hexaddress>    Show the single decompiled z-code routine at <hexaddress>")
                        Console.Error.WriteLine(" --zverbose         Show detailed breakdown of z-code")
                        Console.Error.WriteLine()
                        Console.Error.WriteLine("Report bugs/suggestions to: heasm66@gmail.com")
                        Console.Error.WriteLine("UnZ homepage: https://github.com/heasm66/unz")
                        Exit Sub
                    Case "--hide"
                        showAbbrevsInsertion = False
                    Case "-i"
                        showHeader = True
                        allSections = False
                    Case "-m"
                        showMemoryMap = True
                        allSections = False
                    Case "-o"
                        showObjects = True
                        allSections = False
                    Case "-s"
                        showStrings = True
                        allSections = False
                    Case "-u"
                        showUnidentified = True
                        allSections = False
                    Case "-v"
                        showVariables = True
                        allSections = False
                    Case "-x"
                        showExtra = True
                        allSections = False
                    Case "-z"
                        showZCode = True
                        allSections = False
                        If unpackedArgs.Count > i + 1 Then
                            If Integer.TryParse(unpackedArgs(i + 1), System.Globalization.NumberStyles.HexNumber, Nothing, showOnlyAddress) Then
                                i += 1
                            Else
                                showOnlyAddress = 0
                            End If
                        End If
                    Case "--zverbose"
                        showZCode = True
                        showZCodeVerbose = True
                    Case "--syntax"
                        If unpackedArgs.Count > i + 1 Then
                            If unpackedArgs(i + 1) = "0" Or unpackedArgs(i + 1).Equals("TXD", StringComparison.CurrentCultureIgnoreCase) Then
                                zcodeSyntax = SyntaxType.TXD
                                i += 1
                            ElseIf unpackedArgs(i + 1) = "1" Or unpackedArgs(i + 1).Equals("INFORM6", StringComparison.CurrentCultureIgnoreCase) Then
                                zcodeSyntax = SyntaxType.Inform6
                                i += 1
                            ElseIf unpackedArgs(i + 1) = "2" Or unpackedArgs(i + 1).Equals("ZAP", StringComparison.CurrentCultureIgnoreCase) Then
                                zcodeSyntax = SyntaxType.ZAP
                                i += 1
                            ElseIf unpackedArgs(i + 1) = "3" Or unpackedArgs(i + 1).Equals("PSEUDO", StringComparison.CurrentCultureIgnoreCase) Then
                                zcodeSyntax = SyntaxType.Pseudo
                                i += 1
                            End If
                        End If
                    Case "--hexdump"
                        showHex = True
                    Case "--gametext"
                        createGametext = True
                    Case "--invisiclues"
                        showInvisiClues = True
                        showObjects = True
                    Case Else
                        If unpackedArgs(i).StartsWith("-"c) Then
                            Console.Error.WriteLine("Unknown option, '" & unpackedArgs(i) & "'.")
                            Console.Error.WriteLine("Try 'unz -h' for more information.")
                            Exit Sub
                        End If
                        If IO.File.Exists(unpackedArgs(i)) Then
                            sFilename = unpackedArgs(i)
                        Else
                            Console.Error.WriteLine("Unknown file, '" & unpackedArgs(i) & "'.")
                            Console.Error.WriteLine("Try 'unz -h' for more information.")
                            Exit Sub
                        End If
                End Select
            Next
            If allSections Then
                showHeader = True
                showAbbreviations = True
                showObjects = True
                showGrammar = True
                showDictionary = True
                showZCode = True
                showStrings = True
                showUnidentified = True
                showMemoryMap = True
                showExtra = True
                showVariables = True
            End If

            If sFilename.Trim = "" Then
                Console.Error.WriteLine("No file supplied.")
                Console.Error.WriteLine("Try 'unz -h' for more information.")
                Exit Sub
            End If

            Dim sFile As String = sFilename
            If Not IO.File.Exists(sFilename) Then
                sFile = IO.Path.Combine(IO.Directory.GetCurrentDirectory, sFilename)
            End If

            byteStory = IO.File.ReadAllBytes(sFile)

            iZVersion = byteStory(0)
            If iZVersion > 8 Then
                Console.Error.WriteLine("Unknown z-machine version, {0}.", iZVersion)
                Console.Error.WriteLine("Try 'unz -h' for more information.")
                Exit Sub
            End If

            If Not createGametext Then
                Console.WriteLine()
                Console.WriteLine("***** ANALYZING *****")
                Console.WriteLine()
                Console.WriteLine("Filename:                                  {0}", sFilename)
            End If

            compilerSource = IdentifyCompiler()
            Dim sCompilerSource As String = ""
            Select Case compilerSource
                Case EnumCompilerSource.ZILCH
                    sCompilerSource = "Zilch (Infocom)"
                Case EnumCompilerSource.ZILF
                    sCompilerSource = "Zilf/Zapf"
                Case EnumCompilerSource.INFORM5
                    sCompilerSource = "Inform5 (or earlier)"
                Case EnumCompilerSource.INFORM6
                    sCompilerSource = "Inform " & ExtractASCIIChars(60, 4)
                Case EnumCompilerSource.DIALOG
                    sCompilerSource = "Dialog " & ExtractASCIIChars(60, 2) & "/" & ExtractASCIIChars(62, 2)
                Case Else
                    sCompilerSource = "Unknown"
            End Select
            If Not createGametext Then
                Console.WriteLine("Compiled With:                             {0}", sCompilerSource)
                Console.WriteLine("Z-machine version:                         {0}", iZVersion)
            End If

            ' ****************************
            ' ***** Build memory map *****
            ' ****************************

            Dim iLengthFile As Integer = Helper.GetAdressFromWord(byteStory, 26)
            iLengthFile *= 2
            If iZVersion > 3 Then iLengthFile *= 2
            If iZVersion > 5 Then iLengthFile *= 2
            If iLengthFile = 0 Or iLengthFile > byteStory.Length Then iLengthFile = byteStory.Length

            Dim checksum As Integer = 0
            For i As Integer = 64 To iLengthFile - 1
                checksum = (checksum + byteStory(i) And &HFFFF)
            Next
            If Not createGametext Then
                Console.Write("Calculated checksum:                       0x{0:X4}, checksum ", checksum)
                If checksum = Helper.GetAdressFromWord(byteStory, 28) Then Console.WriteLine("ok") Else Console.WriteLine("error")
            End If

            memoryMap.Add(New MemoryMapEntry("Header table", &H0, &H3F, MemoryMapType.MM_HEADER_TABLE))     ' Header, always in the first 64 bytes

            ' Extract possible custom alphabet
            Dim iAlphabetAddress As Integer = Helper.GetAdressFromWord(byteStory, &H34)
            If iAlphabetAddress > 0 Then
                ' Always replace " with ~ (for presentation, Inform style)
                ' alphabet2, position 7 is always new_line, represented by ^ (Inform style)
                alphabet(0) = "      " & ExtractASCIIChars(iAlphabetAddress, 26).Replace(Chr(34), "~")
                alphabet(1) = "      " & ExtractASCIIChars(iAlphabetAddress + 26, 26).Replace(Chr(34), "~")
                alphabet(2) = "      " & ExtractASCIIChars(iAlphabetAddress + 52, 26).Replace(Chr(34), "~")
                alphabet(2) = String.Concat(alphabet(2).Substring(0, 7), "^", alphabet(2).Substring(8))
                memoryMap.Add(New MemoryMapEntry("Z-character set table", iAlphabetAddress, iAlphabetAddress + 77, MemoryMapType.MM_CHRSET))
            Else
                alphabet(0) = "      abcdefghijklmnopqrstuvwxyz"
                alphabet(1) = "      ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                If iZVersion = 1 Then
                    alphabet(2) = "       0123456789.,!?_#'~/\<-:()"
                Else
                    alphabet(2) = "       ^0123456789.,!?_#'~/\-:()"
                End If
            End If


            ' Header extension table
            Dim iAddrHeaderExtStart As Integer = Helper.GetAdressFromWord(byteStory, &H36)
            Dim iHeaderExtLen As Integer = 0
            If iAddrHeaderExtStart > 0 Then
                iHeaderExtLen = Helper.GetAdressFromWord(byteStory, iAddrHeaderExtStart)     ' Number of further words in table
                memoryMap.Add(New MemoryMapEntry("Header extension table", iAddrHeaderExtStart, iAddrHeaderExtStart + iHeaderExtLen * 2 + 1, MemoryMapType.MM_HEADER_EXT_TABLE))
                If iHeaderExtLen >= 3 Then
                    Dim unicodeAddr As Integer = Helper.GetAdressFromWord(byteStory, iAddrHeaderExtStart + 6)
                    If unicodeAddr > 0 Then
                        Dim unicodeLen As Integer = byteStory(unicodeAddr)
                        Helper.UnicodeTranslationTableAddr = unicodeAddr
                        memoryMap.Add(New MemoryMapEntry("Unicode translation table", unicodeAddr, unicodeAddr + unicodeLen * 2, MemoryMapType.MM_UNICODE_TABLE))
                    End If

                End If
            End If

            ' Terminating characters table
            Dim iAddrTCharsStart As Integer = Helper.GetAdressFromWord(byteStory, &H2E)
            Dim iTCharsLen As Integer = 0
            If iAddrTCharsStart > 0 Then
                If byteStory(iAddrTCharsStart + iTCharsLen) > 0 Then
                    Do
                        iTCharsLen += 1
                    Loop Until byteStory(iAddrTCharsStart + iTCharsLen) = 0
                End If
                memoryMap.Add(New MemoryMapEntry("Terminating characters table", iAddrTCharsStart, iAddrTCharsStart + iTCharsLen, MemoryMapType.MM_TERMINATING_CHARS_TABLE))
            End If

            ' IFID
            Dim patternIFID As Byte() = {85, 85, 73, 68, 58, 47, 47}   ' UUID://
            Dim foundIFID As Boolean = False
            Dim addressIFID As Integer = 0
            Dim IFID As String = ""
            For i As Integer = 0 To byteStory.Length - patternIFID.Length - 1
                If byteStory.Skip(i).Take(patternIFID.Length).SequenceEqual(patternIFID) Then
                    foundIFID = True
                    addressIFID = i
                    Exit For
                End If
            Next
            If foundIFID AndAlso byteStory(addressIFID + 43) = 47 AndAlso byteStory(addressIFID + 44) = 47 Then   ' An IFID should end with //
                Dim endAddress As Integer = addressIFID + 44
                For i = addressIFID To endAddress
                    IFID = String.Concat(IFID, Chr(byteStory(i)))
                Next
                If byteStory(addressIFID - 1) = 45 Then addressIFID -= 1   ' Sometimes (Inform) the IFID is constructed as a length byte array with the length of the array in the first byte
                If Not createGametext Then
                    Console.WriteLine("IFID:                                      {0}", IFID)
                End If
                memoryMap.Add(New MemoryMapEntry("IFID", addressIFID, endAddress, MemoryMapType.MM_IFID))   ' Always 45 bytes
            Else
                addressIFID = 0
            End If

            ' Abbreviation table
            Dim iAddrAbbrevTableStart As Integer = Helper.GetAdressFromWord(byteStory, 24)
            Dim iAddrAbbrevStringsStart As Integer = 0
            Dim iAddrAbbrevStringsEnd As Integer = 0
            Dim maxNumberofAbbrevs As Integer = 96
            If iAddrAbbrevTableStart > 0 Then
                If iZVersion < 3 Then maxNumberofAbbrevs = 32

                memoryMap.Add(New MemoryMapEntry("Abbreviation table", iAddrAbbrevTableStart, iAddrAbbrevTableStart + 2 * maxNumberofAbbrevs - 1, MemoryMapType.MM_ABBREVIATION_TABLE))   ' 192 bytes long for v3+ (96 abbreviations), v2 only 32 abbreviations

                ' Abbreviation strings
                memoryMap.Add(New MemoryMapEntry("Abbreviation strings", 0, 0, MemoryMapType.MM_ABBREVIATION_STRINGS))
                For i As Integer = 0 To (maxNumberofAbbrevs - 1)
                    Dim iAddressAbbrev As Integer = 2 * (byteStory(iAddrAbbrevTableStart + i * 2) * 256 + byteStory(iAddrAbbrevTableStart + i * 2 + 1))
                    If iAddressAbbrev > 0 Then
                        sAbbreviations(i) = ExtractZString(iAddressAbbrev, False)          ' Abbreviations can't contain abbreviations 
                        If memoryMap.Last.addressStart = 0 Then memoryMap.Last.addressStart = iAddressAbbrev
                        If iAddressAbbrev < memoryMap.Last.addressStart Then memoryMap.Last.addressStart = iAddressAbbrev
                        If iAddressAbbrev > memoryMap.Last.addressEnd Then memoryMap.Last.addressEnd = iAddressAbbrev
                    Else
                        sAbbreviations(i) = ""
                    End If
                Next
                If memoryMap.Last.addressStart = &H42 And byteStory(&H40) = &H80 And byteStory(&H41) = 0 Then
                    ' Inform places "   " as a default abbreviation here and if all 96 abbreviations are used, this becomes
                    ' an unused abbreviation string (80 00).
                    memoryMap.Last.addressStart -= 2
                End If
                If memoryMap.Last.addressEnd > 0 And (byteStory(memoryMap.Last.addressEnd) And &H80) = 0 Then
                    ' Search for last bytes in last Z-string (last word end with bit 15 set)
                    Do
                        memoryMap.Last.addressEnd += 2
                    Loop Until (byteStory(memoryMap.Last.addressEnd) And &H80) = &H80
                End If
                memoryMap.Last.addressEnd += 1
                iAddrAbbrevStringsStart = memoryMap.Last.addressStart
                iAddrAbbrevStringsEnd = memoryMap.Last.addressEnd
            End If

            ' Object defaults table
            Dim iAddrObjectDefaultsTableStart As Integer = Helper.GetAdressFromWord(byteStory, 10)
            Dim iPropDefaultCount As Integer = 63
            If iZVersion <= 3 Then iPropDefaultCount = 31
            Dim iAddrObjectTreeStart As Integer = iAddrObjectDefaultsTableStart + iPropDefaultCount * 2
            Dim iAddrObjectDefaultsTableEnd As Integer = iAddrObjectTreeStart - 1
            memoryMap.Add(New MemoryMapEntry("Object defaults table", iAddrObjectDefaultsTableStart, iAddrObjectDefaultsTableEnd, MemoryMapType.MM_PROPERTY_DEFAULTS_TABLE))

            ' Object tree table
            Dim oPropertyAnalyser As New PropertyAnalyser
            oPropertyAnalyser.Init(byteStory, iAddrObjectTreeStart, iZVersion, compilerSource)
            Dim iObjectTreeEntryLen As Integer = oPropertyAnalyser.objectTreeEntryLength
            Dim iObjectCount As Integer = oPropertyAnalyser.objectCount
            Dim iAddrObjectTreeEnd As Integer = oPropertyAnalyser.addressObjectTreeEnd
            If Not createGametext Then
                Console.WriteLine("Object count:                              {0}", iObjectCount)
            End If
            If iObjectCount > 0 Then
                memoryMap.Add(New MemoryMapEntry("Object tree table", iAddrObjectTreeStart, iAddrObjectTreeEnd, MemoryMapType.MM_OBJECT_TREE_TABLE))
            End If

            ' Object properties tables
            Dim iAddrObjectPropTableStart As Integer = oPropertyAnalyser.addressObjectPropTableStart
            Dim iAddrObjectPropTableEnd As Integer = oPropertyAnalyser.addressObjectPropTableEnd
            If iObjectCount > 0 Then
                memoryMap.Add(New MemoryMapEntry("Object properties tables", iAddrObjectPropTableStart, iAddrObjectPropTableEnd, MemoryMapType.MM_OBJECT_PROPERTIES_TABLES))
            End If

            ' Set up variables for dictionary
            Dim iAddrDictionary As Integer = Helper.GetAdressFromWord(byteStory, 8)
            Dim iWordSeparatorsCount As Integer = byteStory(iAddrDictionary)
            Dim iWordSize As Integer = byteStory(iAddrDictionary + iWordSeparatorsCount + 1)
            Dim iWordCount As Integer = byteStory(iAddrDictionary + iWordSeparatorsCount + 2) * 256 + byteStory(iAddrDictionary + iWordSeparatorsCount + 3)
            Dim iAddrDictionaryEnd As Integer = iAddrDictionary + iWordSeparatorsCount + 3 + (iWordCount * iWordSize)

            ' Extract dictionary values
            Dim DictEntriesList As New DictionaryEntries With {.WordSize = iWordSize}
            Dim iLowestVerbNumber As Integer = 256
            For i As Integer = 0 To iWordCount - 1
                Dim dictEntry As New DictionaryEntry With {
                .dictAddress = iAddrDictionary + iWordSeparatorsCount + 4 + (i * iWordSize),
                .dictWord = ExtractZString(iAddrDictionary + iWordSeparatorsCount + 4 + (i * iWordSize), False)         ' Dictionary words don't use abbreviations
                }
                dictEntry.Byte6ToLast = byteStory(dictEntry.dictAddress + iWordSize - 6)
                dictEntry.Byte5ToLast = byteStory(dictEntry.dictAddress + iWordSize - 5)
                dictEntry.Byte4ToLast = byteStory(dictEntry.dictAddress + iWordSize - 4)
                dictEntry.Byte3ToLast = byteStory(dictEntry.dictAddress + iWordSize - 3)
                dictEntry.Byte2ToLast = byteStory(dictEntry.dictAddress + iWordSize - 2)
                dictEntry.Byte1ToLast = byteStory(dictEntry.dictAddress + iWordSize - 1)
                ' Set Flags, V1 & V2 as if it's grammar version 1 (Inform, Zilf or Zilch)
                ' Inform6 grammar version 2 don't use V2 and it can be stripped (len = 6 or 8)
                ' Zilch, Zilf grammar version 1 can strip V2 with COMPACT-VOCABULARY?
                If iWordSize = 6 Or iWordSize = 8 Then
                    dictEntry.Flags = dictEntry.Byte2ToLast
                    dictEntry.V1 = dictEntry.Byte1ToLast
                Else
                    dictEntry.Flags = dictEntry.Byte3ToLast
                    dictEntry.V1 = dictEntry.Byte2ToLast
                    dictEntry.V2 = dictEntry.Byte1ToLast
                End If
                DictEntriesList.Add(dictEntry)
                Dim iTmpNo As Integer = 256
                If {EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF}.Contains(compilerSource) AndAlso (dictEntry.Flags And 64) = 64 Then
                    If (dictEntry.Flags And 3) = 1 Then iTmpNo = dictEntry.V1 Else iTmpNo = dictEntry.V2
                End If
                If {EnumCompilerSource.INFORM5, EnumCompilerSource.INFORM6}.Contains(compilerSource) AndAlso (dictEntry.Flags And 1) = 1 Then iTmpNo = dictEntry.V1
                If iTmpNo < iLowestVerbNumber Then iLowestVerbNumber = iTmpNo
            Next
            memoryMap.Add(New MemoryMapEntry("Vocabulary/Dictionary", iAddrDictionary, iAddrDictionaryEnd, MemoryMapType.MM_DICTIONARY))

            Dim iVerbCount As Integer = 256 - iLowestVerbNumber
            Dim iVerbActionCount As Integer = 0
            Dim iAddrGrammarTableStart As Integer = 0
            Dim iAddrGrammarDataStart As Integer = 0
            Dim iAddrGrammarDataEnd As Integer = 0
            Dim compactSyntaxes As Boolean = False
            Dim resultGrammarScan As GrammarScanResult = Nothing
            If {EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF, EnumCompilerSource.INFORM5, EnumCompilerSource.INFORM6}.Contains(compilerSource) Then
                ' Scan for grammar table
                resultGrammarScan = ScanForGrammarTable(iVerbCount, DictEntriesList)
                grammarVer = resultGrammarScan.grammarVer
                compactSyntaxes = resultGrammarScan.CompactSyntaxes
                If Not grammarVer = EnumGrammarVer.UNKNOWN Then
                    iVerbCount = resultGrammarScan.NumberOfVerbs
                    If iVerbCount = 0 Then resultGrammarScan.NumberOfActions = 0
                    iVerbActionCount = resultGrammarScan.NumberOfActions
                    If Not createGametext Then
                        Console.WriteLine("Unique verbs count:                        {0}", iVerbCount)
                        Console.Write("Grammar table version:                     {0}", CInt(grammarVer))
                        If compactSyntaxes Then Console.Write(" with <COMPACT-SYNTAXES? T>")
                        Console.WriteLine()
                        Console.WriteLine("Verb action count:                         {0}", resultGrammarScan.NumberOfActions)
                    End If
                Else
                    If Not createGametext Then
                        Console.WriteLine("Unique verbs count:                        0")
                        Console.Write("Grammar table version:                     No syntax/grammar identified")
                        Console.WriteLine()
                        Console.WriteLine("Verb action count:                         0")
                    End If
                End If
                If Not grammarVer = EnumGrammarVer.UNKNOWN Then
                    iAddrGrammarTableStart = resultGrammarScan.addrGrammarTableStart
                    iAddrGrammarDataStart = resultGrammarScan.addrGrammarDataStart
                    iAddrGrammarDataEnd = resultGrammarScan.addrGrammarDataEnd
                    If resultGrammarScan.addrGrammarTableEnd - resultGrammarScan.addrGrammarTableStart > 0 Then
                        memoryMap.Add(New MemoryMapEntry("Syntax/Grammar table", resultGrammarScan.addrGrammarTableStart, resultGrammarScan.addrGrammarTableEnd, MemoryMapType.MM_GRAMMAR_TABLE))
                    End If
                    If resultGrammarScan.addrGrammarDataEnd - resultGrammarScan.addrGrammarDataStart > 0 Then
                        memoryMap.Add(New MemoryMapEntry("Syntax/Grammar table data", resultGrammarScan.addrGrammarDataStart, resultGrammarScan.addrGrammarDataEnd, MemoryMapType.MM_GRAMMAR_TABLE_DATA))
                    End If
                End If
            End If

            ' Print dictionary information
            If grammarVer = 1 And (iWordSize = 6 Or iWordSize = 8) And (compilerSource = EnumCompilerSource.ZILCH Or compilerSource = EnumCompilerSource.ZILF) Then DictEntriesList.v1_CompactVocabulary = True
            If grammarVer = 2 And (iWordSize = 9 Or iWordSize = 11) And (compilerSource = EnumCompilerSource.ZILCH Or compilerSource = EnumCompilerSource.ZILF) Then DictEntriesList.v2_OneBytePartsOfSpeech = True
            If grammarVer = 2 And iWordSize > 10 And (compilerSource = EnumCompilerSource.ZILCH Or compilerSource = EnumCompilerSource.ZILF) Then DictEntriesList.v2_WordFlagsInTable = False
            If Not createGametext Then
                Console.Write("Dictionary word count:                     {0}", iWordCount)
                If DictEntriesList.v1_CompactVocabulary Then Console.Write(" with <COMPACT-VOCABULARY? T>")
                If grammarVer = 2 And {EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF}.Contains(compilerSource) Then
                    If DictEntriesList.v2_OneBytePartsOfSpeech And Not DictEntriesList.v2_WordFlagsInTable Then Console.Write(" with <ONE-BYTE-PARTS-OF-SPEECH T>")
                    If Not DictEntriesList.v2_OneBytePartsOfSpeech And DictEntriesList.v2_WordFlagsInTable Then Console.Write(" with <WORD-FLAGS-IN-TABLE T>")
                    If DictEntriesList.v2_OneBytePartsOfSpeech And DictEntriesList.v2_WordFlagsInTable Then Console.Write(" with <ONE-BYTE-PARTS-OF-SPEECH T>, <WORD-FLAGS-IN-TABLE T>")
                End If
                Console.WriteLine()
            End If

            ' Analyze dictentries, now that we know the grammar-version
            For Each dictEntry As DictionaryEntry In DictEntriesList

                If {EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF}.Contains(compilerSource) AndAlso grammarVer = 1 Then
                    ' Verb
                    If (dictEntry.Flags And 64) = 64 Then
                        If (dictEntry.Flags And 3) = 1 Then dictEntry.VerbNum = dictEntry.V1 Else dictEntry.VerbNum = dictEntry.V2
                    End If
                    ' Adjective
                    If iZVersion <= 3 AndAlso (dictEntry.Flags And 32) = 32 Then
                        If (dictEntry.Flags And 3) = 2 Then dictEntry.AdjNum = dictEntry.V1 Else dictEntry.AdjNum = dictEntry.V2
                    End If
                    ' Direction
                    If (dictEntry.Flags And 16) = 16 Then
                        If (dictEntry.Flags And 3) = 3 Then dictEntry.DirNum = dictEntry.V1 Else dictEntry.DirNum = dictEntry.V2
                    End If
                    ' Preposition
                    If (dictEntry.Flags And 8) = 8 Then
                        If (dictEntry.Flags And 3) = 0 Then dictEntry.PrepNum = dictEntry.V1 Else dictEntry.PrepNum = dictEntry.V2
                    End If
                End If

                If {EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF}.Contains(compilerSource) AndAlso grammarVer = 2 Then
                    ' CLASSIFICATION-NUMBER are either the one or two last bytes, depending on ONE-BYTE-PARTS-OF-SPEECH
                    '   When the highest bit in CLASSIFICATION-NUMBER (bit 7 for byte or bit 15 for word) is set,
                    '   it means that it is packed. Shift other bits left 7 positions to get correct number ((&HB0 and &H3F) << 7 = 512) 
                    ' FLAGS are either third and fourth last bytes or stored in WORD-FLAG-TABLE, depending on WORD-FLAGS-IN-TABLE
                    ' SEMANTIC-STUFF are either 3rd & 4th or 5th & 6th last bytes, depending on WORD-FLAGS-IN-TABLE
                    '   Verb      : Contain address to verb definition
                    '   Direction : High byte is the direction ID
                    '   All other : 0 or address in dictionary = synonym

                    ' Calculate CLASSIFICATION-NUMBER and identify PARTS-OF-SPEECH by looking for well known words
                    If DictEntriesList.v2_OneBytePartsOfSpeech Then
                        If (dictEntry.Byte1ToLast And &H80) = &H80 Then
                            dictEntry.v2_ClassificationNumber = (dictEntry.Byte1ToLast And &H7F) << 7
                        Else
                            dictEntry.v2_ClassificationNumber = dictEntry.Byte1ToLast
                        End If
                    Else
                        If (dictEntry.Byte2ToLast And &H80) = &H80 Then
                            dictEntry.v2_ClassificationNumber = ((dictEntry.Byte1ToLast + 256 * dictEntry.Byte2ToLast) And &H7FFF) << 15
                        Else
                            dictEntry.v2_ClassificationNumber = dictEntry.Byte1ToLast + 256 * dictEntry.Byte2ToLast
                        End If
                    End If
                    If Int32.PopCount(dictEntry.v2_ClassificationNumber) = 1 Then
                        Select Case dictEntry.dictWord
                            Case "quit" : DictEntriesList.v2_VerbWord = dictEntry.v2_ClassificationNumber             '    1,      1,     1,      1 in all available
                            Case "it" : DictEntriesList.v2_NounWord = dictEntry.v2_ClassificationNumber               '    2,      2,     2,      2 in all available
                            Case "its" : DictEntriesList.v2_AdjWord = dictEntry.v2_ClassificationNumber               '    4,      4,     4,      4 in all available
                            Case "once" : DictEntriesList.v2_AdvWord = dictEntry.v2_ClassificationNumber              ' 1024,    n/a,     8,      8 in Zork 0, Shogun, Arthur, Milliways
                            Case "all" : DictEntriesList.v2_QuantWord = dictEntry.v2_ClassificationNumber             ' 2048,      8,    16,     16 in Zork 0, Shogun, Arthur, Milliways
                            Case "again" : DictEntriesList.v2_MiscWord = dictEntry.v2_ClassificationNumber            ' 4096,     16,    32,     32 in Zork 0, Shogun, Arthur, Milliways
                            Case "," : DictEntriesList.v2_CommaWord = dictEntry.v2_ClassificationNumber               '  512,   8192,  1024,   1024 in Zork 0, Shogun, Arthur, Milliways
                            Case "with" : DictEntriesList.v2_ParticleWord = dictEntry.v2_ClassificationNumber         '   16,   1024,  2048,   2048 in Zork 0, Shogun, Arthur, Milliways
                            Case "but" : DictEntriesList.v2_PrepWord = dictEntry.v2_ClassificationNumber              '   32,   2048,  4096,   4096 in Zork 0, Shogun, Arthur, Milliways
                            Case "am" : DictEntriesList.v2_ToBeWord = dictEntry.v2_ClassificationNumber               '    0,     64,   128,   2048 in Zork 0, Shogun, Arthur, Milliways. Milliways class it as PARTICLE
                            Case "'" : DictEntriesList.v2_ApostrWord = dictEntry.v2_ClassificationNumber              ' 4096,  16384,    32,  16384 in Zork 0, Shogun, Arthur, Milliways. Zork 0 and Arthur class it as MISCWORD
                            Case "of" : DictEntriesList.v2_OfWord = dictEntry.v2_ClassificationNumber                 '  128,  32768,    32,  32768 in Zork 0, Shogun, Arthur, Milliways. Arthur class it as MISCWORD
                            Case "the" : DictEntriesList.v2_ArticleWord = dictEntry.v2_ClassificationNumber           ' 4096,  65536,    32,  65536 in Zork 0, Shogun, Arthur, Milliways. Zork 0 and Arthur class it as MISCWORD
                            Case "~" : DictEntriesList.v2_QuoteWord = dictEntry.v2_ClassificationNumber               '  256, 131072, 16384, 131072 in Zork 0, Shogun, Arthur, Milliways
                            Case "." : DictEntriesList.v2_EOIWord = dictEntry.v2_ClassificationNumber                 ' 8192, 262144, 32768, 262144 in Zork 0, Shogun, Arthur, Milliways
                        End Select
                    End If
                    If Int32.PopCount(dictEntry.v2_ClassificationNumber And &HFFF8) = 1 Then     ' Words combined with verb, noun or adj
                        Select Case dictEntry.dictWord
                            Case "east" : DictEntriesList.v2_DirWord = dictEntry.v2_ClassificationNumber And &HFFF8   '    8,     32,    64,     64 in Zork 0, Shogun, Arthur, Milliways
                            Case "can" : DictEntriesList.v2_CanDoWord = dictEntry.v2_ClassificationNumber And &HFFF8  '    0,    256,   512,      0 in Zork 0, Shogun, Arthur, Milliways
                            Case "how" : DictEntriesList.v2_QWord = dictEntry.v2_ClassificationNumber And &HFFF8      '    0,    128,   256,      0 in Zork 0, Shogun, Arthur, Milliways
                            Case "ask" : DictEntriesList.v2_AskWord = dictEntry.v2_ClassificationNumber And &HFFF8    '   64,   4096,  8192,   8192 in Zork 0, Shogun, Arthur, Milliways
                        End Select
                    End If

                    ' SEMANTIC-STUFF
                    If DictEntriesList.v2_WordFlagsInTable And DictEntriesList.v2_OneBytePartsOfSpeech Then dictEntry.v2_SemanticStuff = dictEntry.Byte2ToLast + 256 * dictEntry.Byte3ToLast
                    If DictEntriesList.v2_WordFlagsInTable And Not DictEntriesList.v2_OneBytePartsOfSpeech Then dictEntry.v2_SemanticStuff = dictEntry.Byte3ToLast + 256 * dictEntry.Byte4ToLast
                    If Not DictEntriesList.v2_WordFlagsInTable And DictEntriesList.v2_OneBytePartsOfSpeech Then dictEntry.v2_SemanticStuff = dictEntry.Byte4ToLast + 256 * dictEntry.Byte5ToLast
                    If Not DictEntriesList.v2_WordFlagsInTable And Not DictEntriesList.v2_OneBytePartsOfSpeech Then dictEntry.v2_SemanticStuff = dictEntry.Byte5ToLast + 256 * dictEntry.Byte6ToLast

                    ' WORD-FLAGS
                    If DictEntriesList.v2_WordFlagsInTable Then
                        ' The location of the WORD-FLAGS-TABLE isn't known yet. This will be resolved later when the location is known.
                    Else
                        If DictEntriesList.v2_OneBytePartsOfSpeech Then dictEntry.v2_WordFlags = dictEntry.Byte2ToLast + 256 * dictEntry.Byte3ToLast
                        If Not DictEntriesList.v2_OneBytePartsOfSpeech Then dictEntry.v2_WordFlags = dictEntry.Byte3ToLast + 256 * dictEntry.Byte4ToLast
                    End If
                End If

                If {EnumCompilerSource.INFORM5, EnumCompilerSource.INFORM6}.Contains(compilerSource) Then
                    ' Verb
                    If (dictEntry.Flags And 1) = 1 Then dictEntry.VerbNum = dictEntry.V1

                    ' Inform grammar ver 1 preposition
                    If grammarVer = 1 AndAlso (dictEntry.Flags And 8) = 8 Then dictEntry.PrepNum = dictEntry.V2
                End If

            Next
            If {EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF}.Contains(compilerSource) AndAlso grammarVer = 1 And Not createGametext Then Console.WriteLine("Unique directions count:                   {0}", DictEntriesList.DirectionCount())
            If grammarVer = 1 And Not createGametext Then Console.WriteLine("Unique prepositions count:                 {0}", DictEntriesList.PrepositionCountUnique)

            ' Z-code
            Dim iFOFF As Integer = Helper.GetAdressFromWord(byteStory, &H28) * 8
            Dim iCodeBase As Integer = Helper.GetNextValidPackedAddress(byteStory, iAddrDictionaryEnd + 1)
            If iFOFF > 0 Then iCodeBase = iFOFF
            Dim scaler As Integer = 2
            Select Case iZVersion
                Case 4, 5, 6, 7
                    scaler = 4
                Case 8
                    scaler = 8
            End Select
            Dim iStart As Integer = iCodeBase
            Dim iNext As Integer = 0
            Dim iAddrFirstRoutine As Integer = 0
            Dim dataStartList As New List(Of Integer)
            Dim dataEndList As New List(Of Integer)
            Dim iDataIndex As Integer = -1
            Dim bDataFlag As Boolean = False
            Dim validRoutinesList As New List(Of RoutineData)
            Dim validStringsList As New List(Of StringData)
            Dim iAddrInitialPC = Helper.GetAdressFromWord(byteStory, 6)
            Dim totalPadding As Integer = 0
            If iZVersion = 6 Then iAddrInitialPC = iAddrInitialPC * scaler + iFOFF ' In version 6 the address is packed
            Dim oDecode As New Decode
            Dim routineCallsFromTo As New List(Of CallsFromTo)
            Do
                Dim inlineStringCountBefore = inlineStrings.Count
                iNext = oDecode.DecodeRoutine(iStart, byteStory, sAbbreviations, True, zcodeSyntax, validStringsList,
                                          validRoutinesList, DictEntriesList, alphabet, iAddrInitialPC,
                                          oPropertyAnalyser.propertyNumberMin, oPropertyAnalyser.propertyNumberMax,
                                          showAbbrevsInsertion, inlineStrings, False)
                If iNext > 0 Then
                    Dim oRoutineData As New RoutineData With {
                    .entryPoint = iStart,
                    .endPoint = iNext - 1,
                    .entryPointPacked = (iStart - iFOFF) \ scaler
                }
                    routineCallsFromTo.AddRange(oDecode.callsTo)        ' Concatinate calls
                    validRoutinesList.Add(oRoutineData)
                    Dim inlineStringCountAfter As Integer = inlineStrings.Count
                    If inlineStringCountAfter <> inlineStringCountBefore Then
                        Dim inlineStringSize As Integer = 0
                        For i = inlineStringCountBefore To inlineStringCountAfter - 1
                            inlineStringSize += inlineStrings(i).size
                        Next
                        Dim padding As Integer = Helper.GetNextValidPackedAddress(byteStory, iNext) - iNext
                        totalPadding += padding
                        inlineStrings.Add(New InlineString With {.text = String.Concat("z-routine size: ", iNext - iStart, " bytes, inline strings size: ", inlineStringSize, " bytes, z-routine without inline strings size: ", iNext - iStart - inlineStringSize, " bytes, Padding: ", padding, " byte(s)."),
                                                         .size = 0})
                    End If
                    iStart = Helper.GetNextValidPackedAddress(byteStory, iNext)
                Else
                    ' Routines below initial pc are considered false hits if they are small (under 512 bytes) and not continous
                    ' with routines starting at initial pc 
                    If iStart < iAddrInitialPC And validRoutinesList.Count > 0 AndAlso iStart - validRoutinesList(0).entryPoint < 512 Then
                        oDecode = New Decode
                        validRoutinesList = New List(Of RoutineData)
                    End If
                    iStart += scaler
                End If
            Loop Until iStart >= iLengthFile - 1 Or (iNext < 0 AndAlso validRoutinesList.Count > 1 AndAlso iStart > oDecode.highest_routine AndAlso iStart > iAddrInitialPC)

            ' Prepare list of CallsFrom for each routine
            For i = 0 To routineCallsFromTo.Count - 1
                Dim routineCall As CallsFromTo = routineCallsFromTo(i)
                Dim validRoutineData As RoutineData = validRoutinesList.Find(Function(c) c.entryPoint = routineCall.toAddress)
                If validRoutineData IsNot Nothing Then
                    Dim routineString As String = routineCall.fromAddress.ToString("X5")
                    If validRoutineData.callsFrom.Count > 0 AndAlso Not validRoutineData.callsFrom(0).Contains(routineString) Then
                        validRoutineData.callsFrom(0) = String.Concat(validRoutineData.callsFrom(0), ", 0x", routineCall.fromAddress.ToString("X5"))
                    ElseIf validRoutineData.callsFrom.Count = 0 Then
                        validRoutineData.callsFrom.Add(String.Concat("Called from routine(s) at 0x", routineCall.fromAddress.ToString("X5")))
                    End If
                End If
            Next

            If Not createGametext Then
                Console.WriteLine("Scanning for routines from:                0x{0:X5}", iCodeBase)
                Console.WriteLine("Found first routine at address:            0x{0:X5}", validRoutinesList(0).entryPoint)
                Console.WriteLine("Lowest routine address (immediate call) :  0x{0:X5}", oDecode.lowest_routine)
                Console.WriteLine("Highest routine address (immediate call):  0x{0:X5}", oDecode.highest_routine)
            End If
            If oDecode.lowest_string < iLengthFile And Not createGametext Then
                Console.WriteLine("Lowest string address (immediate address): 0x{0:X5}", oDecode.lowest_string)
            End If

            ' Find start of strings
            Dim iSOFF As Integer = Helper.GetAdressFromWord(byteStory, &H2A) * 8
            Dim iAddrStrings As Integer = Helper.GetNextValidPackedAddress(byteStory, validRoutinesList.Last.endPoint + 1)
            If iSOFF > iAddrStrings Then iAddrStrings = iSOFF
            memoryMap.Add(New MemoryMapEntry("Z-code", validRoutinesList(0).entryPoint, iAddrStrings - 1, MemoryMapType.MM_ZCODE))
            memoryMap.Add(New MemoryMapEntry("Static strings", iAddrStrings, iLengthFile - 1, MemoryMapType.MM_STATIC_STRINGS))
            If Not createGametext Then
                Console.WriteLine("Strings start at address:                  0x{0:X5}", iAddrStrings)
            End If

            ' Static strings
            Dim stringCount As Integer = 0
            Dim iNextStringAddress As Integer = iAddrStrings
            Dim iNextValidStringAddress As Integer = 0
            Do
                iNextValidStringAddress = Helper.GetNextValidPackedAddress(byteStory, iNextStringAddress)
                Dim count As Integer = -2
                Do
                    count += 2
                Loop Until iNextValidStringAddress + count >= byteStory.Length OrElse (byteStory(iNextValidStringAddress + count) And 128) = 128
                If iNextValidStringAddress + count >= byteStory.Length Then Exit Do
                stringCount += 1
                Dim oStringData As New StringData With {
                .number = stringCount,
                .name = "S" & stringCount.ToString.PadLeft(4, "0"c),
                .text = ExtractZString(iNextValidStringAddress),
                .textWithAbbrevs = ExtractZString(iNextValidStringAddress, True),
                .entryPoint = iNextValidStringAddress,
                .entryPointPacked = (iNextValidStringAddress - iSOFF) \ scaler
            }
                validStringsList.Add(oStringData)
                iNextStringAddress = iNextValidStringAddress + count + 2
            Loop Until Helper.GetNextValidPackedAddress(byteStory, iNextStringAddress) >= iLengthFile - 1

            ' Global variables
            Dim addressGlobalsTableStart As Integer = Helper.GetAdressFromWord(byteStory, 12)
            ' Some compilers (Zilf) only reserves space for actual used global variables. Therefore lets search for nearest identified
            ' memory block and asume that that's the required space. The reserved space can never be more than 480 bytes (240 globals).
            If Not createGametext And oDecode.highest_global <> -1 Then
                Console.WriteLine("Highest used global in z-code:             {0}", oDecode.highest_global + 1)
                Console.WriteLine("Number of used globals in z-code:          {0}", oDecode.usedGlobals.Count)
            End If
            Dim addressGlobalsTableEnd As Integer = byteStory.Length
            For i = 0 To memoryMap.Count - 1
                If memoryMap(i).addressStart > addressGlobalsTableStart And memoryMap(i).addressStart < addressGlobalsTableEnd Then addressGlobalsTableEnd = memoryMap(i).addressStart
            Next
            addressGlobalsTableEnd -= 1
            ' Dialog always compact its globals
            If compilerSource = EnumCompilerSource.DIALOG Then addressGlobalsTableEnd = addressGlobalsTableStart + (oDecode.highest_global + 1) * 2 - 1
            If addressGlobalsTableEnd - addressGlobalsTableStart > 479 Then addressGlobalsTableEnd = addressGlobalsTableStart + 479
            If compilerSource = EnumCompilerSource.INFORM6 Then addressGlobalsTableEnd = addressGlobalsTableStart + 2 * oDecode.highest_global + 1  ' ZCODE_COMPACT_GLOBALS
            memoryMap.Add(New MemoryMapEntry("Global variables", addressGlobalsTableStart, addressGlobalsTableEnd, MemoryMapType.MM_GLOBAL_VARIABLES))

            ' Save possible addresses of arrays (loadb, loadw, storeb, storew & globals
            For i As Integer = addressGlobalsTableStart To addressGlobalsTableEnd Step 2
                oDecode.arraysStart.Add(Helper.GetAdressFromWord(byteStory, i))
            Next
            Dim possibleStartOfArrays As List(Of Integer) = oDecode.arraysStart.ToList
            possibleStartOfArrays.Sort()

            ' Find action table
            Dim iAddrActionTable As Integer = 0
            Dim iAddrPreActionTable As Integer = 0
            Dim iAddrAdjectiveTable As Integer = 0
            If iVerbActionCount > 0 And iVerbCount > 0 Then
                If compilerSource = EnumCompilerSource.INFORM6 And grammarVer = 3 Then
                    iAddrActionTable = iAddrGrammarDataEnd + 1
                    iAddrPreActionTable = iAddrActionTable + 2 * iVerbActionCount
                    iAddrAdjectiveTable = iAddrPreActionTable + 2 * resultGrammarScan.NumberOfParsingRoutines
                    memoryMap.Add(New MemoryMapEntry("Action table", iAddrActionTable, iAddrActionTable + 2 * iVerbActionCount - 1, MemoryMapType.MM_ACTION_TABLE))
                    memoryMap.Add(New MemoryMapEntry("Parsing routines table", iAddrPreActionTable, iAddrAdjectiveTable - 1, MemoryMapType.MM_PREACTION_PARSING_TABLE))
                    memoryMap.Add(New MemoryMapEntry("Preposition/Adjective table", iAddrAdjectiveTable, iAddrAdjectiveTable + 2 + 2 * resultGrammarScan.NumberOfPrepositions - 1, MemoryMapType.MM_PREPOSITION_TABLE))
                Else
                    If {EnumCompilerSource.INFORM5, EnumCompilerSource.INFORM6}.Contains(compilerSource) And grammarVer = 1 Then
                        ' Preposition (adjective) table is only relevant for Inform, grammar ver 1. In ver 2 it's two 00-bytes after action-table
                        If DictEntriesList.PrepositionCountUnique = 0 Then
                            iAddrAdjectiveTable = iAddrDictionary - 2 ' If there are no prepositions, there are two 00-bytes before dictionary
                        Else
                            iAddrAdjectiveTable = ScanForInformV1AdjectiveTable(DictEntriesList)
                        End If
                        If iAddrAdjectiveTable > 0 Then
                            ' Word preceding table contains number of prepositions
                            memoryMap.Add(New MemoryMapEntry("Preposition/Adjective table", iAddrAdjectiveTable, iAddrAdjectiveTable + 2 + 4 * DictEntriesList.PrepositionCountUnique - 1, MemoryMapType.MM_PREPOSITION_TABLE))
                        End If
                    ElseIf grammarVer = 1 Then
                        iAddrAdjectiveTable = ScanForZilV1PrepositionTable(DictEntriesList, DictEntriesList.v1_CompactVocabulary)
                        If iAddrAdjectiveTable > 0 Then
                            Dim numberOfEntries As Integer = Helper.GetAdressFromWord(byteStory, iAddrAdjectiveTable)
                            Dim lengthOfEntries As Integer = 4
                            If DictEntriesList.v1_CompactVocabulary Then lengthOfEntries = 3
                            memoryMap.Add(New MemoryMapEntry("Preposition/Adjective table", iAddrAdjectiveTable, iAddrAdjectiveTable + numberOfEntries * lengthOfEntries + 1, MemoryMapType.MM_PREPOSITION_TABLE))
                        End If
                    End If

                    ' First search for action table from grammar table and forward
                    iAddrActionTable = ScanForActionTable(iVerbActionCount, validRoutinesList, iAddrGrammarTableStart)
                    ' If failed, retry from start
                    If iAddrActionTable = 0 Then iAddrActionTable = ScanForActionTable(iVerbActionCount, validRoutinesList, 0)
                    If iAddrActionTable > 0 Then
                        memoryMap.Add(New MemoryMapEntry("Action table", iAddrActionTable, iAddrActionTable + 2 * iVerbActionCount - 1, MemoryMapType.MM_ACTION_TABLE))
                        ' Pre-action table follows immediately for grammar version 1 and ZIL (grammar version 2 don't have a pre-action table)
                        If {EnumCompilerSource.INFORM5, EnumCompilerSource.INFORM6, EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF}.Contains(compilerSource) Then
                            iAddrPreActionTable = iAddrActionTable + 2 * iVerbActionCount
                            If {EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF}.Contains(compilerSource) Then
                                ' Preaction table is of the same size as the action table in ZIL.
                                memoryMap.Add(New MemoryMapEntry("Preaction table", iAddrPreActionTable, iAddrPreActionTable + 2 * iVerbActionCount - 1, MemoryMapType.MM_PREACTION_TABLE))
                            ElseIf grammarVer = 1 Then
                                ' Inform, grammar ver 1
                                ' Preaction is used for parsing routines and only used in ver 1 grammar
                                ' Preaction table is either the size of actual parsing routines * 2 (Inform6)
                                ' or the same size as the action table (Inform5) if there is room for it before the start of the
                                ' adjective table. 
                                Dim iPreactionSize As Integer = 2 * iVerbActionCount
                                If iAddrAdjectiveTable - iAddrPreActionTable < iPreactionSize Then iPreactionSize = iAddrAdjectiveTable - iAddrPreActionTable
                                memoryMap.Add(New MemoryMapEntry("Parsing routines table", iAddrPreActionTable, iAddrPreActionTable + iPreactionSize - 1, MemoryMapType.MM_PREACTION_PARSING_TABLE))
                            ElseIf grammarVer = 2 Then
                                ' Inform, grammar ver 2
                                ' Don't use either preaction or preposition table. There is two 00 bytes as placeholder for preposition table, though
                                memoryMap.Add(New MemoryMapEntry("Preposition/Adjective table", iAddrPreActionTable, iAddrPreActionTable + 1, MemoryMapType.MM_PREPOSITION_TABLE))
                            End If
                        End If
                    End If
                End If
            End If

            ' More stuff to resolve Zilch/Zilf version 2 parser
            If {EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF}.Contains(compilerSource) AndAlso grammarVer = 2 Then
                ' Build lookup-table of all the found PARTS-OF-SPEECH. The order it is added is important so as don't classify wrong for example
                ' class all MISCWORDs as APOSTR for Zork 0 just because Zork 0 don't use APOSTR and class them as MISCWORD.
                If DictEntriesList.v2_VerbWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_VerbWord, "VERB")
                If DictEntriesList.v2_NounWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_NounWord, "NOUN")
                If DictEntriesList.v2_AdjWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_AdjWord, "ADJ")
                If DictEntriesList.v2_QuantWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_QuantWord, "QUANT")
                If DictEntriesList.v2_MiscWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_MiscWord, "MISCWORD")
                If DictEntriesList.v2_CommaWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_CommaWord, "COMMA")
                If DictEntriesList.v2_ParticleWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_ParticleWord, "PARTICLE")
                If DictEntriesList.v2_PrepWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_PrepWord, "PREP")
                If DictEntriesList.v2_QuoteWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_QuoteWord, "QUOTE")
                If DictEntriesList.v2_EOIWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_EOIWord, "END-OF-INPUT")
                If DictEntriesList.v2_DirWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_DirWord, "DIR")
                If DictEntriesList.v2_CanDoWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_CanDoWord, "CANDO")
                If DictEntriesList.v2_QWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_QWord, "QWORD")
                If DictEntriesList.v2_AskWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_AskWord, "ASKWORD")
                ' Do these last
                If DictEntriesList.v2_AdvWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_AdvWord, "ADV")
                If DictEntriesList.v2_ToBeWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_ToBeWord, "TOBE")
                If DictEntriesList.v2_ApostrWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_ApostrWord, "APOSTR")
                If DictEntriesList.v2_OfWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_OfWord, "OFWORD")
                If DictEntriesList.v2_ArticleWord > 0 Then DictEntriesList.v2_PartsOfSpeech.TryAdd(DictEntriesList.v2_ArticleWord, "ARTICLE")

                ' Resolve thw word-flags from WORD-FLAGS-TABLE
                If DictEntriesList.v2_WordFlagsInTable Then
                    ' The WORD-FLAG-TABLE is a table with first 2-byte word containing the length of the table. Thereafter follows pairs of
                    ' two 2-byte words with the first 2 bytes containing the word-address and the second 2 bytes containing the flags for
                    ' that word.
                    ' In Zilch the table i placed immediately before the syntax/grammar table and in Zilf it's placed immediately before
                    ' the action table. Zilch isn't entirely consistent with the length and it can differ by one with the actual length.
                    Dim endAddress As Integer = iAddrGrammarTableStart
                    If compilerSource = EnumCompilerSource.ZILF Then endAddress = iAddrActionTable
                    Dim startAddress As Integer = endAddress
                    Dim flagCount As Integer = 0
                    If Helper.GetAdressFromWord(byteStory, endAddress - 2) > 0 Then
                        Do
                            startAddress -= 4
                            Dim dictWord As DictionaryEntry = DictEntriesList.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, startAddress))
                            If dictWord IsNot Nothing Then
                                flagCount += 1
                                dictWord.v2_WordFlags = Helper.GetAdressFromWord(byteStory, startAddress + 2)
                            Else
                                Exit Do
                            End If
                        Loop Until Helper.GetAdressFromWord(byteStory, startAddress - 2) = flagCount * 2 And DictEntriesList.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, startAddress - 4)) Is Nothing
                    End If
                    startAddress -= 2
                    endAddress -= 1
                    memoryMap.Add(New MemoryMapEntry("Word flags table", startAddress, endAddress, MemoryMapType.MM_WORD_FLAGS_TABLE))
                End If
            End If

            ' Add calls from Action Table, Preaction Table/Parsing Routines Table and properties to ValidRoutinesList
            If iAddrActionTable > 0 And iAddrPreActionTable > 0 Then
                Dim actionTable As MemoryMapEntry = memoryMap.Find(Function(c) c.type = MemoryMapType.MM_ACTION_TABLE)
                Dim actionNum As Integer = 0
                For i As Integer = actionTable.addressStart To actionTable.addressEnd Step 2
                    Dim address As Integer = Helper.GetAdressFromPacked(byteStory, i, True)
                    Dim oRoutineData As RoutineData = validRoutinesList.Find(Function(c) c.entryPoint = address)
                    If oRoutineData IsNot Nothing Then oRoutineData.callsFrom.Add(String.Concat("Called from action #", actionNum))
                    actionNum += 1
                Next
                Dim preactionTable As MemoryMapEntry = memoryMap.Find(Function(c) c.type = MemoryMapType.MM_PREACTION_TABLE)
                If preactionTable Is Nothing Then preactionTable = memoryMap.Find(Function(c) c.type = MemoryMapType.MM_PREACTION_PARSING_TABLE)
                If preactionTable IsNot Nothing Then
                    Dim preactionNum As Integer = 0
                    For i As Integer = preactionTable.addressStart To preactionTable.addressEnd Step 2
                        Dim address As Integer = Helper.GetAdressFromPacked(byteStory, i, True)
                        Dim oRoutineData As RoutineData = validRoutinesList.Find(Function(c) c.entryPoint = address)
                        If oRoutineData IsNot Nothing And preactionTable.type = MemoryMapType.MM_PREACTION_TABLE Then oRoutineData.callsFrom.Add(String.Concat("Called from preaction #", preactionNum))
                        If oRoutineData IsNot Nothing And preactionTable.type = MemoryMapType.MM_PREACTION_PARSING_TABLE Then oRoutineData.callsFrom.Add(String.Concat("Called from parsing routine #", preactionNum))
                        preactionNum += 1
                    Next
                End If

                ' Loop through properties
                Dim iObject As Integer = 0
                Dim iAddrProperties As Integer = 0
                Dim iAddrObjectProperties As Integer = 65536
                Do
                    iAddrProperties = byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 12) * 256 + byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 13)
                    If iZVersion <= 3 Then iAddrProperties = byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 7) * 256 + byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 8)
                    If iAddrProperties < iAddrObjectProperties Then iAddrObjectProperties = iAddrProperties
                    Dim iPropDescLen As Integer = byteStory(iAddrProperties)
                    Dim objectName As String = "no-name"
                    If iPropDescLen > 0 Then objectName = ExtractZString(iAddrProperties + 1, False)
                    Dim iTmpProp As Integer = iAddrProperties + 2 * iPropDescLen + 1
                    Do While byteStory(iTmpProp) <> 0
                        Dim iPropNum As Integer = 0
                        Dim iPropSize As Integer = 0
                        Dim iPropStart As Integer = iTmpProp + 1
                        Dim sPropData As String = ""
                        If iZVersion <= 3 Then
                            iPropNum = (byteStory(iTmpProp) And &H1F)
                            iPropSize = 1 + (byteStory(iTmpProp) And 224) \ 32
                        Else
                            iPropNum = (byteStory(iTmpProp) And &H3F)
                            If (byteStory(iTmpProp) And &H80) = &H80 Then
                                iPropSize = (byteStory(iTmpProp + 1) And &H3F)
                                If iPropSize = 0 Then iPropSize = &H40
                                iPropStart += 1
                            Else
                                If (byteStory(iTmpProp) And 64) = 64 Then iPropSize = 2 Else iPropSize = 1
                            End If
                        End If
                        DecodePropertyData(True, byteStory, DictEntriesList, validStringsList, validRoutinesList, iPropNum, iPropSize, iPropStart, sPropData, iObject, objectName)
                        iTmpProp = iPropStart + iPropSize
                    Loop
                    iObject += 1
                Loop Until (iAddrObjectTreeStart + iObject * iObjectTreeEntryLen) >= iAddrObjectProperties
            End If

            ' Loop through object with properties
            Dim objectList As New List(Of ObjectData)
            If iObjectCount > 0 Then
                Dim iObject As Integer = 0
                Dim iAddrProperties As Integer = 0
                Dim iAddrObjectProperties As Integer = 65536
                Do
                    Dim objectData As New ObjectData With {.Id = iObject + 1}
                    If iZVersion <= 3 Then
                        objectData.ParentId = byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 4)
                        objectData.NextId = byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 5)
                        objectData.ChildId = byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 6)
                        iAddrProperties = byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 7) * 256 + byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 8)
                    Else
                        objectData.ParentId = byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 6) * 256 + byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 7)
                        objectData.NextId = byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 8) * 256 + byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 9)
                        objectData.ChildId = byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 10) * 256 + byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 11)
                        iAddrProperties = byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 12) * 256 + byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 13)
                    End If
                    If iAddrProperties < iAddrObjectProperties Then iAddrObjectProperties = iAddrProperties
                    Dim iPropDescLen As Integer = byteStory(iAddrProperties)
                    If iPropDescLen > 0 Then objectData.Description = ExtractZString(iAddrProperties + 1, False)
                    Dim iTmpProp As Integer = iAddrProperties + 2 * iPropDescLen + 1
                    Do While byteStory(iTmpProp) <> 0
                        Dim iPropNum As Integer = 0
                        Dim iPropSize As Integer = 0
                        Dim iPropStart As Integer = iTmpProp + 1
                        Dim sPropData As String = ""
                        If iZVersion <= 3 Then
                            iPropNum = (byteStory(iTmpProp) And &H1F)
                            iPropSize = 1 + (byteStory(iTmpProp) And 224) \ 32
                        Else
                            iPropNum = (byteStory(iTmpProp) And &H3F)
                            If (byteStory(iTmpProp) And &H80) = &H80 Then
                                iPropSize = (byteStory(iTmpProp + 1) And &H3F)
                                If iPropSize = 0 Then iPropSize = &H40
                                iPropStart += 1
                            Else
                                If (byteStory(iTmpProp) And 64) = 64 Then iPropSize = 2 Else iPropSize = 1
                            End If
                        End If

                        ' Potential InvisiClues-file.
                        ' The PRIZM project made z5 of all InvisiClues booklets
                        ' Every Menu-item are objects (two levels) and the individual hints are on 
                        ' property 8 (level 3) and on property 8 on an eventual child node.
                        ' Property 4 have containts eventual descriptive text for sub-item menu (level 2). 
                        If iPropNum = 4 Or iPropNum = 8 Then
                            For i As Integer = 0 To iPropSize - 2 Step 2
                                Dim position As Integer = i
                                Dim oStringData As StringData = validStringsList.Find(Function(c) c.entryPointPacked = Helper.GetAdressFromWord(byteStory, iPropStart + position))
                                If oStringData IsNot Nothing Then
                                    If iPropNum = 4 Then objectData.Property4Strings.Add(oStringData.text)
                                    If iPropNum = 8 Then objectData.Property8Strings.Add(oStringData.text)
                                End If
                            Next
                        End If

                        iTmpProp = iPropStart + iPropSize
                    Loop
                    objectList.Add(objectData)
                    iObject += 1
                Loop Until (iAddrObjectTreeStart + iObject * iObjectTreeEntryLen) >= iAddrObjectProperties
            End If

            If compilerSource = EnumCompilerSource.DIALOG Then
                ' Dialog Main Heap always follows header and are filled with 0x1F
                ' If IFID is defined it is stored in the 40 first bytes, but then is overwritten during gameplay
                ' So there's overlap of IFID and Main Heap
                Dim mainHeapStart As Integer = &H40
                Dim mainHeapEnd As Integer = mainHeapStart + 40
                Do
                    mainHeapEnd += 1
                Loop Until byteStory(mainHeapEnd) = &H1F And byteStory(mainHeapEnd + 1) = &H3F
                memoryMap.Add(New MemoryMapEntry("Main heap", mainHeapStart, mainHeapEnd, MemoryMapType.MM_MAIN_HEAP))

                ' Dialog Aux & Long-Term Heap immediately follows Main Heap but are filled with 0x3F
                Dim auxHeapStart = mainHeapEnd + 1
                Dim auxHeapEnd = auxHeapStart
                Do
                    auxHeapEnd += 1
                Loop Until byteStory(auxHeapEnd) = &H3F And byteStory(auxHeapEnd + 1) <> &H3F
                memoryMap.Add(New MemoryMapEntry("Aux and long-term heap", auxHeapStart, auxHeapEnd, MemoryMapType.MM_AUX_LT_HEAP))

                ' Before version 0e/01 predicate data is between aux/lt heap and object defaults table
                ' From 0e/01 predicate data is between globals and abbreviations
                Dim predicateStart As Integer = 0
                Dim predicateEnd As Integer = 0
                If sCompilerSource < "Dialog 0e/01" Then
                    For Each mm As MemoryMapEntry In memoryMap
                        If mm.type = MemoryMapType.MM_AUX_LT_HEAP Then predicateStart = mm.addressEnd + 1
                        If mm.type = MemoryMapType.MM_PROPERTY_DEFAULTS_TABLE Then predicateEnd = mm.addressStart - 1
                    Next
                Else
                    For Each mm As MemoryMapEntry In memoryMap
                        If mm.type = MemoryMapType.MM_GLOBAL_VARIABLES Then predicateStart = mm.addressEnd + 1
                        If mm.type = MemoryMapType.MM_ABBREVIATION_STRINGS Then predicateEnd = mm.addressStart - 1
                    Next
                End If
                If predicateEnd > predicateStart Then memoryMap.Add(New MemoryMapEntry("Predicate data", predicateStart, predicateEnd, MemoryMapType.MM_PREDICATE_DATA))

                ' From 0j01 there is a scratch area for dictionary decoding of 12 bytes after abbreviation table
                If sCompilerSource >= "Dialog 0j/01" Then
                    Dim scratchAreaStart As Integer = 0
                    For Each mm As MemoryMapEntry In memoryMap
                        If mm.type = MemoryMapType.MM_ABBREVIATION_TABLE Then scratchAreaStart = mm.addressEnd + 1
                    Next
                    memoryMap.Add(New MemoryMapEntry("Scratch area", scratchAreaStart, scratchAreaStart + 11, MemoryMapType.MM_SCRATCH_AREA))
                End If
            End If

            ' ***** Add unidentified data fragments *****
            ' A large part of the unidewntified data are table (arrays). There's usually a large chunk in dynamic memory for ordinary
            ' tables (arrays) and a large chunk in static memory for pure tables (static arrays).
            ' Note that Infocom often placec their pure tables first in high memory. It doesn't matter much but
            ' in a strict modern way of thinking, the ENDLOD is "wrongly" placed.
            Dim iAddrStaticMem As Integer = Helper.GetAdressFromWord(byteStory, 14)
            Dim iAddrHighMem As Integer = Helper.GetAdressFromWord(byteStory, 4)
            memoryMap.Sort(Function(x, y) x.addressStart.CompareTo(y.addressStart))     ' Order memory map by start-address
            For i As Integer = 1 To memoryMap.Count - 1
                Dim addressStart As Integer = memoryMap(i - 1).addressEnd + 1
                Dim addressEnd As Integer = memoryMap(i).addressStart - 1
                If memoryMap(i).addressStart - memoryMap(i - 1).addressEnd > 1 Then
                    Dim areaDescription As String = "Unidentified data"
                    If compilerSource = EnumCompilerSource.INFORM6 Then
                        If memoryMap(i - 1).type = MemoryMapType.MM_OBJECT_PROPERTIES_TABLES Then areaDescription += " (Class, indiv. prop & symbol table)"
                        If addressStart < iAddrStaticMem And (memoryMap(i - 1).type = MemoryMapType.MM_GLOBAL_VARIABLES Or memoryMap(i - 1).type = MemoryMapType.MM_IFID) Then areaDescription += " (Arrays)"
                        If addressStart > iAddrStaticMem And (memoryMap(i - 1).type = MemoryMapType.MM_DICTIONARY Or memoryMap(i - 1).type = MemoryMapType.MM_IFID) Then areaDescription += " (Static arrays)"
                    End If
                    If iAddrStaticMem > addressStart And iAddrStaticMem < addressEnd Then
                        ' Unidentified data straddles static memory start, split it in two
                        memoryMap.Add(New MemoryMapEntry(areaDescription, addressStart, iAddrStaticMem - 1, MemoryMapType.MM_UNIDENTIFIED_DATA))
                        memoryMap.Add(New MemoryMapEntry(areaDescription, iAddrStaticMem, addressEnd, MemoryMapType.MM_UNIDENTIFIED_DATA))
                    ElseIf iAddrHighMem > addressStart And iAddrHighMem < addressEnd Then
                        ' Unidentified data straddles high memory start, split it in two
                        memoryMap.Add(New MemoryMapEntry(areaDescription, addressStart, iAddrHighMem - 1, MemoryMapType.MM_UNIDENTIFIED_DATA))
                        memoryMap.Add(New MemoryMapEntry(areaDescription, iAddrHighMem, addressEnd, MemoryMapType.MM_UNIDENTIFIED_DATA))
                    Else
                        memoryMap.Add(New MemoryMapEntry(areaDescription, addressStart, addressEnd, MemoryMapType.MM_UNIDENTIFIED_DATA))
                    End If
                End If
            Next
            ' Check that unidentified data around high memory start isn't just padding 
            memoryMap.Sort(Function(x, y) x.addressStart.CompareTo(y.addressStart))     ' Order memory map by start-address
            For i As Integer = 1 To memoryMap.Count - 1
                If memoryMap(i).type = MemoryMapType.MM_UNIDENTIFIED_DATA Then
                    Dim maxPaddingSize As Integer = 2
                    If iZVersion > 3 Then maxPaddingSize *= 2
                    If iZVersion > 7 Then maxPaddingSize *= 2
                    If memoryMap(i).addressStart = iAddrHighMem Or memoryMap(i).addressEnd = iAddrHighMem - 1 Then
                        If memoryMap(i).addressEnd - memoryMap(i).addressStart + 1 < maxPaddingSize Then
                            Dim sumBytes As Integer = 0
                            For j = memoryMap(i).addressStart To memoryMap(i).addressEnd
                                sumBytes += byteStory(j)
                            Next
                            If sumBytes = 0 Then
                                memoryMap(i).name = "Padding"
                                memoryMap(i).type = MemoryMapType.MM_PADDING
                            End If
                        End If
                    End If
                    If compilerSource = EnumCompilerSource.DIALOG And memoryMap(i).addressEnd = iAddrStaticMem - 1 Then
                        ' Dialog always pad so that Static memory starts at least 480 bytes after globals start.
                        memoryMap(i).name = "Padding"
                        memoryMap(i).type = MemoryMapType.MM_PADDING
                    End If
                End If
            Next

            ' Analyse Properties when dictionary, routines and strings are identified
            oPropertyAnalyser.Analyse(DictEntriesList, validStringsList, validRoutinesList, memoryMap)
            If Not createGametext Then
                Console.WriteLine("Number of unique properties:               {0}", oPropertyAnalyser.Count)

                ' Todo: Skipping printing property analysis for the moment
                'For i = oPropertyAnalyser.propertyNumberMax To oPropertyAnalyser.propertyNumberMin Step -1
                '    If Not oPropertyAnalyser.GetPropertyType(i) = ePropertyType.UNUSED And Not oPropertyAnalyser.GetPropertyType(i) = ePropertyType.UNINITIATED Then
                '        If Not oPropertyAnalyser.GetPropertyType(i) = ePropertyType.AMBIGUITY Then
                '            Console.WriteLine("   Property {0:D2}:                        {1}", i, oPropertyAnalyser.GetPropertyTypeName(oPropertyAnalyser.GetPropertyType(i)))
                '        Else
                '            Console.Write("   Property {0:D2}:                        {1} (", i, oPropertyAnalyser.GetPropertyTypeName(oPropertyAnalyser.GetPropertyType(i)))
                '            For Each oTmpProp As ePropertyType In oPropertyAnalyser.GetPropertyAmbiguityTypes(i)
                '                Console.Write("{0}, ", oPropertyAnalyser.GetPropertyTypeName(oTmpProp))
                '            Next
                '            Console.WriteLine()
                '        End If
                '    End If
                'Next
            End If

            ' ***** Print memory map *****
            Dim iStaticMemEnd As Integer = iAddrHighMem - 1

            memoryMap.Sort(Function(x, y) x.addressStart.CompareTo(y.addressStart))     ' Order memory map by start-address
            If showMemoryMap And Not createGametext Then
                Console.WriteLine()
                Console.WriteLine("***** MEMORY MAP *****")
                Console.WriteLine()
                Console.WriteLine("00000-{0:X5} DYNAMIC MEMORY", iAddrStaticMem - 1)
                Dim iPhase As Integer = 0
                Dim bPrinted As Boolean = False
                For Each oMemEntry As MemoryMapEntry In memoryMap
                    If iPhase = 0 Then ' Dynamic
                        If oMemEntry.addressEnd < iAddrStaticMem Then
                            Console.WriteLine("   {0:X5}-{1:X5} {2}", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.name & ", " & oMemEntry.SizeString & ".")
                        ElseIf oMemEntry.addressStart < iAddrStaticMem And oMemEntry.addressEnd > iAddrStaticMem Then
                            Console.WriteLine("   {0:X5}-{1:X5} {2}", oMemEntry.addressStart, iAddrStaticMem - 1, oMemEntry.name & ", " & (iAddrStaticMem - oMemEntry.addressStart).ToString("#,##0") & ".")
                            Console.WriteLine()
                            Console.WriteLine("{0:X5}-{1:X5} STATIC MEMORY", iAddrStaticMem, iStaticMemEnd)
                            Console.WriteLine("   {0:X5}-{1:X5} {2}", iAddrStaticMem, oMemEntry.addressEnd, oMemEntry.name & ", " & (oMemEntry.addressEnd - iAddrStaticMem + 1).ToString("#,##0") & ".")
                            bPrinted = True
                            iPhase += 1
                        ElseIf oMemEntry.addressStart >= iAddrStaticMem Then
                            Console.WriteLine()
                            Console.WriteLine("{0:X5}-{1:X5} STATIC MEMORY", iAddrStaticMem, iStaticMemEnd)
                            iPhase += 1
                        End If
                    End If
                    If iPhase = 1 Then ' Static
                        If bPrinted Then
                            bPrinted = False
                        Else
                            If oMemEntry.addressEnd < iAddrHighMem Then
                                Console.WriteLine("   {0:X5}-{1:X5} {2}", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.name & ", " & oMemEntry.SizeString & ".")
                            ElseIf oMemEntry.addressStart < iAddrHighMem And oMemEntry.addressEnd > iAddrHighMem Then
                                Console.WriteLine("   {0:X5}-{1:X5} {2}", oMemEntry.addressStart, iAddrHighMem - 1, oMemEntry.name & ", " & oMemEntry.SizeString & ".")
                                Console.WriteLine()
                                Console.WriteLine("{0:X5}-{1:X5} HIGH MEMORY", iAddrHighMem, iLengthFile - 1)
                                Console.WriteLine("   {0:X5}-{1:X5} {2}", iAddrHighMem, oMemEntry.addressEnd, oMemEntry.name & ", " & oMemEntry.SizeString & ".")
                                bPrinted = True
                                iPhase += 1
                            ElseIf oMemEntry.addressStart >= iAddrHighMem Then
                                Console.WriteLine()
                                Console.WriteLine("{0:X5}-{1:X5} HIGH MEMORY", iAddrHighMem, iLengthFile - 1)
                                iPhase += 1
                            End If

                        End If
                    End If
                    If iPhase = 2 Then ' High
                        If bPrinted Then
                            bPrinted = False
                        Else
                            Console.WriteLine("   {0:X5}-{1:X5} {2}", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.name & ", " & oMemEntry.SizeString & ".")
                        End If
                    End If
                Next
                Console.WriteLine()
            End If

            ' Build verbGrammarList
            Dim verbGrammarList As New Dictionary(Of Integer, List(Of String))
            If iAddrActionTable > 0 And iAddrPreActionTable > 0 And iAddrGrammarTableStart > 0 And (iAddrGrammarDataStart > 0 Or compilerSource = EnumCompilerSource.ZILF) Then
                Dim iVerbNum As Integer = 255
                Do
                    Dim iAddrSyntax As Integer = iAddrGrammarTableStart + (255 - iVerbNum) * 2
                    iAddrSyntax = byteStory(iAddrSyntax) * 256 + byteStory(iAddrSyntax + 1)
                    Dim iSyntaxLines As Integer = byteStory(iAddrSyntax)
                    If grammarVer = 1 And iAddrAdjectiveTable > 0 Then
                        Dim iStartAddress As Integer = iAddrSyntax + 1
                        For i As Integer = 1 To iSyntaxLines
                            Dim grammarStrings As List(Of String) = Nothing
                            If compilerSource = EnumCompilerSource.INFORM5 Or compilerSource = EnumCompilerSource.INFORM6 Or compilerSource = EnumCompilerSource.ZILF Or compilerSource = EnumCompilerSource.ZILCH Then
                                Dim actionNumber As Integer = byteStory(iStartAddress + 7)
                                Dim syntaxLineLength As Integer = 8
                                If compactSyntaxes Then
                                    actionNumber = byteStory(iStartAddress + 1)
                                    syntaxLineLength = 2
                                    Select Case (byteStory(iStartAddress) And &HC0)
                                        Case &H40 : syntaxLineLength = 4
                                        Case &H80 : syntaxLineLength = 7
                                    End Select
                                End If
                                Dim grammarString As String
                                If compilerSource = EnumCompilerSource.INFORM5 Or compilerSource = EnumCompilerSource.INFORM6 Then
                                    grammarString = DecodeGrammarsInformV1(iStartAddress, DictEntriesList, iAddrActionTable, iAddrPreActionTable, iAddrAdjectiveTable)
                                Else
                                    grammarString = DecodeGrammarsZilV1(iStartAddress, DictEntriesList, iAddrActionTable, iAddrPreActionTable, iAddrAdjectiveTable, compactSyntaxes)
                                End If
                                If Not verbGrammarList.TryGetValue(actionNumber, grammarStrings) Then
                                    grammarStrings = New List(Of String)
                                    verbGrammarList.Add(actionNumber, grammarStrings)
                                End If
                                Try
                                    If compilerSource = EnumCompilerSource.ZILCH Or compilerSource = EnumCompilerSource.ZILF Then
                                        grammarStrings.Add(grammarString.Replace("*", DictEntriesList.GetVerb(iVerbNum)(0).dictWord.ToUpper))
                                    Else
                                        grammarStrings.Add(grammarString.Replace("*", DictEntriesList.GetVerb(iVerbNum)(0).dictWord))
                                    End If
                                Catch
                                    If compilerSource = EnumCompilerSource.ZILCH Or compilerSource = EnumCompilerSource.ZILF Then
                                        grammarStrings.Add(grammarString.Replace("*", "NO-VERB"))
                                    Else
                                        grammarStrings.Add(grammarString.Replace("*", "no-verb"))
                                    End If
                                End Try
                                iStartAddress += syntaxLineLength
                            End If
                        Next
                    End If
                    If grammarVer = 2 And (compilerSource = EnumCompilerSource.INFORM5 Or compilerSource = EnumCompilerSource.INFORM6) Then
                        Dim grammarStrings As List(Of String) = Nothing

                        If iSyntaxLines > 0 Then
                            Dim iCurrentLine As Integer = 0
                            Dim iCurrentAddress As Integer = iAddrSyntax + 1
                            Dim iStartAddress As Integer = 0
                            Do
                                iCurrentLine += 1
                                iStartAddress = iCurrentAddress
                                iCurrentAddress += 2
                                Dim actionNumber As Integer = (Helper.GetAdressFromWord(byteStory, iStartAddress) And &H3FF)
                                Dim grammarString As String = DecodeGrammarsInformV2(iStartAddress, DictEntriesList, iAddrActionTable)
                                If Not verbGrammarList.TryGetValue(actionNumber, grammarStrings) Then
                                    grammarStrings = New List(Of String)
                                    verbGrammarList.Add(actionNumber, grammarStrings)
                                End If
                                Try
                                    grammarStrings.Add(grammarString.Replace("*", DictEntriesList.GetVerb(iVerbNum)(0).dictWord))
                                Catch
                                    grammarStrings.Add(grammarString.Replace("*", "no-verb"))
                                End Try
                                If Not byteStory(iCurrentAddress) = &HF Then
                                    Do
                                        iCurrentAddress += 3
                                    Loop Until byteStory(iCurrentAddress) = &HF
                                End If
                                iCurrentAddress += 1
                            Loop Until iCurrentLine >= iSyntaxLines
                        End If
                    End If
                    If grammarVer = 3 Then
                        Dim grammarStrings As List(Of String) = Nothing

                        If iSyntaxLines > 0 Then
                            Dim iCurrentLine As Integer = 0
                            Dim iCurrentAddress As Integer = iAddrSyntax + 1
                            Dim iStartAddress As Integer = 0
                            Do
                                iCurrentLine += 1
                                iStartAddress = iCurrentAddress
                                iCurrentAddress += 2
                                Dim actionNumber As Integer = (Helper.GetAdressFromWord(byteStory, iStartAddress) And &H3FF)
                                Dim tokens As Integer = (Helper.GetAdressFromWord(byteStory, iStartAddress) And &HF800) >> 11
                                Dim grammarString As String = DecodeGrammarsInformV3(iStartAddress, DictEntriesList, iAddrActionTable, iAddrPreActionTable, iAddrAdjectiveTable)
                                If Not verbGrammarList.TryGetValue(actionNumber, grammarStrings) Then
                                    grammarStrings = New List(Of String)
                                    verbGrammarList.Add(actionNumber, grammarStrings)
                                End If
                                Try
                                    grammarStrings.Add(grammarString.Replace("*", DictEntriesList.GetVerb(iVerbNum)(0).dictWord))
                                Catch
                                    grammarStrings.Add(grammarString.Replace("*", "no-verb"))
                                End Try
                                iCurrentAddress += tokens * 2
                            Loop Until iCurrentLine >= iSyntaxLines
                        End If

                    End If
                    iVerbNum -= 1
                Loop Until iVerbNum < DictEntriesList.GetLowestVerbNum
                If grammarVer = 2 And (compilerSource = EnumCompilerSource.ZILCH Or compilerSource = EnumCompilerSource.ZILF) Then
                    DictEntriesList.v2_VerbAddresses.Sort()
                    For i As Integer = 0 To DictEntriesList.v2_VerbAddresses.Count - 1
                        Dim address As Integer = DictEntriesList.v2_VerbAddresses(i)
                        Dim verbWord As String = ""
                        For Each dictWord As DictionaryEntry In DictEntriesList
                            If (dictWord.v2_ClassificationNumber And DictEntriesList.v2_VerbWord) = DictEntriesList.v2_VerbWord And dictWord.v2_SemanticStuff = address Then
                                verbWord = dictWord.dictWord.ToUpper
                                Exit For
                            End If
                        Next
                        Dim nullObjAction As Integer = Helper.GetAdressFromWord(byteStory, address)
                        Dim nullObjPrep As Integer = Helper.GetAdressFromWord(byteStory, address + 2)
                        Dim addressOneObj As Integer = Helper.GetAdressFromWord(byteStory, address + 4)
                        Dim addressTwoObj As Integer = Helper.GetAdressFromWord(byteStory, address + 6)
                        If Not nullObjAction = &HFFFF Then
                            Dim syntax As String = DecodeGrammarsZilV2(address, DictEntriesList, iAddrActionTable, iAddrPreActionTable, 0)
                            Dim grammarStrings As List(Of String) = Nothing
                            If Not verbGrammarList.TryGetValue(nullObjAction, grammarStrings) Then
                                grammarStrings = New List(Of String)
                                verbGrammarList.Add(nullObjAction, grammarStrings)
                            End If
                            grammarStrings.Add(syntax.Replace("*", verbWord))
                        End If
                        If addressOneObj > 0 Then
                            Dim numberOfEntries As Integer = Helper.GetAdressFromWord(byteStory, addressOneObj)
                            For j As Integer = 0 To numberOfEntries - 1
                                Dim syntax As String = DecodeGrammarsZilV2(addressOneObj + 2 + j * 6, DictEntriesList, iAddrActionTable, iAddrPreActionTable, 1)
                                Dim action As Integer = Helper.GetAdressFromWord(byteStory, addressOneObj + 2 + j * 6)
                                Dim grammarStrings As List(Of String) = Nothing
                                If Not verbGrammarList.TryGetValue(action, grammarStrings) Then
                                    grammarStrings = New List(Of String)
                                    verbGrammarList.Add(action, grammarStrings)
                                End If
                                grammarStrings.Add(syntax.Replace("*", verbWord))
                            Next
                        End If
                        If addressTwoObj > 0 Then
                            Dim numberOfEntries As Integer = Helper.GetAdressFromWord(byteStory, addressTwoObj)
                            For j As Integer = 0 To numberOfEntries - 1
                                Dim syntax As String = DecodeGrammarsZilV2(addressTwoObj + 2 + j * 10, DictEntriesList, iAddrActionTable, iAddrPreActionTable, 2)
                                Dim action As Integer = Helper.GetAdressFromWord(byteStory, addressTwoObj + 2 + j * 10)
                                Dim grammarStrings As List(Of String) = Nothing
                                If Not verbGrammarList.TryGetValue(action, grammarStrings) Then
                                    grammarStrings = New List(Of String)
                                    verbGrammarList.Add(action, grammarStrings)
                                End If
                                grammarStrings.Add(syntax.Replace("*", verbWord))
                            Next
                        End If
                    Next
                End If
            End If

            ' **********************
            ' ***** PRINT DATA *****
            ' **********************
            For Each oMemEntry As MemoryMapEntry In memoryMap

                ' Todo: Skip this at the moment
                'If oMemEntry.addressStart = 0 Then oMemEntry.startOfDynamic = True
                'If oMemEntry.addressStart = iAddrStaticMem Then oMemEntry.startOfStatic = True
                'If oMemEntry.addressStart = iAddrHighMem Then oMemEntry.startOfHigh = True

                If oMemEntry.type = MemoryMapType.MM_HEADER_TABLE And showHeader And Not createGametext Then
                    ' ***** HEADER *****
                    ' Always the first 64 bytes
                    oMemEntry.PrintMemoryLabel()
                    Console.WriteLine()
                    Console.WriteLine("***** HEADER (00000-0003F, {0}) *****", oMemEntry.SizeString)
                    If showHex Then HexDump(0, 63, True)
                    Console.WriteLine()
                    Console.WriteLine("00000 {0:X2}                      VERSION Z-machine version:         {1}", byteStory(0), iZVersion)
                    Console.WriteLine("00001 {0:X2}                      MODE    Flags 1:                   0x{1:X2}", byteStory(1), byteStory(1))
                    Console.WriteLine("00002 {0:X2} {1:X2}                   ZORKID  Release number:            {2}", byteStory(2), byteStory(3), Helper.GetAdressFromWord(byteStory, 2))
                    Console.WriteLine("00004 {0:X2} {1:X2}                   ENDLOD  Base of high memory:       0x{2:X4}", byteStory(4), byteStory(5), iAddrHighMem)
                    Console.WriteLine("00006 {0:X2} {1:X2}                   START   Initial value of pc:       0x{2:X4}", byteStory(6), byteStory(7), Helper.GetAdressFromWord(byteStory, 6))
                    Console.WriteLine("00008 {0:X2} {1:X2}                   VOCAB   Dictionary:                0x{2:X4}", byteStory(8), byteStory(9), iAddrDictionary)
                    Console.WriteLine("0000A {0:X2} {1:X2}                   OBJECT  Object table:              0x{2:X4}", byteStory(10), byteStory(11), iAddrObjectDefaultsTableStart)
                    Console.WriteLine("0000C {0:X2} {1:X2}                   GLOBALS Global variables table:    0x{2:X4}", byteStory(12), byteStory(13), addressGlobalsTableStart)
                    Console.WriteLine("0000E {0:X2} {1:X2}                   PURBOT  Base of static memory:     0x{2:X4}", byteStory(14), byteStory(15), iAddrStaticMem)
                    Console.WriteLine("00010 {0:X2} {1:X2}                   FLAGS   Flags 2:                   0x{2:X4}", byteStory(16), byteStory(17), Helper.GetAdressFromWord(byteStory, &H10))
                    Console.WriteLine("00012 {0:X2} {1:X2} {2:X2} {3:X2} {4:X2} {5:X2}       SERIAL  Serial number:             {6}", byteStory(18), byteStory(19), byteStory(20), byteStory(21), byteStory(22), byteStory(23), ExtractASCIIChars(18, 6))
                    Console.WriteLine("00018 {0:X2} {1:X2}                   FWORDS  Abbreviations table:       0x{2:X4}", byteStory(24), byteStory(25), iAddrAbbrevTableStart)
                    Console.WriteLine("0001A {0:X2} {1:X2}                   PLENTH  Length of file:            0x{2:X4}", byteStory(26), byteStory(27), iLengthFile)
                    Console.WriteLine("0001C {0:X2} {1:X2}                   PCHKSM  Checksum of file:          0x{2:X4}", byteStory(28), byteStory(29), Helper.GetAdressFromWord(byteStory, &H1C))
                    Console.WriteLine("0001E {0:X2}                      INTWRD  Interpreter number:        {1}", byteStory(30), byteStory(&H1E))
                    Console.WriteLine("0001F {0:X2}                              Interpreter version:       {1}", byteStory(31), byteStory(&H1F))
                    Console.WriteLine("00020 {0:X2}                      SCRWRD  Screen height (lines):     {1}", byteStory(32), byteStory(&H20))
                    Console.WriteLine("00021 {0:X2}                              Screen width (chars):      {1}", byteStory(33), byteStory(&H21))
                    Console.WriteLine("00022 {0:X2} {1:X2}                   HWRD    Screen width in units:     0x{2:X4}", byteStory(34), byteStory(35), Helper.GetAdressFromWord(byteStory, &H22))
                    Console.WriteLine("00024 {0:X2} {1:X2}                   VWRD    Screen width in units:     0x{2:X4}", byteStory(36), byteStory(37), Helper.GetAdressFromWord(byteStory, &H24))
                    Console.WriteLine("00026 {0:X2}                      FWRD    Font width/height:         {1}", byteStory(38), byteStory(&H26))
                    Console.WriteLine("00027 {0:X2}                              Font width/height:         {1}", byteStory(39), byteStory(&H27))
                    Console.WriteLine("00028 {0:X2} {1:X2}                   FOFF    Routines offset:           0x{2:X4}", byteStory(40), byteStory(41), Helper.GetAdressFromWord(byteStory, &H28) * 8)
                    Console.WriteLine("0002A {0:X2} {1:X2}                   SOFF    Static strings offset:     0x{2:X4}", byteStory(42), byteStory(43), Helper.GetAdressFromWord(byteStory, &H2A) * 8)
                    Console.WriteLine("0002C {0:X2}                      CLRWRD  Default backgr. color:     {1}", byteStory(&H2C), byteStory(&H2C))
                    Console.WriteLine("0002D {0:X2}                              Default foregr. color:     {1}", byteStory(&H2D), byteStory(&H2D))
                    Console.WriteLine("0002E {0:X2} {1:X2}                   TCHARS  Terminating chars table:   0x{2:X4}", byteStory(46), byteStory(47), iAddrTCharsStart)
                    Console.WriteLine("00030 {0:X2} {1:X2}                   TWID    Output loc for DIROUT:     0x{2:X4}", byteStory(48), byteStory(49), Helper.GetAdressFromWord(byteStory, &H30))
                    Console.WriteLine("00032 {0:X2} {1:X2}                           Standard revision number:  0x{2:X4}", byteStory(50), byteStory(51), Helper.GetAdressFromWord(byteStory, &H32))
                    Console.WriteLine("00034 {0:X2} {1:X2}                   CHRSET  Alphabet table address:    0x{2:X4}", byteStory(52), byteStory(53), Helper.GetAdressFromWord(byteStory, &H34))
                    Console.WriteLine("00036 {0:X2} {1:X2}                   EXTAB   Header extension address:  0x{2:X4}", byteStory(54), byteStory(55), iAddrHeaderExtStart)
                    Console.WriteLine("00038 {0:X2} {1:X2} {2:X2} {3:X2} {4:X2} {5:X2} {6:X2} {7:X2} USRNM   Username:                  {8}", byteStory(56), byteStory(57), byteStory(58), byteStory(59), byteStory(60), byteStory(61), byteStory(62), byteStory(63), ExtractASCIIChars(56, 8))
                    Console.WriteLine()
                End If


                If oMemEntry.type = MemoryMapType.MM_HEADER_EXT_TABLE And showHeader And Not createGametext Then
                    ' ***** HEADER EXTENSION TABLE *****
                    If Helper.GetAdressFromWord(byteStory, &H36) > 0 Then
                        oMemEntry.PrintMemoryLabel()
                        Console.WriteLine()
                        Console.WriteLine("***** HEADER EXTENSION TABLE ({0:X5}-{1:X5}, {2}) *****", iAddrHeaderExtStart, iAddrHeaderExtStart + iHeaderExtLen * 2 + 1, oMemEntry.SizeString)
                        If showHex Then HexDump(iAddrHeaderExtStart, iAddrHeaderExtStart + iHeaderExtLen * 2 + 1, True)
                        Console.WriteLine()
                        Console.WriteLine("{0:X5} {1:X2} {2:X2}                   Number of further words:                  {3}", iAddrHeaderExtStart, byteStory(iAddrHeaderExtStart), byteStory(iAddrHeaderExtStart + 1), iHeaderExtLen)
                        For i As Integer = 1 To iHeaderExtLen
                            Select Case i
                                Case 1
                                    Console.WriteLine("{0:X5} {1:X2} {2:X2}                   MSLOCX  X-coord of mouse after a click:    0x{3:X4}", iAddrHeaderExtStart + i * 2, byteStory(iAddrHeaderExtStart + i * 2), byteStory(iAddrHeaderExtStart + i * 2 + 1), Helper.GetAdressFromWord(byteStory, iAddrHeaderExtStart + i * 2))
                                Case 2
                                    Console.WriteLine("{0:X5} {1:X2} {2:X2}                   MSLOCX  Y-coord of mouse after a click:    0x{3:X4}", iAddrHeaderExtStart + i * 2, byteStory(iAddrHeaderExtStart + i * 2), byteStory(iAddrHeaderExtStart + i * 2 + 1), Helper.GetAdressFromWord(byteStory, iAddrHeaderExtStart + i * 2))
                                Case 3
                                    Console.WriteLine("{0:X5} {1:X2} {2:X2}                           Unicode translation table address: 0x{3:X4}", iAddrHeaderExtStart + i * 2, byteStory(iAddrHeaderExtStart + i * 2), byteStory(iAddrHeaderExtStart + i * 2 + 1), Helper.GetAdressFromWord(byteStory, iAddrHeaderExtStart + i * 2))
                                Case 4
                                    Console.WriteLine("{0:X5} {1:X2} {2:X2}                           Flags 3:                           0x{3:X4}", iAddrHeaderExtStart + i * 2, byteStory(iAddrHeaderExtStart + i * 2), byteStory(iAddrHeaderExtStart + i * 2 + 1), Helper.GetAdressFromWord(byteStory, iAddrHeaderExtStart + i * 2))
                                Case 5
                                    Console.WriteLine("{0:X5} {1:X2} {2:X2}                           True default foreground colour:    0x{3:X4}", iAddrHeaderExtStart + i * 2, byteStory(iAddrHeaderExtStart + i * 2), byteStory(iAddrHeaderExtStart + i * 2 + 1), Helper.GetAdressFromWord(byteStory, iAddrHeaderExtStart + i * 2))
                                Case 6
                                    Console.WriteLine("{0:X5} {1:X2} {2:X2}                           True default background colour:    0x{3:X4}", iAddrHeaderExtStart + i * 2, byteStory(iAddrHeaderExtStart + i * 2), byteStory(iAddrHeaderExtStart + i * 2 + 1), Helper.GetAdressFromWord(byteStory, iAddrHeaderExtStart + i * 2))
                                Case Else
                                    Console.WriteLine("{0:X5} {1:X2} {2:X2}                                                              0x{3:X2}", iAddrHeaderExtStart + i * 2, byteStory(iAddrHeaderExtStart + i * 2), byteStory(iAddrHeaderExtStart + i * 2 + 1), Helper.GetAdressFromWord(byteStory, i))
                            End Select
                        Next
                        Console.WriteLine()
                    End If
                End If

                If oMemEntry.type = MemoryMapType.MM_IFID And showExtra And Not createGametext Then
                    ' ***** IFID *****
                    oMemEntry.PrintMemoryLabel()
                    Console.WriteLine()
                    Console.WriteLine("***** IFID ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                    If showHex Then HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                    Console.WriteLine()
                    Dim addressStart As Integer = oMemEntry.addressStart
                    If oMemEntry.addressEnd - addressStart = 45 Then
                        Console.WriteLine("{0:X5} {1:X2}{2}Length of IFID: {3}", addressStart, byteStory(addressStart), Space(43), byteStory(addressStart))
                        addressStart += 1
                    End If
                    Dim count As Integer = 0
                    Console.Write("{0:X5} ", addressStart)
                    For i As Integer = addressStart To oMemEntry.addressEnd
                        Console.Write("{0:X2} ", byteStory(i))
                        count += 1
                        If count = 15 And Not i = oMemEntry.addressEnd Then
                            Console.WriteLine()
                            Console.Write("      ")
                            count = 0
                        End If
                    Next
                    Console.WriteLine("{0}", IFID)
                    Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_UNICODE_TABLE And showExtra And Not createGametext Then
                    ' ***** UNICODE TRANSLATION TABLE *****
                    oMemEntry.PrintMemoryLabel()
                    Console.WriteLine()
                    Console.WriteLine("***** UNICODE TRANSLATION TABLE ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                    If showHex Then HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                    Console.WriteLine()
                    Dim unicodeCount As Integer = byteStory(oMemEntry.addressStart)
                    Console.WriteLine("{0:X5} {1:X2}                       Number of entries: {2}", oMemEntry.addressStart, byteStory(oMemEntry.addressStart), unicodeCount)
                    If unicodeCount > 0 Then
                        For i As Integer = 0 To unicodeCount - 1
                            Dim currentAddress As Integer = oMemEntry.addressStart + 1 + i * 2
                            Dim unicodeVal As Integer = Helper.GetAdressFromWord(byteStory, currentAddress)
                            Console.WriteLine("{0:X5} {1:X2} {2:X2}                    ZSCII #{3} = U+{4:X4} '{5}'", currentAddress, byteStory(currentAddress), byteStory(currentAddress + 1), 155 + i, unicodeVal, Strings.ChrW(unicodeVal))
                        Next
                    End If
                    Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_MAIN_HEAP And showExtra And Not createGametext Then
                    ' ***** MAIN HEAP *****
                    Console.WriteLine("***** MAIN HEAP ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                    HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                    Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_AUX_LT_HEAP And showExtra And Not createGametext Then
                    ' ***** AUX & LONG-TERM HEAP *****
                    Console.WriteLine("***** AUX & LONG-TERM HEAP ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                    HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                    Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_PREDICATE_DATA And showExtra And Not createGametext Then
                    ' ***** PREDICATE DATA *****
                    Console.WriteLine("***** PREDICATE DATA ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                    HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                    Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_SCRATCH_AREA And showExtra And Not createGametext Then
                    ' ***** SCRATCH AREA *****
                    Console.WriteLine("***** SCRATCH AREA ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                    HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                    Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_ABBREVIATION_TABLE And showAbbreviations And Not createGametext Then
                    ' ***** ABBREVIATION TABLE *****
                    oMemEntry.PrintMemoryLabel()
                    Console.WriteLine()
                    Console.WriteLine("***** ABBREVIATION TABLE ({0:X5}-{1:X5}, {2}) *****", iAddrAbbrevTableStart, 2 * maxNumberofAbbrevs - 1, oMemEntry.SizeString)
                    If showHex Then HexDump(iAddrAbbrevTableStart, iAddrAbbrevTableStart + 2 * maxNumberofAbbrevs - 1, True)
                    Dim count As Integer = 0
                    Console.WriteLine()
                    For i As Integer = iAddrAbbrevTableStart To iAddrAbbrevTableStart + 2 * maxNumberofAbbrevs - 1 Step 2
                        Console.WriteLine("{0:X5} {1:X2} {2:X2}                    Address of abbreviation {3,2}: 0x{4:X4}", i, byteStory(i), byteStory(i + 1), count, Helper.GetAdressFromWord(byteStory, i) * 2)
                        count += 1
                    Next
                    Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_ABBREVIATION_STRINGS And showAbbreviations And Not createGametext Then
                    ' ***** ABBREVIATION STRINGS *****
                    oMemEntry.PrintMemoryLabel()
                    Console.WriteLine()
                    Console.WriteLine("***** ABBREVIATION STRINGS ({0:X5}-{1:X5}, {2}) *****", iAddrAbbrevStringsStart, iAddrAbbrevStringsEnd, oMemEntry.SizeString)
                    If showHex Then HexDump(iAddrAbbrevStringsStart, iAddrAbbrevStringsEnd, True)
                    Console.WriteLine()

                    ' Build sorted list of all abbreviations (there's not certain that abbreviations are
                    ' sorted, i.e. that A00 is the first, A02 the second and so on...
                    Dim abbreviationList As New List(Of String)
                    For i As Integer = 0 To (maxNumberofAbbrevs - 1)
                        Dim startAddress As Integer = Helper.GetAdressFromWord(byteStory, iAddrAbbrevTableStart + i * 2) * 2
                        abbreviationList.Add(startAddress.ToString("D5") & ";" & i.ToString("D2"))
                    Next
                    abbreviationList.Add((iAddrAbbrevStringsEnd + 1).ToString("D5"))
                    abbreviationList.Sort()

                    If CInt(abbreviationList(0).Substring(0, 5)) = &H42 And iAddrAbbrevStringsStart = &H40 And byteStory(&H40) = &H80 And byteStory(&H41) = 0 Then
                        Console.WriteLine("00040 80 00                    Unused default string: {0}   {1}", Chr(34), Chr(34))
                    End If

                    ' Print abbreviation strings
                    For i As Integer = 0 To (maxNumberofAbbrevs - 1)
                        Dim startAddress As Integer = CInt(abbreviationList(i).Substring(0, 5))
                        Dim endAddress As Integer = CInt(abbreviationList(i + 1).Substring(0, 5)) - 1
                        Dim abbreviationNumber As Integer = CInt(abbreviationList(i).Substring(6, 2))
                        If endAddress <= startAddress Then endAddress = startAddress + 1              ' Minimum of two bytes
                        Console.Write("{0:X5} ", startAddress)
                        Dim count As Integer = 0
                        For j = startAddress To endAddress
                            Console.Write("{0:X2} ", byteStory(j))
                            count += 1
                            If count = 8 And j < endAddress Then
                                Console.WriteLine()
                                Console.Write(Space(6))
                                count = 0
                            End If
                        Next
                        Console.Write(Space(3 * (8 - count)))
                        Console.Write(" A{0:D2}: {1}", abbreviationNumber, Convert.ToChar(34))
                        Console.Write(sAbbreviations(abbreviationNumber))
                        Console.WriteLine("{0}", Convert.ToChar(34))
                    Next

                    Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_CHRSET And showExtra And Not createGametext Then
                    ' ***** Z-CHARACTER SET TABLE *****
                    oMemEntry.PrintMemoryLabel()
                    Console.WriteLine()
                    Console.WriteLine("***** Z-CHARACTER SET TABLE ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                    If showHex Then HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)

                    Console.WriteLine()
                    For i = 0 To 2
                        Dim a As Integer = oMemEntry.addressStart
                        Dim o As Integer = 0
                        If i = 1 Then o = 26
                        If i = 2 Then o = 52
                        Console.Write("{0:X5} ", a + o)
                        For j = 0 To 25
                            Console.Write("{0:X2} ", byteStory(a + o + j))
                        Next
                        Console.WriteLine(alphabet(i).Substring(6))
                    Next
                    Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_TERMINATING_CHARS_TABLE And showExtra And Not createGametext Then
                    ' ***** TERMINATING CHARACTERS TABLE *****
                    oMemEntry.PrintMemoryLabel()
                    If Helper.GetAdressFromWord(byteStory, &H36) > 0 Then
                        Console.WriteLine()
                        Console.WriteLine("***** TERMINATING CHARACTERS TABLE ({0:X5}-{1:X5}, {2}) *****", iAddrTCharsStart, iAddrTCharsStart + iTCharsLen, oMemEntry.SizeString)
                        If showHex Then HexDump(iAddrTCharsStart, iAddrTCharsStart + iTCharsLen, True)
                        Console.WriteLine()
                        For i As Integer = iAddrTCharsStart To iAddrTCharsStart + iTCharsLen
                            Console.Write("{0:X5} {1:X2} ZSCII-char {1,3} ", i, byteStory(i))
                            Select Case byteStory(i)
                                Case 129 : Console.WriteLine("Cursor up")
                                Case 130 : Console.WriteLine("Cursor down")
                                Case 131 : Console.WriteLine("Cursor left")
                                Case 132 : Console.WriteLine("Cursor right")
                                Case 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144
                                    Console.WriteLine("F{0}", byteStory(i) - 133)
                                Case 145, 146, 147, 148, 149, 150, 151, 152, 153, 154
                                    Console.WriteLine("Keypad {0}", byteStory(i) - 145)
                                Case 252 : Console.WriteLine("Menu-click")
                                Case 253 : Console.WriteLine("Double-click")
                                Case 254 : Console.WriteLine("Single-click")
                                Case 255 : Console.WriteLine("All function keys")
                                Case 0 : Console.WriteLine("Table terminating byte")
                                Case Else : Console.WriteLine("Illegal value")
                            End Select
                        Next
                    End If
                    Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_PROPERTY_DEFAULTS_TABLE And showExtra And Not createGametext Then
                    ' ***** PROPERTY DEFAULTS TABLE *****
                    oMemEntry.PrintMemoryLabel()
                    Console.WriteLine()
                    Console.WriteLine("***** PROPERTY DEFAULTS TABLE ({0:X5}-{1:X5}, {2}) *****", iAddrObjectDefaultsTableStart, iAddrObjectDefaultsTableEnd, oMemEntry.SizeString)
                    If showHex Then HexDump(iAddrObjectDefaultsTableStart, iAddrObjectDefaultsTableEnd, True)
                    Console.WriteLine()
                    For i As Integer = 0 To iPropDefaultCount - 1
                        Console.Write("{0:X5} {1:X2} {2:X2}", iAddrObjectDefaultsTableStart + i * 2, byteStory(iAddrObjectDefaultsTableStart + i * 2), byteStory(iAddrObjectDefaultsTableStart + i * 2 + 1))
                        Console.WriteLine("                   Default value property {0} = {1:X4}", i + 1, byteStory(iAddrObjectDefaultsTableStart + i * 2) * 256 + byteStory(iAddrObjectDefaultsTableStart + i * 2 + 1))
                    Next
                    Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_OBJECT_TREE_TABLE And (showObjects Or createGametext) Then
                    ' ***** OBJECT TABLE *****
                    If Not createGametext Then
                        oMemEntry.PrintMemoryLabel()
                        Console.WriteLine()
                        Console.WriteLine("***** OBJECT TREE TABLE ({0:X5}-{1:X5}, {2}) *****", iAddrObjectTreeStart, iAddrObjectTreeEnd, oMemEntry.SizeString)
                        If showHex Then HexDump(iAddrObjectTreeStart, iAddrObjectTreeEnd, True)
                        Console.WriteLine()
                        If objectList.Count > 0 Then
                            For i As Integer = 0 To objectList.Count - 1
                                If objectList(i).ParentId = 0 And objectList(i).ChildId = 0 Then
                                    If showInvisiClues Then
                                        Console.WriteLine("{0}{1}", Space(0), objectList(i).Description)
                                    Else
                                        Console.WriteLine("{0}[{1,3:###}] {2}", Space(0), objectList(i).Id, objectList(i).Description)
                                    End If
                                ElseIf objectList(i).ParentId = 0 Then
                                    PrintObjectTree(objectList, objectList(i).Id, 0, True)
                                End If
                            Next
                            Console.WriteLine()
                        End If

                        For Each oTmpMM As MemoryMapEntry In memoryMap
                            If oTmpMM.type = MemoryMapType.MM_OBJECT_PROPERTIES_TABLES Then
                                Console.WriteLine("***** OBJECT PROPERTIES TABLES ({0:X5}-{1:X5}, {2}) *****", iAddrObjectPropTableStart, iAddrObjectPropTableEnd, oTmpMM.SizeString)
                                If showHex Then HexDump(iAddrObjectPropTableStart, iAddrObjectPropTableEnd, True)
                                Console.WriteLine()
                            End If
                        Next
                        Console.WriteLine("Object count: {0}", iObjectCount)
                        Console.WriteLine()
                    End If
                    Dim iObject As Integer = 0
                    Dim iAddrObjectProperties As Integer = 65536
                    Dim iAddrProperties As Integer = 0
                    Do
                        If Not createGametext Then
                            Console.WriteLine("Object: {0}", iObject + 1)
                            Console.Write("{0:X5} ", iAddrObjectTreeStart + iObject * iObjectTreeEntryLen)
                            ' print attributes
                            For i As Integer = 0 To iObjectTreeEntryLen - 1
                                Console.Write("{0:X2} ", byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + i))
                            Next
                            Console.WriteLine()
                        End If
                        Dim sAttributes As String = "  Attributes: "
                        For i As Integer = 0 To 5
                            If i < 4 Or iZVersion >= 3 Then
                                Dim iByteAddr As Integer = iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + i
                                If (byteStory(iByteAddr) And 128) = 128 Then sAttributes = sAttributes & CStr(i * 8) & ", "
                                If (byteStory(iByteAddr) And 64) = 64 Then sAttributes = sAttributes & CStr(i * 8 + 1) & ", "
                                If (byteStory(iByteAddr) And 32) = 32 Then sAttributes = sAttributes & CStr(i * 8 + 2) & ", "
                                If (byteStory(iByteAddr) And 16) = 16 Then sAttributes = sAttributes & CStr(i * 8 + 3) & ", "
                                If (byteStory(iByteAddr) And 8) = 8 Then sAttributes = sAttributes & CStr(i * 8 + 4) & ", "
                                If (byteStory(iByteAddr) And 4) = 4 Then sAttributes = sAttributes & CStr(i * 8 + 5) & ", "
                                If (byteStory(iByteAddr) And 2) = 2 Then sAttributes = sAttributes & CStr(i * 8 + 6) & ", "
                                If (byteStory(iByteAddr) And 1) = 1 Then sAttributes = sAttributes & CStr(i * 8 + 7) & ", "
                            End If
                        Next
                        Dim objectData As ObjectData = objectList(iObject)
                        If iZVersion <= 3 Then
                            iAddrProperties = byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 7) * 256 + byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 8)
                        Else
                            iAddrProperties = byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 12) * 256 + byteStory(iAddrObjectTreeStart + iObject * iObjectTreeEntryLen + 13)
                        End If

                        If Not createGametext Then
                            Console.WriteLine(sAttributes.TrimEnd.TrimEnd(","c))
                            Console.WriteLine("  Parent = {0} ", objectData.ParentId)
                            Console.WriteLine("  Next   = {0} ", objectData.NextId)
                            Console.WriteLine("  Child  = {0} ", objectData.ChildId)
                        End If
                        If iAddrProperties < iAddrObjectProperties Then iAddrObjectProperties = iAddrProperties
                        If Not createGametext Then
                            Console.WriteLine("  Properties address  = {0:X4} ", iAddrProperties)
                        End If
                        Dim iPropDescLen As Integer = byteStory(iAddrProperties)
                        If Not createGametext Then
                            Console.Write("    {0:X5} ", iAddrProperties)
                            For i As Integer = 0 To iPropDescLen * 2
                                Console.Write("{0:X2} ", byteStory(iAddrProperties + i))
                            Next
                            Console.WriteLine()
                        End If
                        If iPropDescLen > 0 Then
                            objectData.Description = ExtractZString(iAddrProperties + 1, showAbbrevsInsertion)
                            If Not createGametext Then
                                Console.WriteLine("    Description = {0}{1}{2}", Convert.ToChar(34), objectData.Description, Convert.ToChar(34))
                            End If
                            objectNames.Add(ExtractZString(iAddrProperties + 1, False))
                        Else
                            If Not createGametext Then
                                Console.WriteLine("    Description = {0}{1}{2}", Convert.ToChar(34), "", Convert.ToChar(34))
                            End If
                        End If
                        Dim iTmpProp As Integer = iAddrProperties + 2 * iPropDescLen + 1
                        If Not createGametext Then
                            Do While byteStory(iTmpProp) <> 0
                                Dim iPropNum As Integer = 0
                                Dim iPropSize As Integer = 0
                                Dim iPropStart As Integer = iTmpProp + 1
                                If iZVersion <= 3 Then
                                    iPropNum = (byteStory(iTmpProp) And &H1F)
                                    iPropSize = 1 + (byteStory(iTmpProp) And 224) \ 32
                                Else
                                    iPropNum = (byteStory(iTmpProp) And &H3F)
                                    If (byteStory(iTmpProp) And &H80) = &H80 Then
                                        iPropSize = (byteStory(iTmpProp + 1) And &H3F)
                                        If iPropSize = 0 Then iPropSize = &H40
                                        iPropStart += 1
                                    Else
                                        If (byteStory(iTmpProp) And 64) = 64 Then iPropSize = 2 Else iPropSize = 1
                                    End If
                                End If
                                Console.Write("    {0:X5} ", iTmpProp)
                                Dim sPropData As String = ""
                                For i As Integer = 0 To iPropSize + iPropStart - iTmpProp - 1
                                    If i = 1 AndAlso (iPropStart - iTmpProp) = 1 Then Console.Write("   ")
                                    If i >= (iPropStart - iTmpProp) Then
                                        sPropData = sPropData & byteStory(iTmpProp + i).ToString("X2") & " "
                                    Else
                                        Console.Write("{0:X2} ", byteStory(iTmpProp + i))
                                    End If
                                Next
                                For i As Integer = 0 To sPropData.Length Step 24
                                    Dim iLen As Integer = 24
                                    If (i + iLen) > sPropData.Length Then iLen = sPropData.Length - i
                                    Console.Write(sPropData.Substring(i, iLen))
                                    If iLen = 24 And Not (i + 24) = sPropData.Length AndAlso sPropData.Length <> 24 Then
                                        Console.WriteLine()
                                        Console.Write("                ")
                                    End If
                                Next
                                Dim iTmpSpaces As Integer = (24 - (sPropData.Length Mod 24)) Mod 24
                                Console.Write("                                        ".Substring(0, iTmpSpaces))
                                If iPropNum < 10 Then Console.Write(" ")
                                Console.Write("{0}/{1}", iPropNum, iPropSize)
                                If iPropSize < 10 Then Console.Write(" ")
                                Console.Write(" ")
                                DecodePropertyData(False, byteStory, DictEntriesList, validStringsList, validRoutinesList, iPropNum, iPropSize, iPropStart, sPropData, iObject, "")

                                iTmpProp = iPropStart + iPropSize
                            Loop
                            Console.WriteLine()
                        End If
                        iObject += 1
                    Loop Until (iAddrObjectTreeStart + iObject * iObjectTreeEntryLen) >= iAddrObjectProperties
                End If

                If oMemEntry.type = MemoryMapType.MM_GLOBAL_VARIABLES And showVariables And Not createGametext Then
                    ' ***** GLOBAL VARIABLES *****
                    oMemEntry.PrintMemoryLabel()
                    Console.WriteLine()
                    Console.WriteLine("***** GLOBAL VARIABLES TABLE ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                    If showHex Then HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                    Console.WriteLine()
                    Dim globalCount As Integer = 0
                    For i = oMemEntry.addressStart To oMemEntry.addressEnd Step 2
                        Dim spaceCount As Integer = 1
                        If globalCount < 9 Then spaceCount += 1
                        If globalCount < 99 Then spaceCount += 1
                        Console.Write("{0:X5} {1:X2} {2:X2}                    G{3:X2}, Global #{4} ={5}0x{6:X4}", i, byteStory(i), byteStory(i + 1), globalCount, globalCount + 1, Space(spaceCount), Helper.GetAdressFromWord(byteStory, i))
                        If Not oDecode.usedGlobals.Contains(globalCount) Then
                            Console.Write(" (This global is not refered to from z-code.)")
                        End If
                        Console.WriteLine()
                        globalCount += 1
                    Next
                    Console.WriteLine()
                End If

                Dim iMaxActionNum As Integer = 0
                If oMemEntry.type = MemoryMapType.MM_GRAMMAR_TABLE And showGrammar And Not createGametext Then
                    ' ***** SYNTAX/GRAMMAR TABLES *****
                    Dim grammarTable As MemoryMapEntry = memoryMap.Find(Function(c) c.name = "Syntax/Grammar table")
                    If grammarTable IsNot Nothing Then
                        Dim iAddrGrammarStart As Integer = grammarTable.addressStart
                        Dim iAddrGrammarEnd As Integer = grammarTable.addressEnd
                        oMemEntry.PrintMemoryLabel()
                        Console.WriteLine()
                        Console.WriteLine("***** SYNTAX/GRAMMAR TABLE ({0:X5}-{1:X5}, {2}) *****", iAddrGrammarStart, iAddrGrammarEnd, oMemEntry.SizeString)
                        If showHex Then HexDump(iAddrGrammarStart, iAddrGrammarEnd, True)
                        Console.WriteLine()

                        If grammarVer = 2 And (compilerSource = EnumCompilerSource.ZILCH Or compilerSource = EnumCompilerSource.ZILF) Then
                            ' Zilf & Zilch v2
                            ' This table contains verbdefinitions for verbs with no objects and points to the definition of one and two objects verbs
                            DictEntriesList.v2_VerbAddresses.Sort()
                            For i As Integer = 0 To DictEntriesList.v2_VerbAddresses.Count - 1
                                Dim address As Integer = DictEntriesList.v2_VerbAddresses(i)
                                Console.Write("Verb {0}", i)
                                Dim sTmp As String = " <SYNONYM"
                                For Each dictWord As DictionaryEntry In DictEntriesList
                                    If (dictWord.v2_ClassificationNumber And DictEntriesList.v2_VerbWord) = DictEntriesList.v2_VerbWord And dictWord.v2_SemanticStuff = address Then sTmp = String.Concat(sTmp, " ", dictWord.dictWord.ToUpper)
                                Next
                                Console.WriteLine("{0}>", sTmp)
                                Console.Write("{0:X5}", address)
                                For j As Integer = 0 To 7
                                    Console.Write(" {0:X2}", byteStory(address + j))
                                Next
                                Dim nullObjAction As Integer = Helper.GetAdressFromWord(byteStory, address)
                                Dim nullObjPrep As Integer = Helper.GetAdressFromWord(byteStory, address + 2)
                                Dim addressOneObj As Integer = Helper.GetAdressFromWord(byteStory, address + 4)
                                Dim addressTwoObj As Integer = Helper.GetAdressFromWord(byteStory, address + 6)
                                Console.WriteLine("{0}Main data: [{1:X4} {2:X4} {3:X4} {4:X4}]", Space(16), nullObjAction, nullObjPrep, addressOneObj, addressTwoObj)
                                If Not nullObjAction = &HFFFF Then
                                    Console.WriteLine("  No object entry:")
                                    Dim syntax As String = DecodeGrammarsZilV2(address, DictEntriesList, iAddrActionTable, iAddrPreActionTable, 0)
                                    Console.Write("   {0:X5} {1:X2} {2:X2} {3:X2} {4:X2}{5}{6}", address, byteStory(address), byteStory(address + 1), byteStory(address + 2), byteStory(address + 3), Space(25), syntax)
                                    Console.WriteLine()
                                End If
                                If addressOneObj > 0 Then
                                    Dim numberOfEntries As Integer = Helper.GetAdressFromWord(byteStory, addressOneObj)
                                    Console.WriteLine("  One object entries:")
                                    Console.Write("   {0:X5} {1:X2} {2:X2}", addressOneObj, byteStory(addressOneObj), byteStory(addressOneObj + 1))
                                    For j As Integer = 0 To numberOfEntries - 1
                                        For k As Integer = 0 To 5
                                            Console.Write(" {0:X2}", byteStory(addressOneObj + 2 + j * 6 + k))
                                        Next
                                        Console.WriteLine("{0}{1}", Space(13), DecodeGrammarsZilV2(addressOneObj + 2 + j * 6, DictEntriesList, iAddrActionTable, iAddrPreActionTable, 1))
                                        If j < numberOfEntries - 1 Then Console.Write(Space(14))
                                    Next
                                End If
                                If addressTwoObj > 0 Then
                                    Dim numberOfEntries As Integer = Helper.GetAdressFromWord(byteStory, addressTwoObj)
                                    Console.WriteLine("  Two object entries:")
                                    Console.Write("   {0:X5} {1:X2} {2:X2}", addressTwoObj, byteStory(addressTwoObj), byteStory(addressTwoObj + 1))
                                    For j As Integer = 0 To numberOfEntries - 1
                                        For k As Integer = 0 To 9
                                            Console.Write(" {0:X2}", byteStory(addressTwoObj + 2 + j * 10 + k))
                                        Next
                                        Console.WriteLine("{0}{1}", Space(1), DecodeGrammarsZilV2(addressTwoObj + 2 + j * 10, DictEntriesList, iAddrActionTable, iAddrPreActionTable, 2))
                                        If j < numberOfEntries - 1 Then Console.Write(Space(14))
                                    Next
                                End If
                                Console.WriteLine()
                            Next
                            Console.WriteLine()
                        Else
                            ' Inform5, v1, Inform6, v1 & v2, Zilf & Zilch v1
                            Dim iVerbNum As Integer = 255
                            For i = iAddrGrammarStart To iAddrGrammarEnd Step 2
                                Console.Write("{0:X5} {1:X2} {2:X2} ", i, byteStory(i), byteStory(i + 1))
                                Console.Write("                   Address of Verb {0,3}: 0x{1:X4} [", iVerbNum, Helper.GetAdressFromWord(byteStory, i))
                                If DictEntriesList.GetVerb(iVerbNum).Count = 0 Then
                                    Console.Write("no-verb)")
                                Else
                                    Dim sTmp As String = ""
                                    For Each dictEntry As DictionaryEntry In DictEntriesList.GetVerb(iVerbNum)
                                        If {EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF}.Contains(compilerSource) Then
                                            sTmp = sTmp & " " & dictEntry.dictWord.ToUpper
                                        Else
                                            sTmp = sTmp & "'" & dictEntry.dictWord & "'/"
                                        End If
                                    Next
                                    sTmp = sTmp.TrimEnd("/"c).TrimStart()
                                    Console.Write(sTmp & "]")
                                End If
                                iVerbNum -= 1
                                Console.WriteLine()
                            Next
                            Console.WriteLine()
                        End If
                    End If
                End If

                If oMemEntry.type = MemoryMapType.MM_GRAMMAR_TABLE_DATA And showGrammar And Not createGametext Then
                    oMemEntry.PrintMemoryLabel()
                    Console.WriteLine()
                    Console.WriteLine("***** SYNTAX/GRAMMAR TABLE DATA ({0:X5}-{1:X5}, {2}) *****", iAddrGrammarDataStart, iAddrGrammarDataEnd, oMemEntry.SizeString)
                    If showHex Then HexDump(iAddrGrammarDataStart, iAddrGrammarDataEnd, True)
                    Console.WriteLine()
                    Dim iVerbNum As Integer = 255
                    Dim grammarTable As MemoryMapEntry = memoryMap.Find(Function(c) c.name = "Syntax/Grammar table")
                    Dim iAddrGrammarStart As Integer = grammarTable.addressStart
                    Dim iAddrGrammarEnd As Integer = grammarTable.addressEnd

                    If grammarVer = 2 And (compilerSource = EnumCompilerSource.ZILCH Or compilerSource = EnumCompilerSource.ZILF) Then
                        Console.WriteLine("[The one and two object syntax are presented together with the no objexct syntax in the syntax table.]")
                        Console.WriteLine()
                    Else
                        Do
                            Dim iAddrSyntax As Integer = iAddrGrammarStart + (255 - iVerbNum) * 2
                            iAddrSyntax = byteStory(iAddrSyntax) * 256 + byteStory(iAddrSyntax + 1)
                            Dim iSyntaxLines As Integer = byteStory(iAddrSyntax)
                            Console.Write("Verb {0} ", iVerbNum)
                            Dim sTmp As String = ""
                            If {EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF}.Contains(compilerSource) Then sTmp = "<SYNONYM"
                            For Each dictEntry As DictionaryEntry In DictEntriesList.GetVerb(iVerbNum)
                                If {EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF}.Contains(compilerSource) Then
                                    sTmp = sTmp & " " & dictEntry.dictWord.ToUpper
                                Else
                                    sTmp = sTmp & "'" & dictEntry.dictWord & "'/"
                                End If
                            Next
                            If DictEntriesList.GetVerb(iVerbNum).Count = 0 Then
                                sTmp &= "no-verb"
                            End If
                            If {EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF}.Contains(compilerSource) Then
                                sTmp &= ">"
                            Else
                                sTmp = sTmp.TrimEnd("/"c)
                            End If
                            Console.WriteLine(sTmp)

                            Console.Write("{0:X5} {1:X2} ", iAddrSyntax, iSyntaxLines)
                            If grammarVer = 1 Then
                                If compactSyntaxes Then
                                    ' COMPACT-SYNTAXES for ZIL means that syntax lines are of variable length (2, 4 or 7 bytes)
                                    Dim iStartAddress As Integer = iAddrSyntax + 1
                                    For i As Integer = 1 To iSyntaxLines
                                        If i > 1 Then Console.Write("         ")
                                        Dim syntaxLineLength As Integer = 2
                                        Select Case (byteStory(iStartAddress) And &HC0)
                                            Case &H40 : syntaxLineLength = 4
                                            Case &H80 : syntaxLineLength = 7
                                        End Select
                                        If byteStory(iStartAddress + 1) > iMaxActionNum Then iMaxActionNum = byteStory(iStartAddress + 1)   ' ActionNumber always in the second byte
                                        For j = 0 To syntaxLineLength - 1
                                            Console.Write("{0:X2} ", byteStory(iStartAddress + j))
                                        Next
                                        If syntaxLineLength = 2 Then Console.Write(Space(18))
                                        If syntaxLineLength = 4 Then Console.Write(Space(12))
                                        If syntaxLineLength = 7 Then Console.Write(Space(3))
                                        Dim grammarString As String = DecodeGrammarsZilV1(iStartAddress, DictEntriesList, iAddrActionTable, iAddrPreActionTable, iAddrAdjectiveTable, compactSyntaxes)
                                        Console.Write(grammarString)
                                        Console.WriteLine()
                                        iStartAddress += syntaxLineLength
                                    Next
                                Else
                                    For i As Integer = 1 To iSyntaxLines
                                        If i > 1 Then Console.Write("         ")
                                        Dim iStartAddress As Integer = iAddrSyntax + 1 + (i - 1) * 8
                                        For j = 0 To 7
                                            Dim iNum As Integer = byteStory(iAddrSyntax + 1 + (i - 1) * 8 + j)
                                            Console.Write("{0:X2} ", iNum)
                                            If j = 7 And iNum > iMaxActionNum Then iMaxActionNum = iNum
                                        Next
                                        Dim grammarString As String
                                        If compilerSource = EnumCompilerSource.ZILF Or compilerSource = EnumCompilerSource.ZILCH Then
                                            grammarString = DecodeGrammarsZilV1(iStartAddress, DictEntriesList, iAddrActionTable, iAddrPreActionTable, iAddrAdjectiveTable, compactSyntaxes)
                                        Else
                                            grammarString = DecodeGrammarsInformV1(iStartAddress, DictEntriesList, iAddrActionTable, iAddrPreActionTable, iAddrAdjectiveTable)
                                        End If
                                        Console.Write(grammarString)
                                        Console.WriteLine()
                                    Next
                                End If
                                Console.WriteLine()
                            End If
                            If grammarVer = 2 And (compilerSource = EnumCompilerSource.INFORM5 Or compilerSource = EnumCompilerSource.INFORM6) Then
                                If iSyntaxLines = 0 Then
                                    Console.WriteLine()
                                Else
                                    Dim iCurrentLine As Integer = 0
                                    Dim iCurrentAddress As Integer = iAddrSyntax + 1
                                    Dim iStartAddress As Integer = 0
                                    Do
                                        iCurrentLine += 1
                                        iStartAddress = iCurrentAddress
                                        If (byteStory(iCurrentAddress) * 256 + byteStory(iCurrentAddress + 1) And 1023) > iMaxActionNum Then iMaxActionNum = (byteStory(iCurrentAddress) * 256 + byteStory(iCurrentAddress + 1) And 1023)
                                        Console.Write("[ {0:X2} ", byteStory(iCurrentAddress))
                                        Console.Write("{0:X2} ", byteStory(iCurrentAddress + 1))
                                        iCurrentAddress += 2
                                        Dim newlineCounter As Integer = 2
                                        Do
                                            If newlineCounter = 9 Then
                                                Console.WriteLine()
                                                Console.Write("           ")
                                                newlineCounter = 0
                                            End If
                                            Console.Write("{0:X2} ", byteStory(iCurrentAddress))
                                            iCurrentAddress += 1
                                            newlineCounter += 1
                                        Loop Until byteStory(iCurrentAddress - 1) = 15 And (iCurrentAddress - iStartAddress) Mod 3 = 0
                                        Dim grammarString As String = DecodeGrammarsInformV2(iStartAddress, DictEntriesList, iAddrActionTable)
                                        Console.WriteLine(Space(3 * (9 - newlineCounter)) & "] {0}", grammarString)
                                        Console.Write("         ")
                                    Loop Until iCurrentLine >= iSyntaxLines
                                End If
                                Console.WriteLine()
                            End If
                            If grammarVer = 3 And compilerSource = EnumCompilerSource.INFORM6 Then
                                If iSyntaxLines = 0 Then
                                    Console.WriteLine()
                                Else
                                    Dim iCurrentLine As Integer = 0
                                    Dim iCurrentAddress As Integer = iAddrSyntax + 1
                                    Dim iStartAddress As Integer = 0
                                    Do
                                        iCurrentLine += 1
                                        iStartAddress = iCurrentAddress
                                        If (byteStory(iCurrentAddress) * 256 + byteStory(iCurrentAddress + 1) And 1023) > iMaxActionNum Then iMaxActionNum = (byteStory(iCurrentAddress) * 256 + byteStory(iCurrentAddress + 1) And 1023)
                                        Console.Write("[ {0:X2} ", byteStory(iCurrentAddress))
                                        Console.Write("{0:X2} ", byteStory(iCurrentAddress + 1))
                                        Dim tokens As Integer = (Helper.GetAdressFromWord(byteStory, iCurrentAddress) And &HF800) >> 11
                                        iCurrentAddress += 2
                                        Dim newlineCounter As Integer = 2
                                        For i As Integer = 0 To tokens * 2 - 1
                                            If newlineCounter = 9 Then
                                                Console.WriteLine()
                                                Console.Write("           ")
                                                newlineCounter = 0
                                            End If
                                            Console.Write("{0:X2} ", byteStory(iCurrentAddress))
                                            iCurrentAddress += 1
                                            newlineCounter += 1
                                        Next
                                        Dim grammarString As String = DecodeGrammarsInformV3(iStartAddress, DictEntriesList, iAddrActionTable, iAddrPreActionTable, iAddrAdjectiveTable)
                                        Console.WriteLine(Space(3 * (9 - newlineCounter)) & "] {0}", grammarString)
                                        Console.Write("         ")
                                    Loop Until iCurrentLine >= iSyntaxLines
                                End If
                                Console.WriteLine()
                            End If
                            iVerbNum -= 1
                        Loop Until iVerbNum < DictEntriesList.GetLowestVerbNum
                        iMaxActionNum += 1
                    End If
                End If

                Dim iAddrPreAction As Integer = iAddrActionTable + iMaxActionNum * 2
                If oMemEntry.type = MemoryMapType.MM_ACTION_TABLE And showGrammar And Not createGametext Then
                    ' ***** ACTION TABLE *****
                    If iVerbActionCount > 0 Then
                        oMemEntry.PrintMemoryLabel()
                        Console.WriteLine()
                        Console.WriteLine("***** ACTION TABLE ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                        If showHex Then HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                        Console.WriteLine()
                        If verbGrammarList IsNot Nothing AndAlso verbGrammarList.Count > 0 Then
                            For i As Integer = 0 To verbGrammarList.Keys.Max
                                Console.WriteLine("{0:X5} {1:X2} {2:X2}                    Action #{3}, packed address for routine at 0x{4:X5}", oMemEntry.addressStart + i * 2, byteStory(oMemEntry.addressStart + i * 2), byteStory(oMemEntry.addressStart + i * 2 + 1), i, Helper.GetAdressFromPacked(byteStory, oMemEntry.addressStart + i * 2, True))

                                Dim grammarStrings As List(Of String) = Nothing
                                If verbGrammarList.TryGetValue(i, grammarStrings) Then
                                    For Each grammarString As String In grammarStrings
                                        Console.WriteLine("                                    {0}", grammarString)
                                    Next
                                End If
                                Console.WriteLine()
                            Next
                        End If
                    End If
                End If

                If oMemEntry.type = MemoryMapType.MM_PREACTION_TABLE And showGrammar And Not createGametext Then
                    ' ***** PRE-ACTION TABLE *****
                    If iVerbActionCount > 0 Then
                        oMemEntry.PrintMemoryLabel()
                        Console.WriteLine()
                        Console.WriteLine("***** PRE-ACTION TABLE ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                        ' ZIL, ZILCH, ver 1: Same size as action table. Contains packed addresses for pre-action
                        If showHex Then HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                        Console.WriteLine()
                        If verbGrammarList IsNot Nothing AndAlso verbGrammarList.Count > 0 Then
                            For i As Integer = 0 To verbGrammarList.Keys.Max
                                If Helper.GetAdressFromWord(byteStory, oMemEntry.addressStart + i * 2) = 0 Then
                                    Console.WriteLine("{0:X5} {1:X2} {2:X2}                    Unused", oMemEntry.addressStart + i * 2, byteStory(oMemEntry.addressStart + i * 2), byteStory(oMemEntry.addressStart + i * 2 + 1))
                                Else
                                    Console.WriteLine("{0:X5} {1:X2} {2:X2}                    Pre-action #{3}, packed address for routine at 0x{4:X5}", oMemEntry.addressStart + i * 2, byteStory(oMemEntry.addressStart + i * 2), byteStory(oMemEntry.addressStart + i * 2 + 1), i, Helper.GetAdressFromPacked(byteStory, oMemEntry.addressStart + i * 2, True))
                                    Dim grammarStrings As List(Of String) = Nothing
                                    If verbGrammarList.TryGetValue(i, grammarStrings) Then
                                        For Each grammarString As String In grammarStrings
                                            Console.WriteLine("                                    {0}", grammarString)
                                        Next
                                    End If
                                End If
                                Console.WriteLine()
                            Next
                        End If
                    End If
                End If

                If oMemEntry.type = MemoryMapType.MM_PREACTION_PARSING_TABLE And showGrammar And Not createGametext Then
                    ' ***** PARSING ROUTINE TABLE *****
                    If iVerbActionCount > 0 Then
                        oMemEntry.PrintMemoryLabel()
                        Console.WriteLine()
                        Console.WriteLine("***** PARSING ROUTINE TABLE ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                        ' Inform5, ver 1   : Same size as action table. Contains packed parser-routines
                        ' Inform6, ver 1   : Variable size. Contains packed parser-routines
                        ' Inform6, ver 2   : Don't exists
                        If showHex Then HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                        Console.WriteLine()
                        Dim parsingNumber As Integer = 0
                        For i As Integer = oMemEntry.addressStart To oMemEntry.addressEnd Step 2
                            If Helper.GetAdressFromWord(byteStory, i) = 0 Then
                                Console.WriteLine("{0:X5} {1:X2} {2:X2}                    Unused", i, byteStory(i), byteStory(i + 1))
                            Else
                                Console.WriteLine("{0:X5} {1:X2} {2:X2}                    Parsing routine #{3}, packed address for routine at 0x{4:X5}", i, byteStory(i), byteStory(i + 1), parsingNumber, Helper.GetAdressFromPacked(byteStory, i, True))
                            End If
                            parsingNumber += 1
                        Next
                        Console.WriteLine()
                    End If
                End If

                If oMemEntry.type = MemoryMapType.MM_PREPOSITION_TABLE And showGrammar And Not createGametext Then
                    ' ***** PREPOSITION/ADJECTIVE TABLE *****
                    If iVerbActionCount > 0 Then
                        oMemEntry.PrintMemoryLabel()
                        Console.WriteLine()
                        Console.WriteLine("***** PREPOSITION/ADJECTIVE TABLE ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                        ' Inform5, ver 1   : Variable size. First word = number of entries then follows entries of 4 bytes where
                        '                    first word is dictionary-address and second word is adjective/preposition number. 
                        '                    Entries is ordered lowest number to highest.
                        '                    Curses_r7 have a broken table here.
                        ' Inform6, ver 1   : Same as Inform5, ver 1.
                        ' Inform6, ver 2   : Isn't used. Contains two 00 bytes as placeholders.
                        ' Inform6, ver 3   : Variable size. First word = number of entries then follows entries of 2 bytes where
                        '                    the position is the adjective/preposition number and the word is dictionary-address. 
                        '                    Entries is ordered lowest number to highest.
                        ' Zilf/Zilch, ver 1: Variable size. First word = number of entries then follows entries of 4 bytes where
                        '                    first word is dictionary-address and second word is preposition number. 
                        '                    Entries is unordered. Entries with same preposition number in vocabulary are synonyms.
                        ' Zilf/Zilch, ver 1: With COMPACT-VOCABULARY. Variable size. First word = number of entries then follows entries of
                        '                    3 bytes where first word is dictionary-address and third byte is preposition number. 
                        '                    Entries is unordered and because vocabulary is compacted it contains no preposition numbers,
                        '                    therefore all prepositions are in this list and there can be multiple entries with the same 
                        '                    preposition number.
                        ' Zilf/Zilch, ver 2: N/A
                        If showHex Then HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                        Console.WriteLine()
                        Dim adjectiveCount As Integer = Helper.GetAdressFromWord(byteStory, oMemEntry.addressStart)
                        Console.WriteLine("{0:X5} {1:X2} {2:X2}                    Number of entries: {3}", oMemEntry.addressStart, byteStory(oMemEntry.addressStart), byteStory(oMemEntry.addressStart + 1), adjectiveCount)
                        Dim printedPrepsForCompactVocab As New HashSet(Of Integer)
                        For i As Integer = 0 To adjectiveCount - 1
                            If DictEntriesList.v1_CompactVocabulary Then
                                Dim currentAddress As Integer = oMemEntry.addressStart + 2 + i * 3
                                Dim prepAddress As Integer = Helper.GetAdressFromWord(byteStory, currentAddress)
                                Dim prepID As Integer = byteStory(oMemEntry.addressStart + 4 + i * 3)
                                Dim preposition As String = DictEntriesList.GetEntryAtAddress(prepAddress).dictWord
                                Console.Write("{0:X5} {1:X2} {2:X2} {3:X2}                 Preposition #{4}: {5}", currentAddress, byteStory(currentAddress), byteStory(currentAddress + 1), byteStory(currentAddress + 2), byteStory(currentAddress + 2), preposition.ToUpper)
                                If Not printedPrepsForCompactVocab.Contains(prepID) Then
                                    Dim first As Boolean = True
                                    For j As Integer = i + 1 To adjectiveCount - 1
                                        If prepID = byteStory(oMemEntry.addressStart + 4 + j * 3) Then
                                            If first Then
                                                Console.Write(" {0}<SYNONYM {1}", Space(10 - preposition.Length), preposition.ToUpper)
                                                first = False
                                            End If
                                            Console.Write(" {0}", DictEntriesList.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, oMemEntry.addressStart + 2 + j * 3)).dictWord.ToUpper)
                                        End If
                                    Next
                                    If Not first Then Console.Write(">")
                                    printedPrepsForCompactVocab.Add(prepID)
                                End If
                            Else
                                Dim currentAddress As Integer = oMemEntry.addressStart + 2 + i * 4
                                Dim prepID As Integer = byteStory(oMemEntry.addressStart + 5 + i * 4)
                                Dim prepAddress As Integer = Helper.GetAdressFromWord(byteStory, currentAddress)
                                Dim preposition As String
                                Dim errMsg As String = ""
                                If grammarVer = 3 Then
                                    currentAddress = oMemEntry.addressStart + 2 + i * 2
                                    prepID = i
                                    prepAddress = Helper.GetAdressFromWord(byteStory, currentAddress)
                                    preposition = DictEntriesList.GetEntryAtAddress(prepAddress).dictWord
                                    Console.Write("{0:X5} {1:X2} {2:X2}                    Preposition #{3}: '{4}'{5}", currentAddress, byteStory(currentAddress), byteStory(currentAddress + 1), prepID, preposition, errMsg)
                                Else
                                    Try
                                        preposition = DictEntriesList.GetEntryAtAddress(prepAddress).dictWord
                                    Catch
                                        preposition = DictEntriesList.GetPreposition(prepID)(0).dictWord
                                        errMsg = " (Incomplete table entry, word taken from dictionary instead.)"
                                    End Try
                                    If compilerSource = EnumCompilerSource.ZILCH Or compilerSource = EnumCompilerSource.ZILF Then
                                        Console.Write("{0:X5} {1:X2} {2:X2} {3:X2} {4:X2}              Preposition #{5}: {6}", currentAddress, byteStory(currentAddress), byteStory(currentAddress + 1), byteStory(currentAddress + 2), byteStory(currentAddress + 3), byteStory(currentAddress + 3), preposition.ToUpper)
                                    Else
                                        Console.Write("{0:X5} {1:X2} {2:X2} {3:X2} {4:X2}              Preposition #{5}: '{6}'{7}", currentAddress, byteStory(currentAddress), byteStory(currentAddress + 1), byteStory(currentAddress + 2), byteStory(currentAddress + 3), byteStory(currentAddress + 3), preposition, errMsg)
                                    End If
                                    If DictEntriesList.GetPreposition(prepID).Count > 1 Then
                                        If compilerSource = EnumCompilerSource.ZILCH Or compilerSource = EnumCompilerSource.ZILF Then
                                            Console.Write(" {0}<SYNONYM {1}", Space(10 - preposition.Length), preposition.ToUpper)
                                        Else
                                            Console.Write(",{0}synonyms = ", Space(10 - preposition.Length))
                                        End If
                                        For j As Integer = 0 To DictEntriesList.GetPreposition(prepID).Count - 1
                                            If DictEntriesList.GetPreposition(prepID)(j).dictAddress <> prepAddress Then
                                                If compilerSource = EnumCompilerSource.ZILCH Or compilerSource = EnumCompilerSource.ZILF Then
                                                    Console.Write(" {0}", DictEntriesList.GetPreposition(prepID)(j).dictWord.ToUpper)
                                                Else
                                                    Console.Write(" '{0}'", DictEntriesList.GetPreposition(prepID)(j).dictWord)
                                                    If j < DictEntriesList.GetPreposition(prepID).Count - 1 Then Console.Write(",")
                                                End If
                                            End If
                                        Next
                                        If compilerSource = EnumCompilerSource.ZILCH Or compilerSource = EnumCompilerSource.ZILF Then Console.Write(">")
                                    End If
                                End If
                            End If
                            Console.WriteLine()
                        Next
                        Console.WriteLine()
                    End If
                End If

                If oMemEntry.type = MemoryMapType.MM_DICTIONARY And showDictionary And Not createGametext Then
                    ' ***** VOCABULARY/DICTIONARY *****
                    oMemEntry.PrintMemoryLabel()
                    Console.WriteLine()
                    Console.WriteLine("***** VOCABULARY/DICTIONARY ({0:X5}-{1:X5}, {2}) *****", iAddrDictionary, iAddrDictionaryEnd, oMemEntry.SizeString)
                    If showHex Then HexDump(iAddrDictionary, iAddrDictionaryEnd, True)
                    Console.WriteLine()
                    Console.WriteLine("{0:X5} {1:X2}                       # of separators: {2}", iAddrDictionary, byteStory(iAddrDictionary), iWordSeparatorsCount)
                    Console.Write("{0:X5}", iAddrDictionary + 1)
                    For i As Integer = 1 To iWordSeparatorsCount
                        Console.Write(" {0:X2}", byteStory(iAddrDictionary + i))
                    Next
                    Console.Write("{0}Word separators: ", Space((8 - iWordSeparatorsCount) * 3 + 2))
                    For i As Integer = 1 To iWordSeparatorsCount
                        Console.Write(System.Text.Encoding.ASCII.GetString(byteStory, i + iAddrDictionary, 1))
                    Next
                    Console.WriteLine()
                    Console.WriteLine("{0:X5} {1:X2}                       Entry length   : {2}", iAddrDictionary + iWordSeparatorsCount + 1, byteStory(iAddrDictionary + iWordSeparatorsCount + 1), iWordSize)
                    Console.WriteLine("{0:X5} {1:X2} {2:X2}                    Word count     : {3}", iAddrDictionary + iWordSeparatorsCount + 2, byteStory(iAddrDictionary + iWordSeparatorsCount + 2), byteStory(iAddrDictionary + iWordSeparatorsCount + 3), iWordCount)
                    Console.WriteLine()
                    For i As Integer = 0 To iWordCount - 1
                        Console.Write("{0:X5} ", DictEntriesList(i).dictAddress)
                        For j As Integer = 1 To iWordSize
                            Console.Write("{0:X2} ", byteStory(DictEntriesList(i).dictAddress + j - 1))
                        Next
                        Console.Write(DictEntriesList(i).dictWord)
                        If 10 - DictEntriesList(i).dictWord.Length > 0 Then
                            Console.Write("          ".Substring(0, 10 - DictEntriesList(i).dictWord.Length))
                        End If

                        Dim iPS As Integer = DictEntriesList(i).Flags
                        Dim iV1 As Integer = DictEntriesList(i).V1
                        Dim iV2 As Integer = DictEntriesList(i).V2
                        Dim sPS As String = ""
                        Select Case compilerSource
                            Case EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF
                                If grammarVer = 1 Then
                                    If (iPS And 128) = 128 Then sPS &= "+PS?OBJECT"
                                    If (iPS And 64) = 64 Then sPS &= "+PS?VERB"
                                    If (iPS And 32) = 32 Then sPS &= "+PS?ADJECTIVE"
                                    If (iPS And 16) = 16 Then sPS &= "+PS?DIRECTION"
                                    If (iPS And 8) = 8 Then sPS &= "+PS?PREPOSITION"
                                    If (iPS And 4) = 4 Then sPS &= "+PS?BUZZ-WORD"
                                    If (iPS And 3) = 3 Then
                                        sPS &= "+P1?DIRECTION"
                                    ElseIf (iPS And 2) = 2 Then
                                        sPS &= "+P1?ADJECTIVE"
                                    ElseIf (iPS And 1) = 1 Then
                                        sPS &= "+P1?VERB"
                                    Else
                                        sPS &= "+P1?OBJECT"
                                    End If
                                    sPS = sPS.TrimStart("+"c)
                                    If sPS.Contains("PS?VERB") Then sPS = sPS & ", Verb#=" & DictEntriesList(i).VerbNum.ToString
                                    If sPS.Contains("PS?ADJECTIVE") And iZVersion <= 3 Then sPS = sPS & ", Adjective#=" & DictEntriesList(i).AdjNum.ToString
                                    If sPS.Contains("PS?DIRECTION") Then sPS = sPS & ", Direction#=" & DictEntriesList(i).DirNum.ToString
                                    If Not DictEntriesList.v1_CompactVocabulary And sPS.Contains("PS?PREPOSITION") Then sPS = sPS & ", Preposition#=" & DictEntriesList(i).PrepNum.ToString
                                    Console.Write(" {0}", sPS)
                                Else
                                    Console.Write(" Classification={0} ", DictEntriesList(i).v2_ClassificationNumber)
                                    If DictEntriesList(i).v2_ClassificationNumber > 0 Then
                                        Dim bits As New BitArray({DictEntriesList(i).v2_ClassificationNumber})
                                        Dim first As Boolean = True
                                        For j As Integer = 0 To bits.Length - 2
                                            If bits(j) Then
                                                If first Then
                                                    first = False
                                                    Console.Write("[")
                                                Else
                                                    Console.Write(" ")
                                                End If
                                                If DictEntriesList.v2_PartsOfSpeech.ContainsKey(CInt(2 ^ j)) Then
                                                    Console.Write(DictEntriesList.v2_PartsOfSpeech(CInt(2 ^ j)))
                                                    If DictEntriesList.v2_PartsOfSpeech(CInt(2 ^ j)) = "VERB" Then Console.Write("=0x{0:X4}", DictEntriesList(i).v2_SemanticStuff)
                                                    If DictEntriesList.v2_PartsOfSpeech(CInt(2 ^ j)) = "DIR" Then Console.Write("=0x{0:X2}", CInt(DictEntriesList(i).v2_SemanticStuff / 256))
                                                Else
                                                    Console.Write("UNKNOWN-PART-OF-SPEECH")
                                                End If
                                            End If
                                        Next
                                        If Not first Then Console.Write("]")
                                    Else
                                        If DictEntriesList(i).v2_SemanticStuff = 0 Then Console.Write("[BUZZ]")
                                        If DictEntriesList(i).v2_SemanticStuff > 0 Then Console.Write("Synonym to '{0}'", DictEntriesList.GetEntryAtAddress(DictEntriesList(i).v2_SemanticStuff).dictWord)
                                    End If
                                End If
                                If DictEntriesList(i).v2_WordFlags > 0 Then
                                    Console.Write(" Flags={0} ", DictEntriesList(i).v2_WordFlags)
                                    Dim flags As String = "["
                                    If (DictEntriesList(i).v2_WordFlags And &H8) = &H8 Then flags = String.Concat(flags, "FIRST-PERSON ")
                                    If (DictEntriesList(i).v2_WordFlags And &H10) = &H10 Then flags = String.Concat(flags, "PLURAL-FLAG ")
                                    If (DictEntriesList(i).v2_WordFlags And &H20) = &H20 Then flags = String.Concat(flags, "SECOND-PERSON ")
                                    If (DictEntriesList(i).v2_WordFlags And &H40) = &H40 Then flags = String.Concat(flags, "THIRD-PERSON ")
                                    If (DictEntriesList(i).v2_WordFlags And &H100) = &H100 Then flags = String.Concat(flags, "PRESENT-TENSE ")
                                    If (DictEntriesList(i).v2_WordFlags And &H200) = &H200 Then flags = String.Concat(flags, "PAST-TENSE ")
                                    If (DictEntriesList(i).v2_WordFlags And &H400) = &H400 Then flags = String.Concat(flags, "FUTURE-TENSE ")
                                    If (DictEntriesList(i).v2_WordFlags And &H2000) = &H2000 Then flags = String.Concat(flags, "THING-PNF ")
                                    If (DictEntriesList(i).v2_WordFlags And &H4000) = &H4000 Then flags = String.Concat(flags, "POSSESIVE ")
                                    If (DictEntriesList(i).v2_WordFlags And &H8000) = &H8000 Then flags = String.Concat(flags, "DONT-ORPHAN ")
                                    flags = String.Concat(flags.Trim, "]")
                                    Console.Write(flags)
                                End If
                            Case EnumCompilerSource.INFORM5, EnumCompilerSource.INFORM6
                                If (iPS And 128) = 128 Then sPS &= "+<noun>"
                                If (iPS And 8) = 8 Then sPS &= "+<adj>"
                                If (iPS And 4) = 4 Then sPS &= "+<plural>"
                                If (iPS And 2) = 2 Then sPS &= "+<meta>"
                                If (iPS And 1) = 1 Then sPS &= "+<verb>"
                                sPS = sPS.TrimStart("+"c)
                                If sPS.Contains("<verb>") Then sPS = sPS & ", Verb#=" & DictEntriesList(i).VerbNum.ToString
                                If grammarVer = 1 And iV2 > 0 Then sPS = sPS & ", Preposition#=" & DictEntriesList(i).PrepNum.ToString
                                Console.Write(" {0}", sPS)
                        End Select
                        Console.WriteLine()
                    Next
                    Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_ZCODE And showZCode And Not createGametext Then
                    ' ***** Z-CODE *****
                    If showOnlyAddress = 0 Then
                        oMemEntry.PrintMemoryLabel()
                        Console.WriteLine()
                        Console.WriteLine("***** Z-CODE ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                        If showHex Then HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                    End If
                    Console.WriteLine()
                    oDecode = New Decode
                    Dim iNextAddress As Integer = 0
                    Dim iNextValidAddress As Integer = 0
                    For i As Integer = 0 To validRoutinesList.Count - 1
                        If showOnlyAddress = 0 Or validRoutinesList(i).entryPoint = showOnlyAddress Then
                            iNextAddress = oDecode.DecodeRoutine(validRoutinesList(i).entryPoint, byteStory, sAbbreviations, False,
                                                                 zcodeSyntax, validStringsList, validRoutinesList, DictEntriesList,
                                                                 alphabet, iAddrInitialPC, oPropertyAnalyser.propertyNumberMin,
                                                                 oPropertyAnalyser.propertyNumberMax, showAbbrevsInsertion,
                                                                 inlineStrings, showZCodeVerbose)
                            Console.WriteLine()
                            iNextValidAddress = Helper.GetNextValidPackedAddress(byteStory, iNextAddress)
                            If iNextAddress <> iNextValidAddress Then
                                Console.Write("Padding:")
                                HexDump(iNextAddress, iNextValidAddress - 1, False)
                                Console.WriteLine()
                            End If
                            If i < validRoutinesList.Count - 1 AndAlso iNextValidAddress <> validRoutinesList(i + 1).entryPoint Then
                                Console.Write("Data/orphan fragment:")
                                HexDump(iNextValidAddress, validRoutinesList(i + 1).entryPoint - 1, True)
                                Console.WriteLine()
                            End If
                        End If
                    Next
                End If

                If oMemEntry.type = MemoryMapType.MM_STATIC_STRINGS And showStrings And Not createGametext Then
                    ' ***** STATIC STRINGS *****
                    oMemEntry.PrintMemoryLabel()
                    Console.WriteLine()
                    Console.WriteLine("***** STATIC STRINGS ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                    If showHex Then HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                    Console.WriteLine()
                    For Each oStringData As StringData In validStringsList
                        Console.WriteLine("{0:X5} S{1:D4} {2}{3}{4}", oStringData.entryPoint, oStringData.number, Convert.ToChar(34), oStringData.GetText(showAbbrevsInsertion), Convert.ToChar(34))
                    Next
                    Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_PREPOSITION_TABLE_COUNT And showGrammar And Not createGametext Then
                    ' ***** NUMBER OF PREPOSITIONS IN TABLE (WORD) *****
                    Console.WriteLine()
                    Console.WriteLine("***** NUMBER OF PREPOSITIONS IN TABLE (WORD) ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                    If showHex Then HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                    Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_WORD_FLAGS_TABLE And showDictionary And Not createGametext Then
                    ' ***** WORD FLAGS TABLE *****
                    oMemEntry.PrintMemoryLabel()
                    Console.WriteLine()
                    Console.WriteLine("***** WORD FLAGS TABLE ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                    If showHex Then HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                    Console.WriteLine()
                    Console.WriteLine("{0:X5} {1:X2} {2:X2}                    Length of table: {3}", oMemEntry.addressStart, byteStory(oMemEntry.addressStart), byteStory(oMemEntry.addressStart + 1), Helper.GetAdressFromWord(byteStory, oMemEntry.addressStart))
                    For i As Integer = oMemEntry.addressStart + 2 To oMemEntry.addressEnd Step 4
                        Dim flag As Integer = Helper.GetAdressFromWord(byteStory, i + 2)
                        Dim word As String = DictEntriesList.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, i)).dictWord
                        Console.Write("{0:X5} {1:X2} {2:X2} {3:X2} {4:X2}             '{5}',{6}flags={7}{8}", i, byteStory(i), byteStory(i + 1), byteStory(i + 2), byteStory(i + 3), word, Space(11 - word.Length), flag, Space(7 - flag.ToString.Length))
                        Dim flagString As String = "["
                        If (flag And &H8) = &H8 Then flagString = String.Concat(flagString, "FIRST-PERSON ")
                        If (flag And &H10) = &H10 Then flagString = String.Concat(flagString, "PLURAL-FLAG ")
                        If (flag And &H20) = &H20 Then flagString = String.Concat(flagString, "SECOND-PERSON ")
                        If (flag And &H40) = &H40 Then flagString = String.Concat(flagString, "THIRD-PERSON ")
                        If (flag And &H100) = &H100 Then flagString = String.Concat(flagString, "PRESENT-TENSE ")
                        If (flag And &H200) = &H200 Then flagString = String.Concat(flagString, "PAST-TENSE ")
                        If (flag And &H400) = &H400 Then flagString = String.Concat(flagString, "FUTURE-TENSE ")
                        If (flag And &H2000) = &H2000 Then flagString = String.Concat(flagString, "THING-PNF ")
                        If (flag And &H4000) = &H4000 Then flagString = String.Concat(flagString, "POSSESIVE ")
                        If (flag And &H8000) = &H8000 Then flagString = String.Concat(flagString, "DONT-ORPHAN ")
                        flagString = String.Concat(flagString.Trim, "]")
                        Console.WriteLine(flagString)
                    Next
                    Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_UNIDENTIFIED_DATA And showUnidentified And Not createGametext Then
                    ' ***** UNIDENTIFIED DATA *****
                    oMemEntry.PrintMemoryLabel()
                    Console.WriteLine()
                    Console.WriteLine("***** UNIDENTIFIED DATA ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                    HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                    Console.WriteLine()
                    Dim first As Boolean = True
                    For i As Integer = 0 To possibleStartOfArrays.Count - 1
                        If possibleStartOfArrays(i) >= oMemEntry.addressStart And possibleStartOfArrays(i) < oMemEntry.addressEnd Then
                            If first Then
                                Console.WriteLine("Possible starting points for arrays (from absolute addresses in")
                                Console.WriteLine("opcodes loadw/GET, loadb/GETB, storew/PUT, storeb/PUTB and globals):")
                                first = False
                            End If
                            Console.WriteLine("     0x{0:X5}", possibleStartOfArrays(i))
                        End If
                    Next
                    If Not first Then Console.WriteLine()
                End If

                If oMemEntry.type = MemoryMapType.MM_PADDING And showExtra And Not createGametext Then
                    ' ***** PADDING *****
                    oMemEntry.PrintMemoryLabel()
                    Console.WriteLine()
                    Console.WriteLine("***** PADDING ({0:X5}-{1:X5}, {2}) *****", oMemEntry.addressStart, oMemEntry.addressEnd, oMemEntry.SizeString)
                    HexDump(oMemEntry.addressStart, oMemEntry.addressEnd, True)
                    Console.WriteLine()
                End If

            Next

            If createGametext Then
                Console.WriteLine(String.Concat("I: Transcript of the text of ", Chr(34), sFilename, Chr(34)))
                Console.WriteLine("I: [I:info, G: game Text O: Object name, H:game text inline in opcode]")
                Console.WriteLine("I: [Compiled Z-machine version {0}]", iZVersion)
                For i = 0 To objectNames.Count - 1
                    Console.WriteLine(String.Concat("O: ", objectNames(i).Replace(Chr(34), "~")))
                Next
                For i = 0 To inlineStrings.Count - 1
                    If inlineStrings(i).text.StartsWith("z-routine size") Then
                        Console.WriteLine(String.Concat("I: ", inlineStrings(i).text.Replace(Chr(34), "~")))
                    Else
                        Console.WriteLine(String.Concat("H: ", inlineStrings(i).text.Replace(Chr(34), "~")))
                    End If
                Next
                For i = 0 To validStringsList.Count - 1
                    Console.WriteLine(String.Concat("G: ", validStringsList(i).text.Replace(Chr(34), "~")))
                Next
                Console.WriteLine("I: [Total bytes in Z-code padding, {0} bytes]", totalPadding)
            End If

        Catch ex As Exception
            Console.Error.WriteLine()
            Console.Error.WriteLine("[Failed: Corrupted or unknown format on story-file.]")
            Console.Error.WriteLine()
        End Try
    End Sub

    Private Sub PrintObjectTree(objectList As List(Of ObjectData), objectNo As Integer, indentation As Integer, printDescription As Boolean)
        Dim currentObject As ObjectData = objectList(objectNo - 1)

        If printDescription Then
            If showInvisiClues Then
                Console.WriteLine("{0}{1}", Space(indentation), currentObject.Description)
            Else
                Console.WriteLine("{0}[{1,3:###}] {2}", Space(indentation), objectNo, currentObject.Description)
            End If
        End If

        If showInvisiClues Then
            If currentObject.Property4Strings.Count > 0 Then
                For i As Integer = 0 To currentObject.Property4Strings.Count - 1
                    Console.WriteLine(Space(indentation + 3) & currentObject.Property4Strings(i).Replace("^", vbCrLf & Space(indentation + 3)))
                Next
            End If
            If currentObject.Property8Strings.Count > 0 Then
                For i As Integer = 0 To currentObject.Property8Strings.Count - 1
                    Console.WriteLine(Space(indentation + 3) & ">" & currentObject.Property8Strings(i).Replace("^", vbCrLf & Space(indentation + 4)))
                Next
                If currentObject.ChildId > 0 Then PrintObjectTree(objectList, currentObject.ChildId, indentation, False)
                If currentObject.NextId > 0 Then PrintObjectTree(objectList, currentObject.NextId, indentation, True)
            Else
                If currentObject.ChildId > 0 Then PrintObjectTree(objectList, currentObject.ChildId, indentation + 3, True)
                    If currentObject.NextId > 0 Then PrintObjectTree(objectList, currentObject.NextId, indentation, True)
                End If

                'Exit Sub
            Else
                If currentObject.ChildId > 0 Then PrintObjectTree(objectList, currentObject.ChildId, indentation + 3, True)
            If currentObject.NextId > 0 Then PrintObjectTree(objectList, currentObject.NextId, indentation, True)
        End If
    End Sub

    Private Function ExtractZString(piStringAddress As Integer, Optional pbHighlightAbbrevs As Boolean = False) As String
        Return Helper.ExtractZString(byteStory, piStringAddress, sAbbreviations, alphabet, pbHighlightAbbrevs)
    End Function

    Private Sub HexDump(iStartAddr As Integer, iEndAddr As Integer, bTabbed As Boolean)
        Dim sASCII As String = ""
        Dim iCount As Integer = 0

        If (iStartAddr Mod 16) > 0 Then
            Console.WriteLine()
            If bTabbed Then
                Console.Write("{0:X5} ", iStartAddr - (iStartAddr Mod 16))
                For i As Integer = 0 To (iStartAddr Mod 16) - 1
                    Console.Write("   ")
                    sASCII = String.Concat(sASCII, " ")
                Next
            Else
                Console.Write("{0:X5} ", iStartAddr)
            End If
        End If
        For i As Integer = iStartAddr To iEndAddr
            If (i Mod 16) = 0 Then
                If bTabbed Then Console.Write(" {0}", sASCII)
                sASCII = ""
                iCount = 0
                Console.WriteLine()
                Console.Write("{0:X5} ", i)
            End If
            Console.Write("{0:X2} ", byteStory(i))
            If byteStory(i) > 31 And byteStory(i) < 127 Then sASCII = String.Concat(sASCII, Convert.ToChar(byteStory(i))) Else sASCII = String.Concat(sASCII, ".")
            iCount += 1
        Next
        If bTabbed Then
            Console.Write(" {0}{1}", "                                                ".Substring(0, (16 - iCount) * 3), sASCII)
        End If
        Console.WriteLine()
    End Sub

    Private Function ExtractASCIIChars(piStringAddress As Integer, piLength As Integer) As String
        Dim sRet As String = ""
        For i As Integer = 0 To piLength - 1
            If byteStory(piStringAddress + i) < 32 Then
                sRet &= "."
            Else
                sRet &= System.Text.Encoding.ASCII.GetString(byteStory, piStringAddress + i, 1)
            End If
        Next

        Return sRet
    End Function

    Private Function IdentifyCompiler() As EnumCompilerSource
        ' Compiler is identified by looking at the serialnumber and username in the header.
        ' Modern compilers write a tag into username (Inform6, Zilf/Zapf & Dialog).
        ' A serialnumber earlier than 1993 --> Zilch (Infocom)
        ' All other defaults to earlier Inform (Inform5 and earlier)

        Dim tmp As Integer = 0
        Dim sSerial As String = ExtractASCIIChars(18, 6)

        If ExtractASCIIChars(60, 4) = "ZAPF" Then Return EnumCompilerSource.ZILF
        If ExtractASCIIChars(57, 3) = "Dia" Then Return EnumCompilerSource.DIALOG
        If ExtractASCIIChars(60, 1) = "6" And Integer.TryParse(ExtractASCIIChars(62, 2), tmp) Then Return EnumCompilerSource.INFORM6
        If sSerial > "700000" And sSerial < "930000" Then Return EnumCompilerSource.ZILCH

        ' Known faulty serialnumbers that are Zilch
        If sSerial = "000001" Then Return EnumCompilerSource.ZILCH 'leathergoddesses-r59-s000001.z3
        If sSerial = "??????" Then Return EnumCompilerSource.ZILCH 'moonmist-beta-r65-sXXXXXX.z3, spellbreaker-r63-sXXXXXX.z3, zork1-r5-sXXXXXX.z1 & zork1-r15-sXXXXXX.z2
        If sSerial = "UG3AU5" Then Return EnumCompilerSource.ZILCH 'zork1-r15-sUG3AU5.z2 & zork2-r7-sUG3AU5.z2
        If sSerial = "AS000C" Then Return EnumCompilerSource.ZILCH 'zork1-r2-sAS000C.z1
        If sSerial = "......" Then Return EnumCompilerSource.ZILCH 'zork1-r5-sXXXXXX.z1, zork1-r20-sXXXXXX.z3

        ' Default to Inform5
        Return EnumCompilerSource.INFORM5
    End Function

    Private Function ScanForGrammarTable(pNumberOfVerbs As Integer, pDictEntries As DictionaryEntries) As GrammarScanResult
        ' Grammar table (only identifiable for stories compiled with Inform, Zapf or Zilch
        ' See: Inform Technical Manual, Ch 8
        ' If no grammar are found it would be possible to use the algorithm from ScanForZilchGrammarTableV1 to *slowly* crawl through the memory 
        '       Pre-action for ZIL & INF_1

        ' Scan for grammar table
        Dim resultGrammarScan As GrammarScanResult = ScanForGrammarTableV1(pNumberOfVerbs)
        If resultGrammarScan Is Nothing And compilerSource = EnumCompilerSource.ZILCH Then resultGrammarScan = ScanForZilchGrammarTableV1(pNumberOfVerbs)
        If resultGrammarScan Is Nothing Then resultGrammarScan = ScanForInformGrammarTableV2(pNumberOfVerbs)
        If resultGrammarScan Is Nothing Then resultGrammarScan = ScanForZilchGrammarTableV2(pDictEntries)
        If resultGrammarScan Is Nothing Then resultGrammarScan = ScanForInformGrammarTableV3(pNumberOfVerbs)

        If resultGrammarScan Is Nothing Then resultGrammarScan = New GrammarScanResult

        If resultGrammarScan.NumberOfVerbs = 0 Then resultGrammarScan.NumberOfVerbs = pNumberOfVerbs

        Return resultGrammarScan
    End Function

    Private Function ScanForGrammarTableV1(pNumberOfVerbs As Integer) As GrammarScanResult
        ' V1 grammar tables version 1 (ZILCH, Zapf and Inform v1) have a constant length of 8 bytes for each variant
        ' of the verb + an extra byte for the number of variants of the verb. i.e. 9, 17, 25, 33 ...
        Dim potentialStartAddress = 0
        Dim iGrammarLines(9) As Integer
        Dim deadlineFix As Boolean
        Do
            Dim foundCandidate As Boolean = True
            Dim compactSyntax As Boolean = False
            deadlineFix = False
            ' Do a quick scan to with 10 verbs and see if we find a potential verb table start address
            For i As Integer = 0 To 9
                ' Get potential address for a verb table entry
                iGrammarLines(i) = byteStory(potentialStartAddress + i * 2) * 256 + byteStory(potentialStartAddress + i * 2 + 1)

                If iGrammarLines(i) < &H40 Or iGrammarLines(i) > byteStory.Length Then
                    ' Invalid address for verb table entry, current start is invalid, abandon
                    foundCandidate = False
                    Exit For
                End If

                If i > 0 Then

                    ' Current verb entry must be bigger than previous. If not, abandon 
                    If iGrammarLines(i) < iGrammarLines(i - 1) Then
                        foundCandidate = False
                        Exit For
                    End If

                    Dim syntaxLineCountForPrevious As Integer = byteStory(iGrammarLines(i - 1))

                    ' If size of syntax line is less than minimum for a COMPACT-SYNTAX then abandon
                    If (iGrammarLines(i) - iGrammarLines(i - 1) - 1) < 2 Then
                        foundCandidate = False
                        Exit For
                    End If

                    If iGrammarLines(i - 1) + syntaxLineCountForPrevious * 8 + 1 = iGrammarLines(i) Then
                        ' Addresses are consistent with a grammar table with fixed syntax lines of 8, proceed to next
                        Continue For
                    ElseIf iGrammarLines(i - 1) + syntaxLineCountForPrevious * 8 + 2 = iGrammarLines(i) And byteStory(iGrammarLines(i) - 1) = 0 Then
                        ' Deadline have an extra empty byte between each syntax verb definition
                        deadlineFix = True
                        Continue For
                    Else
                        ' test for COMPACT-SYNTAX
                        Dim nextAddress As Integer = iGrammarLines(i - 1) + 1
                        For j As Integer = 0 To syntaxLineCountForPrevious - 1
                            Select Case (byteStory(nextAddress) And &HC0)
                                Case &H0 : nextAddress += 2
                                Case &H40 : nextAddress += 4
                                Case &H80 : nextAddress += 7
                            End Select
                        Next
                        If nextAddress = iGrammarLines(i) Then
                            ' Addresses are consistent with a grammar table with COMPACT-SYNTAX syntax lines, proceed to next
                            compactSyntax = True
                            Continue For
                        Else
                            foundCandidate = False
                            Exit For
                        End If
                    End If
                End If
                If Not foundCandidate Then Exit For
            Next

            ' Found potential grammar table start address, check for validity for full set of verbs by:
            '   * Check that the next value in the table is the same as the length of the actual grammar lines for that verb
            If foundCandidate Then
                Dim iVerbIdx As Integer = 0
                Dim oRet As New GrammarScanResult
                Dim iMaxAction As Integer = 0
                Do
                    Dim iAddr As Integer = Helper.GetAdressFromWord(byteStory, potentialStartAddress + iVerbIdx * 2)
                    Dim iAddrNext As Integer = Helper.GetAdressFromWord(byteStory, potentialStartAddress + (iVerbIdx + 1) * 2)
                    Dim iAddrNextCalc As Integer
                    If compactSyntax Then
                        iAddrNextCalc = iAddr + 1
                        For j As Integer = 0 To byteStory(iAddr) - 1
                            If byteStory(iAddrNextCalc + 1) > iMaxAction Then iMaxAction = byteStory(iAddrNextCalc + 1)
                            Select Case (byteStory(iAddrNextCalc) And &HC0)
                                Case &H0 : iAddrNextCalc += 2
                                Case &H40 : iAddrNextCalc += 4
                                Case &H80 : iAddrNextCalc += 7
                            End Select
                        Next
                    Else
                        iAddrNextCalc = iAddr + byteStory(iAddr) * 8 + 1
                        If deadlineFix Then iAddrNextCalc += 1
                        For j As Integer = iAddr + 8 To iAddrNextCalc - 1 Step 8
                            If byteStory(j) > iMaxAction Then iMaxAction = byteStory(j)
                        Next
                    End If
                    If iVerbIdx < pNumberOfVerbs - 1 And iAddrNext <> iAddrNextCalc Then foundCandidate = False
                    If oRet.addrGrammarDataStart = 0 Or oRet.addrGrammarDataStart > iAddr Then oRet.addrGrammarDataStart = iAddr
                    If oRet.addrGrammarDataEnd = 0 Or oRet.addrGrammarDataEnd < iAddrNextCalc Then oRet.addrGrammarDataEnd = iAddrNextCalc - 1
                    iVerbIdx += 1
                Loop Until Not foundCandidate Or iVerbIdx = pNumberOfVerbs

                If foundCandidate Then
                    oRet.addrGrammarTableStart = potentialStartAddress
                    oRet.addrGrammarTableEnd = potentialStartAddress + pNumberOfVerbs * 2 - 1
                    oRet.NumberOfActions = iMaxAction + 1
                    oRet.grammarVer = EnumGrammarVer.VERSION_1
                    oRet.CompactSyntaxes = compactSyntax
                    oRet.NumberOfVerbs = pNumberOfVerbs
                    Return oRet
                End If
            End If

            potentialStartAddress += 1
        Loop Until potentialStartAddress > byteStory.Length - 100
        Return Nothing
    End Function

    Private Function ScanForInformGrammarTableV2(pNumberOfVerbs As Integer) As GrammarScanResult
        ' Inform v2 grammar table and grammar table data
        ' Grammar table: Array containing addresses of grammars (syntax lines) in grammar table data,
        '                one for each Inform verb starting at verb 255, then 254 and so on.
        ' Grammar table data: Starts with one byte that holds the number of syntax lines for this verb.
        '                     Then follow each syntax line of variable length, terminated by &HF
        '                     Each syntax line is composed like:
        '                        2 bytes: Action number. Points to position in action table that 
        '                                 holds address of action routine.
        '                       *3 bytes: Tokens in multiples of 3 (token 1 ... Token N)
        '                        1 byte:  ENDIT, &HF
        Dim iIndex = 0
        Do
            Dim iFound As Integer = 0
            For i As Integer = 0 To 9
                Dim iGrammarLines(9) As Integer
                iGrammarLines(i) = byteStory(iIndex + i * 2) * 256 + byteStory(iIndex + i * 2 + 1)
                If iGrammarLines(i) = 0 Or iGrammarLines(i) > byteStory.Length Then
                    iFound = -1
                    Exit For
                End If
                If i > 0 Then
                    If byteStory(iGrammarLines(i - 1)) = 0 AndAlso (iGrammarLines(i) - iGrammarLines(i - 1) - 1) = 0 Then
                        ' no-verb, allowed
                    Else
                        ' If byte before the current pointer points to contains &HF
                        If byteStory(iGrammarLines(i) - 1) <> &HF Then iFound = -1
                        If (iGrammarLines(i) - iGrammarLines(i - 1) - 1) < 2 Then iFound = -1
                    End If
                End If
                If iFound <> 0 Then Exit For
            Next
            If iFound = 0 Then
                ' Found potential grammar table, check for validity by:
                '   * Check that the next value in the table is the same as the length of the actual grammar lines for that verb
                Dim iVerbIdx As Integer = 0
                Dim iMaxAction As Integer = 0
                Dim oRet As New GrammarScanResult
                Do
                    Dim iAddr As Integer = Helper.GetAdressFromWord(byteStory, iIndex + iVerbIdx * 2)
                    Dim iAddrNext As Integer = Helper.GetAdressFromWord(byteStory, iIndex + (iVerbIdx + 1) * 2)
                    Dim iAddrNextCalc As Integer = iAddr
                    Dim iNum15 As Integer = byteStory(iAddrNextCalc)
                    If iNum15 = 0 Then
                        iAddrNextCalc += 1 ' Verb with no grammar lines is legal, just ignore
                    Else
                        Dim iTmpAction1 As Integer = (Helper.GetAdressFromWord(byteStory, iAddrNextCalc + 1) And 1023)
                        If iTmpAction1 > iMaxAction Then iMaxAction = iTmpAction1
                        Do
                            iAddrNextCalc += 3
                            If byteStory(iAddrNextCalc) = 15 Then
                                iNum15 -= 1
                                If iNum15 > 0 Then
                                    Dim iTmpAction2 As Integer = (Helper.GetAdressFromWord(byteStory, iAddrNextCalc + 1) And 1023)
                                    If iTmpAction2 > iMaxAction Then iMaxAction = iTmpAction2
                                End If
                            End If
                        Loop Until iNum15 = 0
                        iAddrNextCalc += 1
                    End If
                    If iVerbIdx < pNumberOfVerbs - 1 And iAddrNext <> iAddrNextCalc Then iFound = -1
                    If oRet.addrGrammarDataStart = 0 Or oRet.addrGrammarDataStart > iAddr Then oRet.addrGrammarDataStart = iAddr
                    If oRet.addrGrammarDataEnd = 0 Or oRet.addrGrammarDataEnd < iAddrNextCalc Then oRet.addrGrammarDataEnd = iAddrNextCalc
                    iVerbIdx += 1
                Loop Until iFound <> 0 Or iVerbIdx = pNumberOfVerbs
                If iFound = 0 Then
                    oRet.addrGrammarTableStart = iIndex
                    oRet.addrGrammarTableEnd = iIndex + pNumberOfVerbs * 2
                    oRet.addrGrammarTableEnd -= 1
                    oRet.NumberOfActions = iMaxAction + 1
                    oRet.grammarVer = EnumGrammarVer.VERSION_2
                    oRet.NumberOfVerbs = pNumberOfVerbs
                    Return oRet
                End If
            End If
            iIndex += 1
        Loop Until iIndex > byteStory.Length - 100
        Return Nothing
    End Function

    Private Function ScanForInformGrammarTableV3(pNumberOfVerbs As Integer) As GrammarScanResult
        ' Inform v3 grammar table and grammar table data
        ' Grammar table: Array containing addresses of grammars (syntax lines) in grammar table data,
        '                one for each Inform verb starting at verb 255, then 254 and so on.
        ' Grammar table data: Starts with one byte that holds the number of syntax lines for this verb.
        '                     Then follow each syntax line of variable length, terminated by &HF
        '                     Each syntax line is composed like:
        '                        2 bytes: Action number. Points to position in action table that 
        '                                 holds address of action routine.
        '                                  bits 0-9  : Action number
        '                                  bit 10    : Reverse bit
        '                                  bits 11-15: Number of tokens for this line
        '                       *2 bytes: Tokens in multiples of 2 (token 1 ... Token N)
        ' GV3 was introduced in Inform 6.43 and the grammar table always starts at static memory start
        ' and the sequence are always: Grammar table
        '                              Grammar table data
        '                              Actions table
        '                              Parsing routines table
        '                              Adjectives table

        ' Init potential start addresses
        Dim startGrammarTable As Integer = Helper.GetAdressFromWord(byteStory, 14)
        Dim startZCode As Integer = Helper.GetAdressFromWord(byteStory, 4)
        Dim startGrammarTableData As Integer = startGrammarTable + 2 * pNumberOfVerbs
        Dim endGrammarTableData As Integer = 0

        Dim foundGV3 As Boolean = True      ' Assume GV3
        Dim maxActionNumber As Integer = 0
        Dim maxAdjectiveNumber As Integer = -1
        Dim maxParsingRoutineNumber As Integer = -1

        ' Check 1: Control that all entries in the grammar table points to an address
        '          between startGrammarTableData and startZCode.
        For verb As Integer = 0 To pNumberOfVerbs - 1
            Dim grammarTableData As Integer = Helper.GetAdressFromWord(byteStory, startGrammarTable + verb * 2)
            If grammarTableData < startGrammarTableData Then foundGV3 = False
            If grammarTableData > startZCode Then foundGV3 = False
        Next

        ' Check 2: Control structure of the grammar table data and collect max action number
        If foundGV3 Then
            For verb = 0 To pNumberOfVerbs - 1
                Dim grammarTableData As Integer = Helper.GetAdressFromWord(byteStory, startGrammarTable + verb * 2)
                Dim nextGrammarTableData As Integer = Helper.GetAdressFromWord(byteStory, startGrammarTable + (verb + 1) * 2)
                Dim index As Integer = grammarTableData
                Dim grammarLines As Integer = byteStory(grammarTableData)
                index += 1
                For grammarLine As Integer = 0 To grammarLines - 1
                    Dim actionNumber As Integer = (Helper.GetAdressFromWord(byteStory, index) And &H3FF)
                    Dim tokens As Integer = (Helper.GetAdressFromWord(byteStory, index) And &HF800) >> 11
                    If actionNumber > maxActionNumber Then maxActionNumber = actionNumber
                    index += 2

                    ' Scan for adjectives and parsing routines
                    For i As Integer = 0 To tokens - 1
                        Dim tokenData As Integer = byteStory(index + i * 2 + 1)
                        If (byteStory(index + i * 2) And &HC0) = &H80 Then
                            ' Parsing routine
                            If tokenData > maxParsingRoutineNumber Then maxParsingRoutineNumber = tokenData
                        End If
                        If (byteStory(index + i * 2) And &H40) = &H40 Then
                            ' Adjective/preposition
                            If tokenData > maxAdjectiveNumber Then maxAdjectiveNumber = tokenData
                        End If
                    Next
                    index += tokens * 2
                Next
                If (verb + 1) < pNumberOfVerbs And index <> nextGrammarTableData Then foundGV3 = False
                endGrammarTableData = index
            Next
        End If

        ' Check 3: First word in adjective/preposition table should match number of adjectives
        Dim startAdjectiveTable As Integer = endGrammarTableData + (maxActionNumber + 1) * 2 + (maxParsingRoutineNumber + 1) * 2
        If Helper.GetAdressFromWord(byteStory, startAdjectiveTable) <> (maxAdjectiveNumber + 1) Then foundGV3 = False

        ' Return result
        If foundGV3 Then
            Dim oRet As New GrammarScanResult With {
                .addrGrammarTableStart = startGrammarTable,
                .addrGrammarTableEnd = startGrammarTable + pNumberOfVerbs * 2 - 1,
                .NumberOfActions = maxActionNumber + 1,
                .addrGrammarDataStart = startGrammarTableData,
                .addrGrammarDataEnd = endGrammarTableData - 1,
                .grammarVer = EnumGrammarVer.VERSION_3,
                .NumberOfVerbs = pNumberOfVerbs,
                .NumberOfParsingRoutines = maxParsingRoutineNumber + 1,
                .NumberOfPrepositions = maxAdjectiveNumber + 1
            }
            Return oRet
        Else
            Return Nothing
        End If

    End Function

    Private Function ScanForZilchGrammarTableV1(pNumberOfVerbs As Integer) As GrammarScanResult
        ' Some early Infocom games compiled with Zilch don't have a "compact" storage of the grammar table data.
        ' There is some stray empty bytes between the syntax lines. But instead they always start at the
        ' start of the static memory.
        ' This function checks if there is a valid grammar table at this position by:
        '   * Check that byte defining number of grammer lines is below 16
        '   * Check that the action numbers (last byte of each grammar line) are contigious         
        Dim iIndex = Helper.GetAdressFromWord(byteStory, &HE)       ' PURBOT, start of static mem
        Dim bActions(255) As Boolean
        Dim bOk As Boolean = True
        Dim oRet As New GrammarScanResult
        For i As Integer = 0 To 255
            bActions(i) = False
        Next
        For i As Integer = iIndex To (iIndex + (pNumberOfVerbs - 1) * 2) Step 2
            Dim iAddr As Integer = Helper.GetAdressFromWord(byteStory, i)
            Dim iCount As Integer = byteStory(iAddr)
            If iCount > 15 Then
                bOk = False
                Exit For
            End If
            For j As Integer = 0 To iCount - 1
                bActions(byteStory(iAddr + 8 * j + 8)) = True
            Next
            If oRet.addrGrammarDataStart = 0 Or oRet.addrGrammarDataStart > iAddr Then oRet.addrGrammarDataStart = iAddr
            If oRet.addrGrammarDataEnd = 0 Or oRet.addrGrammarDataEnd < (iAddr + iCount * 8) Then oRet.addrGrammarDataEnd = iAddr + iCount * 8
        Next

        ' All action number should be contigious
        Dim iMaxAction As Integer = -1
        For i = 0 To 255
            If bActions(i) = False And iMaxAction < 0 Then iMaxAction = i - 1
            If bActions(i) And iMaxAction > -1 Then bOk = False
        Next
        If bOk Then
            oRet.addrGrammarTableStart = iIndex
            oRet.addrGrammarTableEnd = iIndex + pNumberOfVerbs * 2
            oRet.addrGrammarTableEnd -= 1
            oRet.NumberOfActions = iMaxAction + 1
            oRet.grammarVer = EnumGrammarVer.VERSION_1
            oRet.NumberOfVerbs = pNumberOfVerbs
            Return oRet
        End If
        Return Nothing
    End Function

    Private Function ScanForZilchGrammarTableV2(pDictEntries As DictionaryEntries) As GrammarScanResult
        ' Arthur, Shogun and Zork0 use the "new parser"
        ' This is from basedefs.mud
        ' "The semantic-stuff is heavily overloaded. If it's non-zero for a buzzword
        '  (classification number Is 0), it points to a word of which this word is a
        '  synonym. For a verb, it's the pointer to the verb data structure. For a
        '  direction, the high byte Is the direction ID (thus a word can't be both a
        '  verb And a direction).  In all other cases, it's either 0 or a pointer to
        '  a related word (e.g., the singular word of which this is the plural)."

        ' Dictionary is either 9 or 10 bytes. Last byte is part-of-speech where it's a verb
        ' if bit 0 is set or a direction if bit 5 is set. If it's a verb an address is stored
        ' in byte 6 and 7 (word is stored in byte 0-5). If it's a direction, the direction 
        ' number is stored in byte 6.
        ' Address found should be discarded if it's 0 or pointing inside dictionary,
        ' otherwise it points to 8 bytes:
        '    AA AA BB BB CC CC DD DD
        ' A. $FFFF or points to position in action-/pre-action-table
        ' B. 0 or pointing to dictionary word (preposition)
        ' C. 0 or pointing to address of 1 object definitions (length+length*6)
        ' D. 0 or pointing to address of 2 object definitions (length+length*10)
        ' Each entry in the grammar table is 8 bytes so to be a valid v2-dictionary the 
        ' highest verb-address minus the lowest should be 8 * number of verbs.

        Dim iAddressHigh0 As Integer = 0
        Dim iAddressLow0 As Integer = &HFFFF
        Dim iAddressHigh1_2 As Integer = 0
        Dim iAddressLow1_2 As Integer = &HFFFF
        Dim iVerbCount As Integer = 0
        Dim VerbList As New List(Of Integer)
        Dim iActionCount As Integer = 0

        If pDictEntries.WordSize = 9 Or pDictEntries.WordSize = 11 Then pDictEntries.v2_OneBytePartsOfSpeech = True
        If pDictEntries.WordSize > 10 Then pDictEntries.v2_WordFlagsInTable = False

        For Each oDictEntry As DictionaryEntry In pDictEntries
            Dim iAddress As Integer = 0
            If (oDictEntry.Byte1ToLast And &H1) = &H1 Then  ' VERB?
                If pDictEntries.v2_OneBytePartsOfSpeech And pDictEntries.v2_WordFlagsInTable Then iAddress = oDictEntry.Byte3ToLast * 256 + oDictEntry.Byte2ToLast
                If Not pDictEntries.v2_OneBytePartsOfSpeech And pDictEntries.v2_WordFlagsInTable Then iAddress = oDictEntry.Byte4ToLast * 256 + oDictEntry.Byte3ToLast
                If pDictEntries.v2_OneBytePartsOfSpeech And Not pDictEntries.v2_WordFlagsInTable Then iAddress = oDictEntry.Byte5ToLast * 256 + oDictEntry.Byte4ToLast
                If Not pDictEntries.v2_OneBytePartsOfSpeech And Not pDictEntries.v2_WordFlagsInTable Then iAddress = oDictEntry.Byte6ToLast * 256 + oDictEntry.Byte5ToLast

                If iAddress >= byteStory.Length Then Return Nothing

                If iAddress > 0 And pDictEntries.GetEntryAtAddress(iAddress) Is Nothing Then

                    ' Check that word 2 is a valid number
                    Dim iPrep As Integer = Helper.GetAdressFromWord(byteStory, iAddress + 2)
                    If Not iPrep = 0 And pDictEntries.GetEntryAtAddress(iPrep) Is Nothing Then Return Nothing

                    ' Check that distance between two-obj-table and one-obj-table is valid
                    Dim iOneObject As Integer = Helper.GetAdressFromWord(byteStory, iAddress + 4)
                    Dim iTwoObject As Integer = Helper.GetAdressFromWord(byteStory, iAddress + 6)
                    If iOneObject > 0 And iTwoObject > 0 And Not ((iTwoObject - iOneObject - 2) Mod 6) = 0 Then Return Nothing

                    ' Count unique verbs
                    If Not VerbList.Contains(iAddress) Then
                        iVerbCount += 1
                        VerbList.Add(iAddress)
                    End If

                    ' Inspect one-object-table
                    If iOneObject > 0 Then
                        Dim iCount As Integer = Helper.GetAdressFromWord(byteStory, iOneObject)
                        For i As Integer = 0 To iCount - 1
                            Dim iAction1 As Integer = Helper.GetAdressFromWord(byteStory, iOneObject + 2 + i * 6)
                            Dim iPrep1 As Integer = Helper.GetAdressFromWord(byteStory, iOneObject + 2 + i * 6 + 2)
                            If Not iPrep1 = 0 And pDictEntries.GetEntryAtAddress(iPrep1) Is Nothing Then Return Nothing
                            If iAction1 > iActionCount Then iActionCount = iAction1
                        Next
                        If iOneObject < iAddressLow1_2 Then iAddressLow1_2 = iOneObject
                        If iOneObject + 2 + iCount * 6 > iAddressHigh1_2 Then iAddressHigh1_2 = iOneObject + 2 + iCount * 6
                    End If

                    ' Inspect two-object-table
                    If iTwoObject > 0 Then
                        Dim iCount As Integer = Helper.GetAdressFromWord(byteStory, iTwoObject)
                        For i As Integer = 0 To iCount - 1
                            Dim iAction2 As Integer = Helper.GetAdressFromWord(byteStory, iTwoObject + 2 + i * 10)
                            Dim iPrep1 As Integer = Helper.GetAdressFromWord(byteStory, iTwoObject + 2 + i * 10 + 2)
                            Dim iPrep2 As Integer = Helper.GetAdressFromWord(byteStory, iTwoObject + 2 + i * 10 + 6)
                            If Not iPrep1 = 0 And pDictEntries.GetEntryAtAddress(iPrep1) Is Nothing Then Return Nothing
                            If Not iPrep2 = 0 And pDictEntries.GetEntryAtAddress(iPrep2) Is Nothing Then Return Nothing
                            If iAction2 > iActionCount Then iActionCount = iAction2
                        Next
                        If iTwoObject < iAddressLow1_2 Then iAddressLow1_2 = iTwoObject
                        If iTwoObject + 2 + iCount * 10 > iAddressHigh1_2 Then iAddressHigh1_2 = iTwoObject + 2 + iCount * 10
                    End If

                    If iAddress < iAddressLow0 Then iAddressLow0 = iAddress
                    If iAddress + 8 > iAddressHigh0 Then iAddressHigh0 = iAddress + 8

                    Dim iAction As Integer = Helper.GetAdressFromWord(byteStory, iAddress)
                    If iAction < 65535 And iAction > iActionCount Then iActionCount = iAction
                End If
            End If
        Next

        If iAddressLow0 = &HFFFF Then Return Nothing        ' Not found

        Dim oRet As New GrammarScanResult With {
            .addrGrammarTableStart = iAddressLow0,
            .addrGrammarTableEnd = iAddressHigh0 - 1
        }
        If (iAddressLow1_2 > iAddressLow0 And iAddressLow1_2 < iAddressHigh0) Or (iAddressHigh1_2 > iAddressLow0 And iAddressHigh1_2 < iAddressHigh0) Then
            ' 0, 1 & 2 are mixed in same table space
            If iAddressLow1_2 < iAddressLow0 Then oRet.addrGrammarTableStart = iAddressLow1_2
            If iAddressHigh1_2 > iAddressLow0 Then oRet.addrGrammarTableEnd = iAddressHigh1_2 - 1
        Else
            ' 0 are seperate from 1 & 2
            oRet.addrGrammarDataStart = iAddressLow1_2
            oRet.addrGrammarDataEnd = iAddressHigh1_2 - 1
        End If
        oRet.NumberOfVerbs = iVerbCount
        oRet.NumberOfActions = iActionCount + 1
        oRet.grammarVer = EnumGrammarVer.VERSION_2
        pDictEntries.v2_VerbAddresses = VerbList
        Return oRet
    End Function

    Private Function ScanForActionTable(pNumberOfActions As Integer, pValidRoutines As List(Of RoutineData), pStartIndex As Integer) As Integer
        ' Scan for continous area of addresses to valid routines (length = number of actions) 
        Dim iIndex As Integer = pStartIndex
        Do
            Dim bFound As Boolean = True
            For i As Integer = 0 To 2 * (pNumberOfActions - 1) Step 2
                Dim iAddr As Integer = iIndex + i
                If pValidRoutines.Find(Function(x) x.entryPointPacked = Helper.GetAdressFromWord(byteStory, iAddr)) Is Nothing Then
                    bFound = False
                    Exit For
                End If
            Next
            If bFound Then Return iIndex
            iIndex += 1
        Loop Until iIndex > byteStory.Length - 100
        Return 0
    End Function

    Private Function ScanForZilV1PrepositionTable(pDictList As DictionaryEntries, compactVocablulary As Boolean) As Integer
        ' ZIL also have a preposition table in version 1 with slightly different makeup depending on COMPACT-VOCABULARY = yes/no
        ' COMPACT-VOCABULARY = no
        '   Prep numbers can appear in arbitrary order
        '   Only contains one entry for each prep number (synonyms are looked up in dictionary)
        '      2 bytes with the length of the table
        '      4 bytes with dictionary address in 2 bytes and preposition number in 2 bytes
        ' COMPACT-VOCABULARY = yes
        '   Prep numbers can appear in arbitrary order
        '   Only contains all prepositions including synonyms, there are no prep numbers in the dictionary
        '      2 bytes with the length of the table
        '      4 bytes with dictionary address in 2 bytes and preposition number in 1 byte
        Dim potentialAddress As Integer = Helper.GetAdressFromWord(byteStory, &HE) - 1 ' Start search in Static memory
        Dim foundTable As Boolean = False
        Dim prepositionAddresses As New HashSet(Of Integer)
        Dim allDictAddresses As New HashSet(Of Integer)
        Dim totalPrepositionsInDictionary As Integer = pDictList.PrepositionCountTotal
        Dim UniquePrepositionsInDictionary As Integer = pDictList.PrepositionCountUnique

        ' Build hashtable with all addresses in the dictionary to prepositions
        For Each dictEntry In pDictList
            If (dictEntry.Flags And 8) = 8 Then prepositionAddresses.Add(dictEntry.dictAddress)
            allDictAddresses.Add(dictEntry.dictAddress)
        Next

        Do
            potentialAddress += 1

            ' Search for a potential start of a preposition table
            If prepositionAddresses.Contains(Helper.GetAdressFromWord(byteStory, potentialAddress)) OrElse allDictAddresses.Contains(Helper.GetAdressFromWord(byteStory, potentialAddress)) Then
                ' Points to a preposition word (or any dictword, really)

                ' position before should contain number of entries in table, each entry should point to a preposition
                Dim count As Integer = Helper.GetAdressFromWord(byteStory, potentialAddress - 2)

                If count = 0 Then Continue Do  ' can't be an empty table

                For i = 0 To count - 1
                    If compactVocablulary Then
                        ' First two bytes should point to a preposition, now that's not always true but at least it should be a dictionary word
                        ' prep number must be a legal one
                        If Not prepositionAddresses.Contains(Helper.GetAdressFromWord(byteStory, potentialAddress + i * 3)) AndAlso Not allDictAddresses.Contains(Helper.GetAdressFromWord(byteStory, potentialAddress + i * 3)) Then Continue Do
                        If Not byteStory(potentialAddress + i * 3 + 2) > 255 - totalPrepositionsInDictionary Then Continue Do
                    Else
                        ' First two bytes should point to a preposition, now that's not always true but at least it should be a dictionary word
                        ' Third byte should always be 0
                        ' prep number must be a legal one
                        If Not prepositionAddresses.Contains(Helper.GetAdressFromWord(byteStory, potentialAddress + i * 4)) AndAlso Not allDictAddresses.Contains(Helper.GetAdressFromWord(byteStory, potentialAddress + i * 4)) Then Continue Do
                        If Not byteStory(potentialAddress + i * 4 + 2) = 0 Then Continue Do
                        If Not byteStory(potentialAddress + i * 4 + 3) > 255 - UniquePrepositionsInDictionary - 10 Then Continue Do    ' Add some buffer for example Ballyhoo
                    End If
                Next
                foundTable = True
            End If
        Loop Until foundTable Or potentialAddress > byteStory.Length - 1000

        If foundTable Then Return potentialAddress - 2 Else Return 0
    End Function

    Private Function ScanForInformV1AdjectiveTable(pDictList As DictionaryEntries) As Integer
        ' The adjective table (in reality it is a table of prepositions) is a table containing 
        ' cross-reference between the preposition number and its physical address.
        ' The format is (lowest preposition number first and ascending):
        '     <dictionary address of word>  00  <adjective number>
        '     ----2 bytes-----------------  ----2 bytes----------- 

        ' Build footprint for table
        Dim footprintList As New List(Of Byte)
        Dim iLowestPrepNumber As Integer = 256 - pDictList.PrepositionCountUnique
        For i As Integer = iLowestPrepNumber To 255
            Dim dictEntries As DictionaryEntries = pDictList.GetPreposition(i)
            If dictEntries.Count > 0 Then
                Dim oDictEntry As DictionaryEntry = dictEntries(0)      ' There should be only ONE!!!
                footprintList.Add(CByte(oDictEntry.dictAddress >> 8))
                footprintList.Add(CByte(oDictEntry.dictAddress And 255))
                footprintList.Add(0)
                footprintList.Add(CByte(oDictEntry.PrepNum))
            End If
        Next
        Dim iAddr As Integer = 0
        Dim bFound As Boolean
        Dim maxMatch As Integer = 0
        Dim maxMatchAddress As Integer = 0
        Do
            bFound = True
            For i = 0 To footprintList.Count - 1
                If byteStory(iAddr + i) <> footprintList(i) Then
                    bFound = False
                    If i > maxMatch Then
                        maxMatch = i
                        maxMatchAddress = iAddr
                    End If
                    Exit For
                End If
            Next
            If bFound Then Return iAddr - 2         ' Found perfect match
            iAddr += 1
        Loop Until iAddr > byteStory.Length - 100
        Return maxMatchAddress - 2          ' No perfect match, return best match. Curses_r7 have misformed table here
    End Function

    Public ZilPropDirectionList As New List(Of Integer)

    Private Sub DecodePropertyData(silent As Boolean, pStoryData() As Byte, pDictEntries As DictionaryEntries, pStringsList As List(Of StringData), pRoutineList As List(Of RoutineData), pPropNum As Integer, pPropSize As Integer, pPropStart As Integer, pPropData As String, pObjectNum As Integer, pObjectName As String)
        Dim objectName As String = String.Concat((pObjectNum + 1).ToString, " (", Chr(34), pObjectName, Chr(34), ")")
        If {EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF}.Contains(compilerSource) Then
            If ZilPropDirectionList.Contains(pPropNum) Then
                If Not silent Then
                    For Each oDictEntry As DictionaryEntry In pDictEntries.GetDirection(pPropNum)
                        If oDictEntry.dictWord.Length > 1 Then
                            Console.Write("({0} ", oDictEntry.dictWord.ToUpper)
                            Exit For
                        End If
                    Next
                End If
                If iZVersion < 4 Then
                    If Not silent Then
                        Select Case pPropSize
                            Case 1 'UEXIT
                                Console.Write("TO OBJECT-{0})", pStoryData(pPropStart))
                            Case 2 'NEXIT
                                Console.Write("{0}{1}{2})", Convert.ToChar(34), ExtractZString(Helper.GetAdressFromPacked(pStoryData, pPropStart, False), True), Convert.ToChar(34))
                            Case 3 'FEXIT
                                Console.Write("PER Routine at 0x{0:X5})", Helper.GetAdressFromPacked(pStoryData, pPropStart, True))
                            Case 4 'CEXIT - Note that data are in the order: REXIT (byte), CEXITFLAG (byte) & CEXITSTR (word) = 4 bytes 
                                Console.Write("TO OBJECT-{0} IF GLOBAL-{1}", pStoryData(pPropStart), pStoryData(pPropStart + 1) - 15)
                                If Helper.GetAdressFromWord(pStoryData, pPropStart + 2) > 0 Then
                                    Console.Write(" ELSE {0}{1}{2}", Convert.ToChar(34), ExtractZString(Helper.GetAdressFromPacked(pStoryData, pPropStart + 2, False), showAbbrevsInsertion), Convert.ToChar(34))
                                End If
                                Console.Write(")")
                            Case 5 'DEXIT
                                Console.Write("TO OBJECT-{0} IF OBJECT-{1} IS OPEN", pStoryData(pPropStart), pStoryData(pPropStart + 1))
                                If Helper.GetAdressFromWord(pStoryData, pPropStart + 2) > 0 Then
                                    Console.Write(" ELSE {0}{1}{2}", Convert.ToChar(34), ExtractZString(Helper.GetAdressFromPacked(pStoryData, pPropStart + 2, False), showAbbrevsInsertion), Convert.ToChar(34))
                                End If
                                Console.Write(")")
                        End Select
                    Else
                        Select Case pPropSize
                            Case 3 'FEXIT
                                Dim oRoutineData As RoutineData = pRoutineList.Find(Function(c) c.entryPointPacked = Helper.GetAdressFromWord(pStoryData, pPropStart))
                                If oRoutineData IsNot Nothing Then oRoutineData.callsFrom.Add(String.Concat("Called from property #", pPropNum.ToString, " at object #", objectName))
                        End Select
                    End If
                Else
                    If Not silent Then
                        Select Case pPropSize
                            Case 2 'UEXIT
                                Console.Write("TO OBJECT-{0})", Helper.GetAdressFromWord(pStoryData, pPropStart))
                            Case 3 'NEXIT
                                Console.Write("{0}{1}{2})", Convert.ToChar(34), ExtractZString(Helper.GetAdressFromPacked(pStoryData, pPropStart, False), showAbbrevsInsertion), Convert.ToChar(34))
                            Case 4 'FEXIT
                                Console.Write("PER Routine at 0x{0:X5})", Helper.GetAdressFromPacked(pStoryData, pPropStart, True))
                            Case 5 'CEXIT - Note that data are in the order: REXIT (word), CEXITSTR (word) & CEXITFLAG (byte) = 5 bytes
                                Console.Write("TO OBJECT-{0} IF GLOBAL-{1}", Helper.GetAdressFromWord(pStoryData, pPropStart), pStoryData(pPropStart + 4) - 15)
                                If Helper.GetAdressFromWord(pStoryData, pPropStart + 2) > 0 Then
                                    Console.Write(" ELSE {0}{1}{2}", Convert.ToChar(34), ExtractZString(Helper.GetAdressFromPacked(pStoryData, pPropStart + 2, False), showAbbrevsInsertion), Convert.ToChar(34))
                                End If
                                Console.Write(")")
                            Case 6 'DEXIT
                                Console.Write("TO OBJECT-{0} IF OBJECT-{1} IS OPEN", Helper.GetAdressFromWord(pStoryData, pPropStart), Helper.GetAdressFromWord(pStoryData, pPropStart + 2))
                                If Helper.GetAdressFromWord(pStoryData, pPropStart + 4) > 0 Then
                                    Console.Write(" ELSE {0}{1}{2}", Convert.ToChar(34), ExtractZString(Helper.GetAdressFromPacked(pStoryData, pPropStart + 4, False), showAbbrevsInsertion), Convert.ToChar(34))
                                End If
                                Console.Write(")")
                        End Select
                    Else
                        Select Case pPropSize
                            Case 4 'FEXIT
                                Dim oRoutineData As RoutineData = pRoutineList.Find(Function(c) c.entryPointPacked = Helper.GetAdressFromWord(pStoryData, pPropStart))
                                If oRoutineData IsNot Nothing Then oRoutineData.callsFrom.Add(String.Concat("Called from property #", pPropNum.ToString, " at object #", objectName))
                        End Select
                    End If
                End If
                If Not silent Then Console.WriteLine()
            Else
                If pDictEntries.GetDirection(pPropNum).Count > 0 Then
                    ZilPropDirectionList.Add(pPropNum)
                    DecodePropertyData(silent, pStoryData, pDictEntries, pStringsList, pRoutineList, pPropNum, pPropSize, pPropStart, pPropData, pObjectNum, pObjectName)
                    Exit Sub
                End If
                ' Length = 2, Check is Routine, String or Word (in that order)
                ' Length mod 2 = 0, Check if all ar words
                If pPropSize = 2 Then
                    Dim oStringData As StringData = pStringsList.Find(Function(c) c.entryPointPacked = Helper.GetAdressFromWord(pStoryData, pPropStart))
                    Dim oRoutineData As RoutineData = pRoutineList.Find(Function(c) c.entryPointPacked = Helper.GetAdressFromWord(pStoryData, pPropStart))
                    If oStringData IsNot Nothing Then
                        If Not silent Then Console.WriteLine("(PROP#{0} {1}{2}{3})", pPropNum, Convert.ToChar(34), oStringData.GetText(showAbbrevsInsertion), Convert.ToChar(34))
                        Exit Sub
                    End If
                    If oRoutineData IsNot Nothing Then
                        If Not silent Then Console.WriteLine("(PROP#{0} Routine at 0x{1:X5})", pPropNum, oRoutineData.entryPoint)
                        If silent Then oRoutineData.callsFrom.Add(String.Concat("Called from property #", pPropNum.ToString, " at object #", objectName))
                        Exit Sub
                    End If
                End If
                ' Length mod 2 = 0, Check if all ar words
                If (pPropSize Mod 2) = 0 Then
                    Dim bValidWords As Boolean = True
                    For i As Integer = 0 To pPropSize - 2 Step 2
                        If pDictEntries.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, pPropStart + i)) Is Nothing Then
                            bValidWords = False
                            Exit For
                        End If
                    Next
                    If bValidWords Then
                        If Not silent Then
                            Console.Write("(PROP#{0}", pPropNum)
                            For i As Integer = 0 To pPropSize - 2 Step 2
                                Console.Write(" {0}", pDictEntries.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, pPropStart + i)).dictWord.ToUpper)
                            Next
                            Console.WriteLine(")")
                        End If
                        Exit Sub
                    End If
                End If
                ' Length mod 2 = 0, Check if they are a pattern WORD FUNCTION, then PSEUDO
                If (pPropSize Mod 2) = 0 And pPropSize > 3 Then
                    Dim bValidPseudo As Boolean = True
                    For i As Integer = 0 To pPropSize - 4 Step 4
                        Dim iIdx As Integer = pPropStart + i + 2
                        If pDictEntries.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, pPropStart + i)) Is Nothing Or
                           pRoutineList.Find(Function(c) c.entryPointPacked = Helper.GetAdressFromWord(byteStory, iIdx)) Is Nothing Then
                            bValidPseudo = False
                            Exit For
                        End If
                    Next
                    If bValidPseudo Then
                        If Not silent Then
                            Console.Write("(PSEUDO")
                            For i As Integer = 0 To pPropSize - 4 Step 4
                                Dim iIdx As Integer = pPropStart + i + 2
                                Dim oRoutineData As RoutineData = pRoutineList.Find(Function(c) c.entryPointPacked = Helper.GetAdressFromWord(byteStory, iIdx))
                                If Not silent Then Console.Write(" {0}{1}{2}", Convert.ToChar(34), pDictEntries.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, pPropStart + i)).dictWord.ToUpper, Convert.ToChar(34))
                                If Not silent Then Console.Write(" Routine at 0x{0:X5}", oRoutineData.entryPoint)
                                If oRoutineData IsNot Nothing And silent Then oRoutineData.callsFrom.Add(String.Concat("Called from property #", pPropNum.ToString, " at object #", objectName))
                            Next
                            Console.WriteLine(")")
                        End If
                        Exit Sub
                    End If
                End If
                If Not silent Then Console.WriteLine("PROP#{0} Data = {1}", pPropNum, pPropData)
            End If
        ElseIf {EnumCompilerSource.INFORM5, EnumCompilerSource.INFORM6}.Contains(compilerSource) Then
            ' Length = 2, Check is Routine, String or Word (in that order)
            ' Length mod 2 = 0, Check if all ar words
            If pPropSize = 2 Then
                Dim oStringData As StringData = pStringsList.Find(Function(c) c.entryPointPacked = Helper.GetAdressFromWord(pStoryData, pPropStart))
                Dim oRoutineData As RoutineData = pRoutineList.Find(Function(c) c.entryPointPacked = Helper.GetAdressFromWord(pStoryData, pPropStart))
                If oStringData IsNot Nothing Then
                    If Not silent Then Console.WriteLine("prop#{0} {1}{2}{3}", pPropNum, Convert.ToChar(34), oStringData.GetText(showAbbrevsInsertion), Convert.ToChar(34))
                    Exit Sub
                End If
                If oRoutineData IsNot Nothing Then
                    If Not silent Then Console.WriteLine("prop#{0} Routine at 0x{1:X5}", pPropNum, oRoutineData.entryPoint)
                    If silent Then oRoutineData.callsFrom.Add(String.Concat("Called from property #", pPropNum.ToString, " at object #", objectName))
                    Exit Sub
                End If
            End If
            If (pPropSize Mod 2) = 0 Then
                Dim bValidWords As Boolean = True
                For i As Integer = 0 To pPropSize - 2 Step 2
                    If pDictEntries.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, pPropStart + i)) Is Nothing Then
                        bValidWords = False
                        Exit For
                    End If
                Next
                If bValidWords Then
                    If Not silent Then
                        Console.Write("prop#{0}", pPropNum)
                        For i As Integer = 0 To pPropSize - 2 Step 2
                            Console.Write(" '{0}'", pDictEntries.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, pPropStart + i)).dictWord)
                        Next
                        Console.WriteLine()
                    End If
                    Exit Sub
                End If
                If Not silent Then Console.WriteLine("prop#{0} Data = {1}", pPropNum, pPropData)
            End If
        Else
            If Not silent Then Console.WriteLine("prop#{0} Data = {1}", pPropNum, pPropData)
        End If
    End Sub

    Private Function DecodeGrammarsZilV1(syntaxStartAddress As Integer, dictEntriesList As DictionaryEntries, actionTableStartAddress As Integer, preActionTableStartAddress As Integer, prepositionTableStartAddress As Integer, compactSyntax As Boolean) As String
        ' NORMAL SYNTAX
        ' Each syntax line for a verb consist of 8 bytes:
        '   1 byte	NOBJ   Number of objects for this syntax
        '	1 byte	PREP1  Preposition 1 (preposition number, 0 if none)
        '	1 byte	PREP2  Preposition 2 (preposition number, 0 if none)
        ' 	1 byte	FIND1  1st objects FIND, flagbits (specify required flag for object 1, 0 if none)
        '	1 byte	FIND2  2nd objects FIND, flagbits (specify required flag for object 2, 0 if none)
        '	1 byte	OPTS1  1st objects search options
        '              0   1 Unused
        '	           1   2 HAVE, WINNER must (indirectly) hold the object
        '              2   4 MANY, Multiple objects allowed
        '              3   8 TAKE, Attempt implicit take
        ' 	           4  16 ON-GROUND, search scope for object
        ' 	           5  32 IN-ROOM, search scope for object
        ' 	           6  64 CARRIED, search scope for object
        '              7 128 HELD, search scope for object
        '	1 byte	OPTS2  2nd objects search options
        '              Same bits as in FIND1, but for object 2
        '	1 byte	ACTION Points to row in actions/preactions table
        '
        ' COMPACT SYNTAX
        ' Each syntax line can be either 2, 4 or 7 bytes, depending of the number of objects
        '   byte 1, bits 6-7: NOBJ   Number of objects for this syntax
        '   byte 1, bits 0-5: PREP1  Preposition 1, bits 6-7 are considered set (preposition number, 0 if none)
        '   byte 2          : ACTION Points to row in actions/preactions tabl
        '   byte 3          : FIND1  1st objects FIND, flagbits (specify required flag for object 1, 0 if none)
        '   byte 4          : OPTS1  1st objects search options (same as above)
        '   byte 5          : PREP2  Preposition 2, bits 6-7 are considered set (preposition number, 0 if none)
        '   byte 6          : FIND2  2nd objects FIND, flagbits (specify required flag for object 2, 0 if none)
        '   byte 7          : OPTS2  2nd objects search options

        ' Asume normal syntax 
        Dim nobj As Integer = byteStory(syntaxStartAddress)
        Dim prep(2) As Integer
        prep(1) = byteStory(syntaxStartAddress + 1)
        prep(2) = byteStory(syntaxStartAddress + 2)
        Dim find(2) As Integer
        find(1) = byteStory(syntaxStartAddress + 3)
        find(2) = byteStory(syntaxStartAddress + 4)
        Dim opts(2) As Integer
        opts(1) = byteStory(syntaxStartAddress + 5)
        opts(2) = byteStory(syntaxStartAddress + 6)
        Dim action As Integer = byteStory(syntaxStartAddress + 7)
        Dim prepositionOffset As Integer = 4
        If dictEntriesList.WordSize = 6 Or dictEntriesList.WordSize = 8 Then prepositionOffset = 3     ' COMPACT-VOCABULARY?

        If compactSyntax Then
            nobj = (byteStory(syntaxStartAddress) And &HC0) >> 6
            prep(1) = (byteStory(syntaxStartAddress) And &H3F) Or &HC0
            prep(2) = byteStory(syntaxStartAddress + 4) Or &HC0
            find(1) = byteStory(syntaxStartAddress + 2)
            find(2) = byteStory(syntaxStartAddress + 5)
            opts(1) = byteStory(syntaxStartAddress + 3)
            opts(2) = byteStory(syntaxStartAddress + 6)
            action = byteStory(syntaxStartAddress + 1)
            If prep(1) = &HC0 Then prep(1) = 0
            If prep(2) = &HC0 Then prep(2) = 0
        End If

        Dim actionAddress As Integer = Helper.GetAdressFromWord(byteStory, actionTableStartAddress + action * 2)
        actionAddress = Helper.UnpackAddress(actionAddress, byteStory, True)
        Dim preActionAddress As Integer = Helper.GetAdressFromWord(byteStory, preActionTableStartAddress + action * 2)
        preActionAddress = Helper.UnpackAddress(preActionAddress, byteStory, True)
        Dim sRet As String = "<SYNTAX * "
        For i As Integer = 1 To nobj
            If prep(i) > 0 Then
                Dim prepositionCount As Integer = Helper.GetAdressFromWord(byteStory, prepositionTableStartAddress)
                For prepIdx As Integer = 0 To prepositionCount - 1
                    If byteStory(prepositionTableStartAddress + prepositionOffset + 1 + prepIdx * prepositionOffset) = prep(i) Then
                        sRet = String.Concat(sRet, dictEntriesList.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, prepositionTableStartAddress + 2 + prepIdx * prepositionOffset)).dictWord.ToUpper, " ")
                        Exit For
                    End If
                Next
            End If
            sRet = String.Concat(sRet, "OBJECT ")
            If find(i) > 0 Then sRet = String.Concat(sRet, "(FIND FLAG#", find(i).ToString, ") ")
            If opts(i) > 0 Then
                sRet = String.Concat(sRet, "(")
                If (opts(i) And &H2) = 2 Then sRet = String.Concat(sRet, "HAVE ")
                If (opts(i) And &H4) = 4 Then sRet = String.Concat(sRet, "MANY ")
                If (opts(i) And &H8) = 8 Then sRet = String.Concat(sRet, "TAKE ")
                If (opts(i) And &H10) = 16 Then sRet = String.Concat(sRet, "ON-GROUND ")
                If (opts(i) And &H20) = 32 Then sRet = String.Concat(sRet, "IN-ROOM ")
                If (opts(i) And &H40) = 64 Then sRet = String.Concat(sRet, "CARRIED ")
                If (opts(i) And &H80) = 128 Then sRet = String.Concat(sRet, "HELD ")
                sRet = String.Concat(sRet.TrimEnd, ") ")
            End If
        Next
        sRet = String.Concat(sRet, "= 0x", actionAddress.ToString("X5"), " [Action #", action.ToString(), "]")
        If preActionAddress > 0 Then
            sRet = String.Concat(sRet, " 0x", preActionAddress.ToString("X5"), " [Preaction #", action.ToString(), "]")
        End If
        sRet = String.Concat(sRet, ">")

        Return sRet
    End Function

    Private Function DecodeGrammarsZilV2(syntaxStartAddress As Integer, dictEntriesList As DictionaryEntries, actionTableStartAddress As Integer, preActionTableStartAddress As Integer, numberOfObjects As Integer) As String
        Dim nobj As Integer = numberOfObjects
        Dim prep(2) As Integer
        prep(1) = Helper.GetAdressFromWord(byteStory, syntaxStartAddress + 2)
        prep(2) = Helper.GetAdressFromWord(byteStory, syntaxStartAddress + 6)
        Dim find(2) As Integer
        find(1) = byteStory(syntaxStartAddress + 4)
        find(2) = byteStory(syntaxStartAddress + 8)
        Dim opts(2) As Integer
        opts(1) = byteStory(syntaxStartAddress + 5)
        opts(2) = byteStory(syntaxStartAddress + 9)
        Dim action As Integer = Helper.GetAdressFromWord(byteStory, syntaxStartAddress)

        Dim actionAddress As Integer = Helper.GetAdressFromWord(byteStory, actionTableStartAddress + action * 2)
        actionAddress = Helper.UnpackAddress(actionAddress, byteStory, True)
        Dim preActionAddress As Integer = Helper.GetAdressFromWord(byteStory, preActionTableStartAddress + action * 2)
        preActionAddress = Helper.UnpackAddress(preActionAddress, byteStory, True)
        Dim sRet As String = "<SYNTAX * "
        For i As Integer = 1 To nobj
            If prep(i) > 0 Then sRet = String.Concat(sRet, dictEntriesList.GetEntryAtAddress(prep(i)).dictWord.ToUpper, " ")

            sRet = String.Concat(sRet, "OBJECT ")
            If find(i) > 0 Then sRet = String.Concat(sRet, "(FIND FLAG#", find(i).ToString, ") ")
            If opts(i) > 0 Then
                sRet = String.Concat(sRet, "(")
                If (opts(i) And &HF) = &HF Then
                    sRet = String.Concat(sRet, "IN-ROOM ")
                Else
                    If (opts(i) And &H3) = &H3 Then
                        sRet = String.Concat(sRet, "IN-ROOM ")
                    Else
                        If (opts(i) And &H1) = &H1 Then sRet = String.Concat(sRet, "ON-GROUND ")
                        If (opts(i) And &H2) = &H2 Then sRet = String.Concat(sRet, "OFF-GROUND ")
                    End If
                    If (opts(i) And &HC) = &HC Then
                        sRet = String.Concat(sRet, "CARRIED ")
                    Else
                        If (opts(i) And &H4) = &H4 Then sRet = String.Concat(sRet, "HELD ")
                        If (opts(i) And &H8) = &H8 Then sRet = String.Concat(sRet, "POCKETS ")
                    End If
                End If
                If (opts(i) And &H10) = &H10 Then sRet = String.Concat(sRet, "MANY ")
                If (opts(i) And &H20) = &H20 Then sRet = String.Concat(sRet, "TAKE ")
                If (opts(i) And &HC0) = &HC0 Then
                    sRet = String.Concat(sRet, "ADJACENT ")
                Else
                    If (opts(i) And &H40) = &H40 Then sRet = String.Concat(sRet, "HAVE ")
                    If (opts(i) And &H80) = &H80 Then sRet = String.Concat(sRet, "EVERYWHERE ")
                End If
                sRet = String.Concat(sRet.TrimEnd, ") ")
            End If
        Next
        sRet = String.Concat(sRet, "= 0x", actionAddress.ToString("X5"), " [Action #", action.ToString(), "]")
        If preActionAddress > 0 Then
            sRet = String.Concat(sRet, " 0x", preActionAddress.ToString("X5"), " [Preaction #", action.ToString(), "]")
        End If
        sRet = String.Concat(sRet, ">")

        Return sRet
    End Function

    Private Function DecodeGrammarsInformV1(grammarStartAddress As Integer, dictEntriesList As DictionaryEntries, actionTableStartAddress As Integer, parsingRoutinesStart As Integer, adjectiveTableStart As Integer) As String
        ' See Inform Technical Manual, Chapter 8 (https://inform-fiction.org/source/tm/chapter8.txt)
        '    <parameter count>  <token1> ... <token6>  <action number>
        '    -- byte ---------  --byte--     --byte--  -- byte -------

        'The parameter count Is the number of tokens which are Not prepositions.
        'This Is needed because the run of tokens Is terminated by null bytes (00),
        'which Is ambiguous: "noun" tokens are also encoded as 00.
        Dim parameterCount As Integer = byteStory(grammarStartAddress)
        Dim actionTableIndex As Integer = byteStory(grammarStartAddress + 7)
        Dim actionAddress As Integer = Helper.GetAdressFromWord(byteStory, actionTableStartAddress + actionTableIndex * 2)
        actionAddress = Helper.UnpackAddress(actionAddress, byteStory, True)
        Dim sRet As String = "*"
        For i As Integer = grammarStartAddress + 1 To grammarStartAddress + 6
            ' prepositions/adjectives don't count as parameters in the parameter count
            If byteStory(i) > 192 Then parameterCount += 1
        Next
        For i As Integer = grammarStartAddress + 1 To grammarStartAddress + parameterCount
            Select Case byteStory(i)
                Case 0 : sRet = String.Concat(sRet, " noun")
                Case 1 : sRet = String.Concat(sRet, " held")
                Case 2 : sRet = String.Concat(sRet, " multi")
                Case 3 : sRet = String.Concat(sRet, " multiheld")
                Case 4 : sRet = String.Concat(sRet, " multiexcept")
                Case 5 : sRet = String.Concat(sRet, " multiinside")
                Case 6 : sRet = String.Concat(sRet, " creature")
                Case 7 : sRet = String.Concat(sRet, " special")
                Case 8 : sRet = String.Concat(sRet, " number")
                Case 9 To 15 : sRet = String.Concat(sRet, " illegal token")
                Case 16 To 47 : sRet = String.Concat(sRet, " noun=0x", Helper.UnpackAddress(Helper.GetAdressFromWord(byteStory, parsingRoutinesStart + byteStory(i) - 16), byteStory, True).ToString("X5"), " [parse #", byteStory(i) - 16, "]")
                Case 48 To 79 : sRet = String.Concat(sRet, " 0x", Helper.UnpackAddress(Helper.GetAdressFromWord(byteStory, parsingRoutinesStart + byteStory(i) - 48), byteStory, True).ToString("X5"), " [parse #", byteStory(i) - 48, "]")
                Case 80 To 111 : sRet = String.Concat(sRet, " scope=0x", Helper.UnpackAddress(Helper.GetAdressFromWord(byteStory, parsingRoutinesStart + byteStory(i) - 80), byteStory, True).ToString("X5"), " [parse #", byteStory(i) - 80, "]")
                Case 112 To 127 : sRet = String.Concat(sRet, " illegal token")
                Case 128 To 179 : sRet = String.Concat(sRet, " attribute", byteStory(i) - 128)
                Case 180 To 255
                    Dim adjectiveCount As Integer = Helper.GetAdressFromWord(byteStory, adjectiveTableStart)
                    For adjIdx As Integer = 0 To adjectiveCount - 1
                        If byteStory(adjectiveTableStart + 5 + adjIdx * 4) = byteStory(i) Then
                            Dim dictEntry As DictionaryEntry = dictEntriesList.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, adjectiveTableStart + 2 + adjIdx * 4))
                            Dim prep As String
                            If dictEntry IsNot Nothing Then prep = dictEntry.dictWord Else prep = dictEntriesList.GetPreposition(byteStory(i))(0).dictWord
                            sRet = String.Concat(sRet, " '", prep, "'")
                            Exit For
                        End If
                    Next
            End Select
        Next
        sRet = String.Concat(sRet, " -> 0x", actionAddress.ToString("X5"), " [Action #", actionTableIndex.ToString(), "]")
        Return sRet
    End Function

    Private Function DecodeGrammarsInformV2(grammarStartAddress As Integer, dictEntriesList As DictionaryEntries, actionTableStartAddress As Integer) As String
        ' See Inform Technical Manual, Chapter 8 (https://inform-fiction.org/source/tm/chapter8.txt)
        '    <action number>  <token 1> ... <token N>  <ENDIT>
        '    ----2 bytes----  -3 bytes-     -3 bytes-  -byte--
        '
        '  <action number> bit 0-9  : Points to index in action table
        '                  bit 10   : Reverse nouns if set
        '                  bit 11-15: Unused
        ' <token> byte 1,   token type, bit 0-3: Type
        '                               bit 4-5: Alternatives ("/"), only valid for prepositions
        '                                          00 = no alternatives
        '                                          10 = first alternative
        '                                          11 = member of alternative list
        '                                          01 = last alternative
        '                               bit 6-7: Top bits, redundant
        '         byte 2-3, token data 
        ' <ENDIT>: 0xF
        Dim actionTableIndex As Integer = (Helper.GetAdressFromWord(byteStory, grammarStartAddress) And &H3FF)
        Dim actionAddress As Integer = Helper.GetAdressFromWord(byteStory, actionTableStartAddress + actionTableIndex * 2)
        actionAddress = Helper.UnpackAddress(actionAddress, byteStory, True)
        Dim reverse As Boolean = False
        If (Helper.GetAdressFromWord(byteStory, grammarStartAddress) And &H400) = &H400 Then reverse = True
        Dim index As Integer = 2
        Dim sRet As String = "*"
        If Not byteStory(grammarStartAddress + index) = &HF Then
            Do
                Dim tokenType As Integer = byteStory(grammarStartAddress + index)
                Dim tokenData As Integer = Helper.GetAdressFromWord(byteStory, grammarStartAddress + index + 1)
                If (tokenType And &HF) = &H1 Then
                    ' Elementary type
                    Select Case tokenData
                        Case 0 : sRet = String.Concat(sRet, " noun")
                        Case 1 : sRet = String.Concat(sRet, " held")
                        Case 2 : sRet = String.Concat(sRet, " multi")
                        Case 3 : sRet = String.Concat(sRet, " multiheld")
                        Case 4 : sRet = String.Concat(sRet, " multiexcept")
                        Case 5 : sRet = String.Concat(sRet, " multiinside")
                        Case 6 : sRet = String.Concat(sRet, " creature")
                        Case 7 : sRet = String.Concat(sRet, " special")
                        Case 8 : sRet = String.Concat(sRet, " number")
                        Case 9 : sRet = String.Concat(sRet, " topic")
                        Case Else : sRet = String.Concat(sRet, " illegal")
                    End Select
                ElseIf (tokenType And &HF) = &H2 Then
                    Select Case (tokenType And &H30)
                        Case &H0 : sRet = String.Concat(sRet, " '")
                        Case &H10 : sRet = String.Concat(sRet, "/'")
                        Case &H20 : sRet = String.Concat(sRet, " '")
                        Case &H30 : sRet = String.Concat(sRet, "/'")        ' Undocumented? But only lowest bit seems significant for alternatives 
                    End Select
                    sRet = String.Concat(sRet, dictEntriesList.GetEntryAtAddress(tokenData).dictWord, "'")
                ElseIf (tokenType And &HF) = &H3 Then
                    sRet = String.Concat(sRet, " noun=0x", Helper.UnpackAddress(tokenData, byteStory, True).ToString("X5"))
                ElseIf (tokenType And &HF) = &H4 Then
                    sRet = String.Concat(sRet, " attribute", tokenData.ToString())
                ElseIf (tokenType And &HF) = &H5 Then
                    sRet = String.Concat(sRet, " scope=0x", Helper.UnpackAddress(tokenData, byteStory, True).ToString("X5"))
                ElseIf (tokenType And &HF) = &H6 Then
                    sRet = String.Concat(sRet, " 0x", Helper.UnpackAddress(tokenData, byteStory, True).ToString("X5"))
                End If
                index += 3
            Loop Until byteStory(grammarStartAddress + index) = &HF
        End If
        sRet = String.Concat(sRet, " -> 0x", actionAddress.ToString("X5"), " [Action #", actionTableIndex.ToString(), "]")
        If reverse Then sRet = String.Concat(sRet, " reverse")
        Return sRet
    End Function

    Private Function DecodeGrammarsInformV3(grammarStartAddress As Integer, dictEntriesList As DictionaryEntries, actionTableStartAddress As Integer, parsingRoutinesStart As Integer, adjectiveTableStart As Integer) As String
        '    <action number>  <token 1> ... <token N>
        '    ----2 bytes----  -2 bytes-     -2 bytes-
        '
        '  <action number> bit 0-9  : Points to index in action table
        '                  bit 10   : Reverse nouns if set
        '                  bit 11-15: Number of tokens for this line
        ' <token> byte 1,   token type, bit 0-3: Type
        '                               bit 4-5: Alternatives ("/"), only valid for prepositions
        '                                          00 = no alternatives
        '                                          10 = first alternative
        '                                          11 = member of alternative list
        '                                          01 = last alternative
        '                               bit 6-7: Top bits, redundant
        '         byte 2,   token data 
        Dim actionTableIndex As Integer = (Helper.GetAdressFromWord(byteStory, grammarStartAddress) And &H3FF)
        Dim tokens As Integer = (Helper.GetAdressFromWord(byteStory, grammarStartAddress) And &HF800) >> 11
        Dim actionAddress As Integer = Helper.GetAdressFromWord(byteStory, actionTableStartAddress + actionTableIndex * 2)
        actionAddress = Helper.UnpackAddress(actionAddress, byteStory, True)
        Dim reverse As Boolean = False
        If (Helper.GetAdressFromWord(byteStory, grammarStartAddress) And &H400) = &H400 Then reverse = True
        Dim index As Integer = 2
        Dim sRet As String = "*"
        For i As Integer = 0 To tokens - 1
            Dim tokenType As Integer = byteStory(grammarStartAddress + index)
            Dim tokenData As Integer = byteStory(grammarStartAddress + index + 1)
            If (tokenType And &HF) = &H1 Then
                ' Elementary type
                Select Case tokenData
                    Case 0 : sRet = String.Concat(sRet, " noun")
                    Case 1 : sRet = String.Concat(sRet, " held")
                    Case 2 : sRet = String.Concat(sRet, " multi")
                    Case 3 : sRet = String.Concat(sRet, " multiheld")
                    Case 4 : sRet = String.Concat(sRet, " multiexcept")
                    Case 5 : sRet = String.Concat(sRet, " multiinside")
                    Case 6 : sRet = String.Concat(sRet, " creature")
                    Case 7 : sRet = String.Concat(sRet, " special")
                    Case 8 : sRet = String.Concat(sRet, " number")
                    Case 9 : sRet = String.Concat(sRet, " topic")
                    Case Else : sRet = String.Concat(sRet, " illegal")
                End Select
            ElseIf (tokenType And &HF) = &H2 Then
                Select Case (tokenType And &H30)
                    Case &H0 : sRet = String.Concat(sRet, " '")
                    Case &H10 : sRet = String.Concat(sRet, "/'")
                    Case &H20 : sRet = String.Concat(sRet, " '")
                    Case &H30 : sRet = String.Concat(sRet, "/'")        ' Undocumented? But only lowest bit seems significant for alternatives 
                End Select
                sRet = String.Concat(sRet, dictEntriesList.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, adjectiveTableStart + tokenData * 2 + 2)).dictWord, "'")
            ElseIf (tokenType And &HF) = &H3 Then
                sRet = String.Concat(sRet, " noun=0x", String.Concat(sRet, " noun=0x", Helper.UnpackAddress(Helper.GetAdressFromWord(byteStory, parsingRoutinesStart + tokenData * 2), byteStory, True).ToString("X5"), " [parse #", tokenData, "]"))
            ElseIf (tokenType And &HF) = &H4 Then
                sRet = String.Concat(sRet, " attribute", tokenData.ToString())
            ElseIf (tokenType And &HF) = &H5 Then
                sRet = String.Concat(sRet, " scope=0x", String.Concat(sRet, " noun=0x", Helper.UnpackAddress(Helper.GetAdressFromWord(byteStory, parsingRoutinesStart + tokenData * 2), byteStory, True).ToString("X5"), " [parse #", tokenData, "]"))
            ElseIf (tokenType And &HF) = &H6 Then
                sRet = String.Concat(sRet, " 0x", String.Concat(sRet, " noun=0x", Helper.UnpackAddress(Helper.GetAdressFromWord(byteStory, parsingRoutinesStart + tokenData * 2), byteStory, True).ToString("X5"), " [parse #", tokenData, "]"))
            End If
            index += 2
        Next
        sRet = String.Concat(sRet, " -> 0x", actionAddress.ToString("X5"), " [Action #", actionTableIndex.ToString(), "]")
        If reverse Then sRet = String.Concat(sRet, " reverse")
        Return sRet
    End Function

End Module
