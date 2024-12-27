'MIT License

'Copyright(c) 2021-2024 Henrik Åsman

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

Public Enum MemoryMapType
    MM_HEADER_TABLE
    MM_HEADER_EXT_TABLE
    MM_ABBREVIATION_STRINGS
    MM_ABBREVIATION_TABLE
    MM_PROPERTY_DEFAULTS_TABLE
    MM_OBJECT_TREE_TABLE
    MM_OBJECT_PROPERTIES_TABLES
    MM_GLOBAL_VARIABLES
    MM_TERMINATING_CHARS_TABLE
    MM_GRAMMAR_TABLE
    MM_GRAMMAR_TABLE_DATA
    MM_ACTION_TABLE
    MM_PREPOSITION_TABLE
    MM_PREPOSITION_TABLE_COUNT
    MM_PREACTION_TABLE
    MM_PREACTION_PARSING_TABLE
    MM_DICTIONARY
    MM_ZCODE
    MM_STATIC_STRINGS
    MM_CHRSET
    MM_IFID
    MM_UNIDENTIFIED_DATA
    MM_PADDING
    MM_WORD_FLAGS_TABLE
    MM_UNICODE_TABLE
    MM_MAIN_HEAP
    MM_AUX_LT_HEAP
    MM_PREDICATE_DATA
    MM_SCRATCH_AREA
End Enum

Public Class MemoryMapEntry
    Public Sub New(pName As String, pAddressStart As Integer, pAddressEnd As Integer, pType As MemoryMapType)
        name = pName
        addressStart = pAddressStart
        addressEnd = pAddressEnd
        type = pType
    End Sub

    Public name As String = ""
    Public addressStart As Integer = 0
    Public addressEnd As Integer = 0
    Public type As MemoryMapType = MemoryMapType.MM_UNIDENTIFIED_DATA
    Public startOfDynamic As Boolean = False
    Public startOfStatic As Boolean = False
    Public startOfHigh As Boolean = False


    Public ReadOnly Property SizeString As String
        Get
            Dim size As Integer = addressEnd - addressStart + 1
            If size = 1 Then Return "1 byte"
            Return size.ToString("#,##0 bytes")
        End Get
    End Property

    Public Sub PrintMemoryLabel()
        If startOfDynamic Then
            Console.WriteLine()
            Console.WriteLine("*****************************************************")
            Console.WriteLine("********** START OF DYNAMIC MEMORY 0x{0:X5} **********", addressStart)
            Console.WriteLine("*****************************************************")
            Console.WriteLine()
        End If
        If startOfStatic Then
            Console.WriteLine()
            Console.WriteLine("****************************************************")
            Console.WriteLine("********** START OF STATIC MEMORY 0x{0:X5} **********", addressStart)
            Console.WriteLine("****************************************************")
            Console.WriteLine()
        End If
        If startOfHigh Then
            Console.WriteLine()
            Console.WriteLine("**************************************************")
            Console.WriteLine("********** START OF HIGH MEMORY 0x{0:X5} **********", addressStart)
            Console.WriteLine("**************************************************")
            Console.WriteLine()
        End If
    End Sub
End Class
