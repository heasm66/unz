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

Imports System.Drawing
Imports System.Net
Imports System.Net.Mime.MediaTypeNames
Imports System.Runtime.InteropServices
Imports System.Runtime.Intrinsics.Arm

Public Class Decode
    Public startAddress As Integer = 0
    Public endAddress As Integer = 0
    Public localsCount As Integer = 0
    Public highest_routine As Integer = 0
    Public lowest_routine As Integer = Integer.MaxValue
    Public lowest_string As Integer = Integer.MaxValue
    Public highest_global As Integer = -1
    Public callsTo As New List(Of CallsFromTo)
    Private ZVersion As Integer = 0
    Private PC As Integer = 0
    Private byteGame() As Byte
    Private high_pc As Integer
    Private sAbbreviations() As String
    Private bSilent As Boolean = False
    Private bShowZCodeVerbose As Boolean = False
    Private syntax As Integer = 0
    Private validStringList As List(Of StringData) = Nothing
    Private validRoutineList As List(Of RoutineData) = Nothing
    Private DictEntriesList As New DictionaryEntries
    Private alphabet(2) As String
    Private propertyMax As Integer = 63
    Private propertyMin As Integer = 0
    Private showAbbrevsInsertion As Boolean = True
    Private inlineStrings As List(Of InlineString)
    Public arraysStart As New HashSet(Of Integer)
    Public usedGlobals As New HashSet(Of Integer)

    Public Enum EnumOpcodeClass
        EXTENDED_OPERAND
        TWO_OPERAND
        ONE_OPERAND
        ZERO_OPERAND
        VARIABLE_OPERAND
    End Enum

    Public Enum EnumOperand
        P_NIL
        P_ANYTHING
        P_VAR
        P_NUMBER
        P_LOW_ADDR
        P_ROUTINE
        P_OBJECT
        P_STATIC
        P_LABEL
        P_PCHAR
        P_VATTR
        P_PATTR
        P_INDIRECT
        P_PROPNUM
        P_ATTRNUM
    End Enum

    Public Enum EnumExtra
        E_NONE
        E_TEXT
        E_STORE
        E_BRANCH
        E_BOTH
    End Enum

    Public Enum EnumType
        T_PLAIN
        T_CALL
        T_RETURN
        T_ILLEGAL
    End Enum

    Public Enum EnumStatus
        END_OF_CODE
        END_OF_ROUTINE
        END_OF_INSTRUCTION
        BAD_ENTRY
        BAD_OPCODE
    End Enum

    Public Enum EnumAddressMode
        NONE
        LONG_IMMEDIATE
        IMMEDIATE
        VARIABLE
    End Enum

    Private Class DecodeResult
        Public status As EnumStatus
        Public nextPC As Integer
    End Class

    Private Class Opcode
        Public Address As Integer
        Public Code As Integer
        Public ExtModeByte As Integer
        Public OpcodeClass As EnumOpcodeClass
        Public OperandType(7) As EnumOperand
        Public OperandLen(7) As Integer
        Public OperandVal(7) As Integer
        Public OperandAddrMode(7) As EnumAddressMode
        Public OperandStringPseudo(7) As String
        Public Extra As EnumExtra
        Public StoreVal As Integer = 0
        Public StoreStringPseudo As String
        Public BranchTest As Boolean = True             ' Branch if true/false
        Public BranchAddr As Integer = 0                ' Absolut address of branch, 0=false, 1=true
        Public Type As EnumType
        Public Text As String = ""
        Public OperandText As String = ""
        Public OpcodeText As String = ""
        Public OpcodeBytes(20) As Byte

        Public Sub PrintVerbose()
            Dim currentByte As Integer = 0
            Dim byteValue As Integer
            Dim wordValue As Integer
            Dim numberOfOperands As Integer = 0

            Console.WriteLine("  *--------------------------------------------------")

            ' Opcode
            Dim opcodeAsBinary As String = Convert.ToString(Code Or 256, 2).Substring(1)
            Console.WriteLine("  |Opcode:")
            Select Case OpcodeClass
                Case EnumOpcodeClass.ZERO_OPERAND       ' Bit 7-4 = 1011
                    Console.WriteLine("  | Byte {0,2:##}    0OP:{1}", currentByte + 1, Code)
                    Console.WriteLine("  |  {0}  0x{1:X2} {2}", opcodeAsBinary, Code, Code)
                    Console.WriteLine("  |  10        short-form (long-form in ZAP)")
                    Console.WriteLine("  |    11      no operands")
                    Console.WriteLine("  |      {0}  {1}", opcodeAsBinary.Substring(4, 4), OpcodeText)
                Case EnumOpcodeClass.ONE_OPERAND        ' Bit 7-6 = 10
                    numberOfOperands = 1
                    Console.WriteLine("  | Byte {0,2:##}    1OP:{1}", currentByte + 1, Code And &H9F)
                    Console.WriteLine("  |  {0}  0x{1:X2} {2}", opcodeAsBinary, Code, Code)
                    Console.WriteLine("  |  10        short-form (long-form in ZAP)")
                    Console.WriteLine("  |    {0}      {1}", opcodeAsBinary.Substring(2, 2), OperandDescription((Code >> 4) And &H3))
                    'End Select
                    Console.WriteLine("  |      {0}  {1}", opcodeAsBinary.Substring(4, 4), OpcodeText)
                Case EnumOpcodeClass.TWO_OPERAND        ' Bit 7   = 0
                    numberOfOperands = 2
                    Console.WriteLine("  | Byte {0,2:##}    2OP:{1}", currentByte + 1, Code And &H1F)
                    Console.WriteLine("  |  {0}  0x{1:X2} {2}", opcodeAsBinary, Code, Code)
                    Console.WriteLine("  |  0         long-form (short-form in ZAP)")
                    Select Case Code And &H60
                        Case &H0
                            Console.WriteLine("  |   0        constant (immediate)")
                            Console.WriteLine("  |    0       constant (immediate)")
                        Case &H20
                            Console.WriteLine("  |   0        constant (immediate)")
                            Console.WriteLine("  |    1       variable")
                        Case &H40
                            Console.WriteLine("  |   1        variable")
                            Console.WriteLine("  |    0       constant (immediate)")
                        Case &H60
                            Console.WriteLine("  |   1        variable")
                            Console.WriteLine("  |    1       variable")
                    End Select
                    Console.WriteLine("  |     {0}  {1}", opcodeAsBinary.Substring(3, 5), OpcodeText)
                Case EnumOpcodeClass.VARIABLE_OPERAND   ' Bit 7-6 = 11
                    numberOfOperands = 4
                    Console.WriteLine("  | Byte {0,2:##}    VAR:{1} (EXT:{2})", currentByte + 1, Code, Code)
                    Console.WriteLine("  |  {0}  0x{1:X2} {2}", opcodeAsBinary, Code, Code)
                    Console.WriteLine("  |  11        variable-operand (extended)")
                    If Code < 224 Then
                        Console.WriteLine("  |    0       VAR-variant of 2OP:{0}", Code - 192)
                    Else
                        Console.WriteLine("  |    1       VAR")
                    End If
                    Console.WriteLine("  |     {0}  {1}", opcodeAsBinary.Substring(3, 5), OpcodeText)
                Case EnumOpcodeClass.EXTENDED_OPERAND   ' 0OP opcode 190
                    numberOfOperands = 4
                    Console.WriteLine("  | Byte {0,2:##}    0OP:190 extended-form (EXTOP)")
                    Console.WriteLine("  |  10111110  0xBE 190")
                    currentByte += 1
                    Console.WriteLine("  | Byte {0,2:##}    EXT:{1} (EXT:{2})", currentByte + 1, Code, Code + 256)
                    Console.WriteLine("  |  {0}  0x{1:X2} {2}", opcodeAsBinary, Code, Code)
            End Select
            currentByte += 1

            ' Operand types
            byteValue = OpcodeBytes(currentByte)
            Dim operandTypesAsBinary As String = Convert.ToString(byteValue Or 256, 2).Substring(1)
            If OpcodeClass = EnumOpcodeClass.VARIABLE_OPERAND Or OpcodeClass = EnumOpcodeClass.EXTENDED_OPERAND Then
                If byteValue = &HFF Then numberOfOperands = 0
                Console.WriteLine("  |")
                Console.WriteLine("  |Operand types:")
                Console.WriteLine("  | Byte {0,2:##}    Operand types 1-4", currentByte + 1)
                Console.WriteLine("  |  {0}  0x{1:X2} {2}", operandTypesAsBinary, byteValue, byteValue)
                Console.WriteLine("  |  {0}        {1}", operandTypesAsBinary.Substring(0, 2), OperandDescription((byteValue >> 6) And &H3))
                Console.WriteLine("  |    {0}      {1}", operandTypesAsBinary.Substring(2, 2), OperandDescription((byteValue >> 4) And &H3))
                Console.WriteLine("  |      {0}    {1}", operandTypesAsBinary.Substring(4, 2), OperandDescription((byteValue >> 2) And &H3))
                Console.WriteLine("  |        {0}  {1}", operandTypesAsBinary.Substring(6, 2), OperandDescription(byteValue And &H3))
                currentByte += 1
                If (Code And &H3F) = &H2C Or (Code And &H3F) = &H3A Then
                    ' These two opcodes are special and have an extra byte for operand types
                    If byteValue = &HFF Then numberOfOperands = 0 Else numberOfOperands = 8
                    byteValue = OpcodeBytes(currentByte)
                    operandTypesAsBinary = Convert.ToString(byteValue Or 256, 2).Substring(1)
                    Console.WriteLine("  | Byte {0,2:##}    Operand types 5-8", currentByte + 1)
                    Console.WriteLine("  |  {0}  0x{1:X2} {2}", operandTypesAsBinary, byteValue, byteValue)
                    Console.WriteLine("  |  {0}        {1}", operandTypesAsBinary.Substring(0, 2), OperandDescription((byteValue >> 6) And &H3))
                    Console.WriteLine("  |    {0}      {1}", operandTypesAsBinary.Substring(2, 2), OperandDescription((byteValue >> 4) And &H3))
                    Console.WriteLine("  |      {0}    {1}", operandTypesAsBinary.Substring(4, 2), OperandDescription((byteValue >> 2) And &H3))
                    Console.WriteLine("  |        {0}  {1}", operandTypesAsBinary.Substring(6, 2), OperandDescription(byteValue And &H3))
                    currentByte += 1
                End If
            End If

            ' Operands
            If numberOfOperands > 0 Then
                Console.WriteLine("  |")
                Console.WriteLine("  |Operands:")
                For i As Integer = 0 To numberOfOperands - 1
                    byteValue = OpcodeBytes(currentByte)
                    Select Case OperandAddrMode(i)
                        Case EnumAddressMode.IMMEDIATE
                            If OperandType(i) = EnumOperand.P_VAR Then
                                Console.WriteLine("  | Byte {0,2:##}    0x{1:X2} {2} ({3})", currentByte + 1, byteValue, byteValue, VariableDescription(byteValue))
                            ElseIf (byteValue And &H8000) > 0 Then
                                Console.WriteLine("  | Byte {0,2:##}    0x{1:X2} {2} / {3}", currentByte + 1, byteValue, byteValue, -1 * (&H100 - byteValue))
                            Else
                                Console.WriteLine("  | Byte {0,2:##}    0x{1:X2} {2}", currentByte + 1, byteValue, byteValue)
                            End If
                            currentByte += 1
                        Case EnumAddressMode.VARIABLE
                            Dim varText As String = VariableDescription(byteValue)
                            If OperandType(i) = EnumOperand.P_INDIRECT Then varText = "[" & varText & "]"
                            Console.WriteLine("  | Byte {0,2:##}    0x{1:X2} {2} ({3})", currentByte + 1, byteValue, byteValue, varText)
                            currentByte += 1
                        Case EnumAddressMode.LONG_IMMEDIATE
                            wordValue = OpcodeBytes(currentByte) * 256 + OpcodeBytes(currentByte + 1)
                            If OperandType(i) = EnumOperand.P_LABEL Then
                                ' JUMP, always two-complemented word
                                If (wordValue And &H8000) > 0 Then
                                    Console.WriteLine("  | Byte {0,2:##}-{1,2:##} 0x{2:X4} {3}  [{4} (0x{5:X4}) - {6} - 2 = {7} (0x{8:X4})]", currentByte + 1,
                                                                                                                                      currentByte + 2,
                                                                                                                                      wordValue,
                                                                                                                                      -1 * (&H10000 - wordValue),
                                                                                                                                      Address + currentByte + 2,
                                                                                                                                      Address + currentByte + 2,
                                                                                                                                      &H10000 - wordValue,
                                                                                                                                      Address + currentByte + 2 - (&H10000 - wordValue) - 2,
                                                                                                                                     Address + currentByte + 2 - (&H10000 - wordValue) - 2)
                                Else
                                    Console.WriteLine("  | Byte {0,2:##}-{1,2:##} 0x{2:X4} {3}  [{4} (0x{5:X4}) + {6} - 2 = {7} (0x{8:X4})]", currentByte + 1,
                                                                                                                                      currentByte + 2,
                                                                                                                                      wordValue,
                                                                                                                                      wordValue,
                                                                                                                                      Address + currentByte + 2,
                                                                                                                                      Address + currentByte + 2,
                                                                                                                                      wordValue,
                                                                                                                                      Address + currentByte + 2 + wordValue - 2,
                                                                                                                                      Address + currentByte + 2 + wordValue - 2)
                                End If
                            Else
                                If (wordValue And &H8000) > 0 Then
                                    Console.WriteLine("  | Byte {0,2:##}-{1,2:##} 0x{2:X4} {3} / {4}", currentByte + 1, currentByte + 2, wordValue, wordValue, -1 * (&H10000 - wordValue))
                                Else
                                    Console.WriteLine("  | Byte {0,2:##}-{1,2:##} 0x{2:X4} {3}", currentByte + 1, currentByte + 2, wordValue, wordValue)
                                End If
                            End If
                            currentByte += 2
                    End Select

                Next
            End If

            ' Store
            If Extra = EnumExtra.E_STORE Or Extra = EnumExtra.E_BOTH Then
                byteValue = OpcodeBytes(currentByte)
                Console.WriteLine("  |")
                Console.WriteLine("  |Store:")
                Console.WriteLine("  | Byte {0,2:##}    0x{1:X2} {2} ({3})", currentByte + 1, byteValue, byteValue, VariableDescription(byteValue))
                currentByte += 1
            End If

            ' Branch
            If Extra = EnumExtra.E_BRANCH Or Extra = EnumExtra.E_BOTH Then
                Console.WriteLine("  |")
                Console.WriteLine("  |Branch:")
                byteValue = OpcodeBytes(currentByte)
                wordValue = OpcodeBytes(currentByte) * 256 + OpcodeBytes(currentByte + 1)
                Dim predicateAsBinary As String = Convert.ToString(byteValue Or 256, 2).Substring(1)
                If (byteValue And &H40) = &H40 Then
                    ' 6-bit branch, always forward
                    Console.WriteLine("  | Byte {0,2:##}    0x{1:X2} {2}", currentByte + 1, byteValue, byteValue)
                    Console.WriteLine("  |  {0}  ", operandTypesAsBinary)
                    If (byteValue And &H80) = &H80 Then
                        Console.WriteLine("  |  1         branch if true")
                    Else
                        Console.WriteLine("  |  0         branch if false")
                    End If
                    Console.WriteLine("  |   1        6-bit branch, always forward, except when 0 (rfalse) or 1 (rtrue)")
                    If (byteValue And &H3F) = 0 Then
                        Console.WriteLine("  |    {0}  0x{1:X2} {2}  (rfalse)", predicateAsBinary.Substring(2, 6), byteValue And &H3F, byteValue And &H3F)
                    ElseIf (byteValue And &H3F) = 1 Then
                        Console.WriteLine("  |    {0}  0x{1:X2} {2}  (rtrue)", predicateAsBinary.Substring(2, 6), byteValue And &H3F, byteValue And &H3F)
                    Else
                        Console.WriteLine("  |    {0}  0x{1:X2} {2}  [{3} (0x{4:X4}) + {5} - 2 = {6} (0x{7:X4})]", predicateAsBinary.Substring(2, 6),
                                                                                                               byteValue And &H3F,
                                                                                                               byteValue And &H3F,
                                                                                                               Address + currentByte + 1,
                                                                                                               Address + currentByte + 1,
                                                                                                               byteValue And &H3F,
                                                                                                               Address + currentByte + 1 + (byteValue And &H3F) - 2,
                                                                                                               Address + currentByte + 1 + (byteValue And &H3F) - 2)
                    End If
                    currentByte += 1
                Else
                    ' 14-bit branch, two-complemented
                    Console.WriteLine("  | Byte {0,2:##}    0x{1:X2} {2}", currentByte + 1, byteValue, byteValue)
                    Console.WriteLine("  |  {0}  ", operandTypesAsBinary)
                    If (byteValue And &H80) = &H80 Then
                        Console.WriteLine("  |  1         branch if true")
                    Else
                        Console.WriteLine("  |  0         branch if false")
                    End If
                    Console.WriteLine("  |   0        14-bit branch, two-complemented")
                    Console.WriteLine("  |    {0}  high 6 bits", predicateAsBinary.Substring(2, 6))
                    currentByte += 1
                    byteValue = OpcodeBytes(currentByte)
                    predicateAsBinary = Convert.ToString(byteValue Or 256, 2).Substring(1)
                    Console.WriteLine("  | Byte {0,2:##}    0x{1:X2} {2}, low 8 bits", currentByte + 1, byteValue, byteValue)
                    If (wordValue And &H2000) = &H2000 Then
                        Dim value As Integer = &H4000 - (wordValue And &H3FFF)
                        Console.WriteLine("  |  {0}  0x{1:X4} {2}  [{3} (0x{4:X4}) - {5} - 2 = {6} (0x{7:X4})]", predicateAsBinary,
                                                                                                                 -1 * value,
                                                                                                                 -1 * value,
                                                                                                                 Address + currentByte + 1,
                                                                                                                 Address + currentByte + 1,
                                                                                                                 value,
                                                                                                                 Address + currentByte + 1 - value - 2,
                                                                                                                 Address + currentByte + 1 - value - 2)

                    Else
                        Console.WriteLine("  |  {0}  0x{1:X4} {2}  [{3} (0x{4:X4}) + {5} - 2 = {6} (0x{7:X4})]", predicateAsBinary,
                                                                                                                 wordValue And &H3FFF,
                                                                                                                 wordValue And &H3FFF,
                                                                                                                 Address + currentByte + 1,
                                                                                                                 Address + currentByte + 1,
                                                                                                                 wordValue And &H3FFF,
                                                                                                                 Address + currentByte + 1 + (wordValue And &H3FFF) - 2,
                                                                                                                 Address + currentByte + 1 + (wordValue And &H3FFF) - 2)
                    End If
                    currentByte += 1
                End If
            End If

            ' Text
            If Extra = EnumExtra.E_TEXT Then
                Console.WriteLine("  |")
                Console.WriteLine("  |Text:")
                Console.WriteLine("  | Byte {0,2:##}-   string encoded in ZSCII", currentByte + 1)
            End If

            ' Pseudo
            Dim pseudoCodeString As String = PseudoCode()
            Console.WriteLine("  |")
            Console.WriteLine("  |Pseudo-code:")
            Console.WriteLine("  |  {0}", pseudoCodeString)

            Console.WriteLine("  *--------------------------------------------------")
        End Sub

        Public Function PseudoCode() As String
            Dim pseudoCodeString As String = "not implemented"
            Dim stackPeek As Boolean = False
            If Extra = EnumExtra.E_BRANCH Or Extra = EnumExtra.E_BOTH Then
                'check_arg_count, ASSIGNED?	if (check_arg_count > arg1) jump xxxx/rfalse/rtrue
                'dec_chk, DLESS?			var = var - 1; if (var </>= int) jump xxxx/rfalse/rtrue
                'get_child, FIRST?			VAL = child(obj); if (VAL ==/~= nothing) jump xxxx/rfalse/rtrue
                'get_sibling, NEXT?			VAL = sibling(obj); if (VAL ==/~= nothing) jump xxxx/rfalse/rtrue
                'inc_chk", "IGRTR?"			var = var + 1; if (var >/<= int) jump xxxx/rfalse/rtrue
                'je, EQUAL?					if (arg1 ==/~= arg2 [or arg3]...) jump xxxx/rfalse/rtrue
                'jg, GRTR?					if (arg1 >/<= arg2) jump xxxx/rfalse/rtrue
                'jin, IN?					if (arg1 in/notin arg2) jump xxxx/rfalse/rtrue
                'jl, LESS?				    if (arg1 </>= arg2) jump xxxx/rfalse/rtrue
                'jz, ZERO?					if (arg ==/~= 0) jump xxxx/rfalse/rtrue
                'make_menu, MENU			if (make_menu(arg1, arg2) == true/false) jump xxxx/rfalse/rtrue
                'picture_data, PICINF		if (picture_data(arg1, arg2) == true/false) jump xxxx/rfalse/rtrue
                'piracy, ORIGINAL?			if (orginal == true/false) jump xxxx/rfalse/rtrue
                'push_stack, XPUSH			if (push_stack(arg1) == true/false) jump xxxx/rfalse/rtrue
                'restore, RESTORE			if (restore == true/false) jump xxxx/rfalse/rtrue
                'save, SAVE					if (save == true/false) jump xxxx/rfalse/rtrue
                'scan_table, INTBL?			VAL = scan_table(arg1, arg2, arg3[, arg4]); if (VAL ==/~= nothing) jump xxxx/rfalse/rtrue
                'test, BTST			        if ((arg1 & arg2) ==/~= arg2) jump xxxx/rfalse/rtrue
                'test_attr, FSET?			if (arg1 has/hasnt arg2) jump xxxx/rfalse/rtrue 
                'verify, VERIFY				if (verify == true/false) jump xxxx/rfalse/rtrue
                Dim pseudoComp As String
                Select Case OpcodeText
                    Case "@check_arg_count / ASSIGNED?"
                        If BranchTest Then pseudoComp = "==" Else pseudoComp = "~="
                        pseudoCodeString = String.Concat("if (check_arg_count() ", pseudoComp, " ", OperandStringPseudo(0), ") ", PseudoBranchText, ";")
                    Case "@dec_chk / DLESS?"
                        If BranchTest Then pseudoComp = "<" Else pseudoComp = ">="
                        pseudoCodeString = String.Concat(OperandStringPseudo(0), " = ", OperandStringPseudo(0), " - 1; if (", OperandStringPseudo(0), " ", pseudoComp, " ", OperandStringPseudo(1), ") ", PseudoBranchText, ";")
                    Case "@get_child / FIRST?"
                        stackPeek = True
                        If BranchTest Then pseudoComp = "==" Else pseudoComp = "~="
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = child(", OperandStringPseudo(0), ")#; if (", StoreStringPseudo, " ", pseudoComp, " nothing) ", PseudoBranchText, ";")
                    Case "@get_sibling / NEXT?"
                        stackPeek = True
                        If BranchTest Then pseudoComp = "==" Else pseudoComp = "~="
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = sibling(", OperandStringPseudo(0), ")#; if (", StoreStringPseudo, " ", pseudoComp, " nothing) ", PseudoBranchText, ";")
                    Case "@inc_chk / IGRTR?"
                        stackPeek = True
                        If BranchTest Then pseudoComp = "<" Else pseudoComp = ">="
                        pseudoCodeString = String.Concat(OperandStringPseudo(0), " = ", OperandStringPseudo(0), " + 1; if (", OperandStringPseudo(0), " ", pseudoComp, " ", OperandStringPseudo(1), ") ", PseudoBranchText, ";")
                    Case "@je / EQUAL?"
                        If BranchTest Then pseudoComp = "==" Else pseudoComp = "~="
                        pseudoCodeString = String.Concat("if (", OperandStringPseudo(0), " ", pseudoComp, " ", PseudoConcatOperands(1, 3, " or "), ") ", PseudoBranchText, ";")
                    Case "@jg / GRTR?"
                        If BranchTest Then pseudoComp = ">" Else pseudoComp = "<="
                        pseudoCodeString = String.Concat("if (", OperandStringPseudo(0), " ", pseudoComp, " ", OperandStringPseudo(1), ") ", PseudoBranchText, ";")
                    Case "@jin / IN?"
                        If BranchTest Then pseudoComp = "in" Else pseudoComp = "notin"
                        pseudoCodeString = String.Concat("if (", OperandStringPseudo(0), " ", pseudoComp, " ", OperandStringPseudo(1), ") ", PseudoBranchText, ";")
                    Case "@jl / LESS?"
                        If BranchTest Then pseudoComp = "<" Else pseudoComp = ">="
                        pseudoCodeString = String.Concat("if (", OperandStringPseudo(0), " ", pseudoComp, " ", OperandStringPseudo(1), ") ", PseudoBranchText, ";")
                    Case "@jz / ZERO?"
                        If BranchTest Then pseudoComp = "==" Else pseudoComp = "~="
                        pseudoCodeString = String.Concat("if (", OperandStringPseudo(0), " ", pseudoComp, " 0) ", PseudoBranchText, ";")
                    Case "@make_menu / MENU"
                        If BranchTest Then pseudoComp = "true" Else pseudoComp = "false"
                        pseudoCodeString = String.Concat("if (make_menu(", OperandStringPseudo(0), ", ", OperandStringPseudo(1), ") ", pseudoComp, ") ", PseudoBranchText, ";")
                    Case "@picture_data / PICINF"
                        If BranchTest Then pseudoComp = "true" Else pseudoComp = "false"
                        pseudoCodeString = String.Concat("if (picture_data(", OperandStringPseudo(0), ", ", OperandStringPseudo(1), ") ", pseudoComp, ") ", PseudoBranchText, ";")
                    Case "@piracy / ORIGINAL?"
                        If BranchTest Then pseudoComp = "true" Else pseudoComp = "false"
                        pseudoCodeString = String.Concat("if (original() == ", pseudoComp, ") ", PseudoBranchText, ";")
                    Case "@push_stack / XPUSH"
                        If BranchTest Then pseudoComp = "true" Else pseudoComp = "false"
                        pseudoCodeString = String.Concat("if (push_stack(", OperandStringPseudo(0), ") == ", pseudoComp, ") ", PseudoBranchText, ";")
                    Case "@restore / RESTORE"
                        If BranchTest Then pseudoComp = "true" Else pseudoComp = "false"
                        pseudoCodeString = String.Concat("if (restore() == ", pseudoComp, ") ", PseudoBranchText, ";")
                    Case "@save / SAVE"
                        If BranchTest Then pseudoComp = "true" Else pseudoComp = "false"
                        pseudoCodeString = String.Concat("if (save() == ", pseudoComp, ") ", PseudoBranchText, ";")
                    Case "@scan_table / INTBL?"
                        stackPeek = True
                        If BranchTest Then pseudoComp = "==" Else pseudoComp = "~="
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = scan_table(", PseudoConcatOperands(1), ")#; if (", StoreStringPseudo, " ", pseudoComp, " nothing) ", PseudoBranchText, ";")
                    Case "@test / BTST"
                        If BranchTest Then pseudoComp = "==" Else pseudoComp = "~="
                        pseudoCodeString = String.Concat("if ((", OperandStringPseudo(0), " & ", OperandStringPseudo(1), ") ", pseudoComp, " ", OperandStringPseudo(1), ") ", PseudoBranchText, ";")
                    Case "@test_attr / FSET?"
                        If BranchTest Then pseudoComp = "has" Else pseudoComp = "hasnt"
                        pseudoCodeString = String.Concat("if (", OperandStringPseudo(0), " ", pseudoComp, " ", OperandStringPseudo(1), ") ", PseudoBranchText, ";")
                    Case "@verify / VERIFY"
                        If BranchTest Then pseudoComp = "true" Else pseudoComp = "false"
                        pseudoCodeString = String.Concat("if (verify() == ", pseudoComp, ") ", PseudoBranchText, ";")
                End Select
            ElseIf Extra = EnumExtra.E_STORE Then
                'add, ADD
                'And, BAND
                'art_shift, ASHIFT
                'buffer_screen, *BUFFER_SCREEN*
                'Call, CALL
                'call_1s, CALL1
                'call_2s, CALL2
                'call_vs, CALL
                'call_vs2, XCALL
                'Catch, CATCH
                'check_unicode, CHECKU
                'div, DIV
                'get_parent, LOC
                'get_prop, GETP
                'get_prop_addr, GETPT
                'get_prop_len, PTSIZE
                'get_wind_prop, WINGET
                'get_next_prop, NEXTP
                'load, VALUE
                'loadb, GETB
                'loadw, GET
                'log_shift, SHIFT
                'Mod, MOD
                'mul, MUL
                'Not, BCOM
                'Or, BOR
                'pull, POP
                'Random, RANDOM
                'read, READ
                'read_char, INPUT
                'restore, RESTORE
                'restore_undo, IRESTORE
                'save, SAVE
                'save_undo, ISAVE
                'set_font, FONT
                'Sub, SUB
                Select Case OpcodeText
                    Case "@add / ADD"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), " + ", OperandStringPseudo(1), "#;")
                    Case "@and / BAND"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), " & ", OperandStringPseudo(1), "#;")
                    Case "@art_shift / ASHIFT"
                        Dim direction As String = "<<"
                        If OperandStringPseudo(1).Substring(1, 1) = "-" Then
                            direction = ">>"
                            OperandStringPseudo(1) = OperandStringPseudo(1).Substring(1)
                        End If
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), " ", direction, " ", OperandStringPseudo(1), "#;")
                    Case "@buffer_screen / *BUFFER_SCREEN*"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = buffer_screen(", OperandStringPseudo(0), ")#;")
                    Case "@call / CALL", "@call_1s / CALL1", "@call_2s / CALL2", "@call_vs / CALL", "@call_vs2 / XCALL"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), "(", PseudoConcatOperands(1), ")#;")
                    Case "@catch / CÁTCH"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = catch();")
                    Case "@check_unicode / CHECKU"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = check_unicode(", OperandStringPseudo(0), ")#;")
                    Case "@div / DIV"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), " / ", OperandStringPseudo(1), "#;")
                    Case "@get_parent / LOC"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = parent(", OperandStringPseudo(0), ")#;")
                    Case "@get_prop / GETP"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), ".", OperandStringPseudo(1), "#;")
                    Case "@get_prop_addr / GETPT"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), ".&", OperandStringPseudo(1), "#;")
                    Case "@get_prop_len / PTSIZE"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), ".#", OperandStringPseudo(1), "#;")
                    Case "@get_wind_prop / WINGET"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = get_wind_prop(", OperandStringPseudo(0), ", ", OperandStringPseudo(1), ")#;")
                    Case "@get_next_prop / NEXTP"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = get_next_prop(", OperandStringPseudo(0), ", ", OperandStringPseudo(1), ")#;")
                    Case "@load / VALUE"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), "#;")
                    Case "@loadb / GETB"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), "->", OperandStringPseudo(1), "#;")
                    Case "@loadw / GET"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), "-->", OperandStringPseudo(1), "#;")
                    Case "@log_shift / SHIFT"
                        Dim direction As String = "<<"
                        If OperandStringPseudo(1).Substring(1, 1) = "-" Then
                            direction = ">>>"
                            OperandStringPseudo(1) = OperandStringPseudo(1).Substring(1)
                        End If
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), " ", direction, " ", OperandStringPseudo(1), "#;")
                    Case "@mod / MOD"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), " % ", OperandStringPseudo(1), "#;")
                    Case "@mul / MUL"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), " * ", OperandStringPseudo(1), "#;")
                    Case "@not / BCOM"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ~", OperandStringPseudo(0), "#;")
                    Case "@or / BOR"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), " | ", OperandStringPseudo(1), "#;")
                    Case "@pull / POP"
                        If OperandAddrMode(0) = EnumAddressMode.NONE Then
                            pseudoCodeString = String.Concat(StoreStringPseudo, " = stack.pop()#;")
                        Else
                            pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), ".pop()#;")
                        End If
                    Case "@random / RANDOM"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = random(", OperandStringPseudo(0), ")#;")
                    Case "@read / READ"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = read(", PseudoConcatOperands(0), ")#;")
                    Case "@read_char / INPUT"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = read_char(", PseudoConcatOperands(0), ")#;")
                    Case "@restore / RESTORE"
                        Dim sTmp As String = ""
                        If Not OperandAddrMode(1) = EnumAddressMode.NONE Then sTmp = PseudoConcatOperands(0)
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = restore(", sTmp, ")#;")
                    Case "@restore_undo / IRESTORE"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = restore_undo(", OperandStringPseudo(0), ")#;")
                    Case "@save / SAVE"
                        Dim sTmp As String = ""
                        If Not OperandAddrMode(1) = EnumAddressMode.NONE Then sTmp = PseudoConcatOperands(0)
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = save(", sTmp, ")#;")
                    Case "@save_undo / ISAVE"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = save_undo(", OperandStringPseudo(0), ")#;")
                    Case "@set_font / FONT"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = set_font(", PseudoConcatOperands(0), ")#;")
                    Case "@sub / SUB"
                        pseudoCodeString = String.Concat(StoreStringPseudo, " = ", OperandStringPseudo(0), " - ", OperandStringPseudo(1), "#;")
                End Select
            ElseIf Extra = EnumExtra.E_TEXT Then
                'print, PRINTI
                'print_ret, PRINTR
                Dim pseudoText As String = Text.Replace("{", "").Replace("}", "")
                If pseudoText.Length > 40 Then pseudoText = String.Concat(pseudoText.AsSpan(0, 40), "...")
                Select Case OpcodeText
                    Case "@print / PRINTI"
                        pseudoCodeString = String.Concat("print ", Chr(34), pseudoText, Chr(34), ";")
                    Case "@print_ret / PRINTR"
                        pseudoCodeString = String.Concat("print_ret ", Chr(34), pseudoText, Chr(34), ";")
                End Select
            ElseIf Extra = EnumExtra.E_NONE Then
                ' @buffer_mode / BUFOUT
                ' @call_1n / ICALL1
                ' @call_2n / ICALL2
                ' @call_vn / ICALL
                ' @call_vn2 / IXCALL
                ' @clear_attr / FCLEAR
                ' @copy_table / COPYT
                ' @dec / DEC
                ' @draw_picture / DISPLAY
                ' @encode_text / ZWSTR
                ' @erase_line / ERASE
                ' @erase_picture / DCLEAR
                ' @erase_window / CLEAR
                ' @get_cursor / CURGET
                ' @inc / INC
                ' @input_stream / DIRIN
                ' @insert_obj / MOVE
                ' @jump / JUMP
                ' @mouse_window / MOUSE-LIMIT
                ' @move_window / WINPOS
                ' @new_line / CRLF
                ' @nop / NOOP
                ' @output_stream / DIROUT
                ' @picture_table / PICSET
                ' @pop / FSTACK
                ' @pop_stack / FSTACK
                ' @print_addr / PRINTB
                ' @print_char / PRINTC
                ' @print_form / PRINTF
                ' @print_num / PRINTN
                ' @print_obj / PRINTD
                ' @print_paddr / PRINT
                ' @print_table / PRINTT
                ' @print_unicode / PRINTU
                ' @pull / POP
                ' @push / PUSH
                ' @put_prop / PUTP
                ' @put_wind_prop / WINPUT
                ' @quit / QUIT
                ' @read / READ
                ' @read_mouse / MOUSE-INFO
                ' @remove_obj / REMOVE
                ' @restart / RESTART
                ' @ret / RETURN
                ' @ret_popped / RSTACK
                ' @rfalse / RFALSE
                ' @rtrue / RTRUE
                ' @scroll_window / SCROLL
                ' @set_attr / FSET
                ' @set_colour / COLOR
                ' @set_cursor / CURSET
                ' @set_margins / MARGIN
                ' @set_text_style / HLIGHT
                ' @set_true_colour / *SET_TRUE_COLOUR*
                ' @set_window / SCREEN
                ' @show_status / USL
                ' @sound_effect / SOUND
                ' @split_window / SPLIT
                ' @store / SET
                ' @storeb / PUTB
                ' @storew / PUT
                ' @throw / THROW
                ' @tokenise / LEX
                ' @window_size / WINSIZE
                ' @window_style / WINATTR
                Select Case OpcodeText
                    Case "@buffer_mode / BUFOUT"
                        pseudoCodeString = String.Concat("buffer_mode(", OperandStringPseudo(0), ");")
                    Case "@call_1n / ICALL1", "@call_2n / ICALL2", "@call_vn / ICALL", "@call_vn2 / IXCALL"
                        pseudoCodeString = String.Concat(OperandStringPseudo(0), "(", PseudoConcatOperands(1), ");")
                    Case "@clear_attr / FCLEAR"
                        pseudoCodeString = String.Concat("give ", OperandStringPseudo(0), " ~", OperandStringPseudo(1), ";")
                    Case "@copy_table / COPYT"
                        pseudoCodeString = String.Concat("copy_table(", PseudoConcatOperands, ");")
                    Case "@dec / DEC"
                        pseudoCodeString = String.Concat(OperandStringPseudo(0), "--;")
                    Case "@draw_picture / DISPLAY"
                        pseudoCodeString = String.Concat("draw_picture(", PseudoConcatOperands, ");")
                    Case "@encode_text / ZWSTR"
                        pseudoCodeString = String.Concat("encode_text(", PseudoConcatOperands, ");")
                    Case "@erase_line / ERASE"
                        pseudoCodeString = String.Concat("erase_line(", PseudoConcatOperands, ");")
                    Case "@erase_picture / DCLEAR"
                        pseudoCodeString = String.Concat("erase_picture(", PseudoConcatOperands, ");")
                    Case "@erase_window / CLEAR"
                        pseudoCodeString = String.Concat("erase_window(", OperandStringPseudo(0), ");")
                    Case "@get_cursor / CURGET"
                        pseudoCodeString = String.Concat("get_cursor(", OperandStringPseudo(0), ");")
                    Case "@inc / INC"
                        pseudoCodeString = String.Concat(OperandStringPseudo(0), "++;")
                    Case "@input_stream / DIRIN"
                        pseudoCodeString = String.Concat("input_stream(", OperandStringPseudo(0), ");")
                    Case "@insert_obj / MOVE"
                        pseudoCodeString = String.Concat("move ", OperandStringPseudo(0), " to ", OperandStringPseudo(1), ";")
                    Case "@jump / JUMP"
                        pseudoCodeString = String.Concat("jump ", OperandStringPseudo(0), ";")
                    Case "@mouse_window / MOUSE-LIMIT"
                        pseudoCodeString = String.Concat("mouse_window(", OperandStringPseudo(0), ");")
                    Case "@move_window / WINPOS"
                        pseudoCodeString = String.Concat("move_window(", PseudoConcatOperands, ");")
                    Case "@new_line / CRLF"
                        pseudoCodeString = "new_line;"
                    Case "@nop / NOOP"
                        pseudoCodeString = "nop;"
                    Case "@output_stream / DIROUT"
                        pseudoCodeString = String.Concat("output_stream(", PseudoConcatOperands, ");")
                    Case "@picture_table / PICSET"
                        pseudoCodeString = String.Concat("picture_table(", OperandStringPseudo(0), ");")
                    Case "@pop / FSTACK"
                        pseudoCodeString = "stack.pop();"
                    Case "@pop_stack / FSTACK"
                        Dim sTmp As String = "stack"
                        If Not OperandAddrMode(1) = EnumAddressMode.NONE Then sTmp = OperandStringPseudo(1)
                        pseudoCodeString = String.Concat(sTmp, ".pop(", OperandStringPseudo(0), ");")
                    Case "@print_addr / PRINTB"
                        pseudoCodeString = String.Concat("print_addr(", OperandStringPseudo(0), ");")
                    Case "@print_char / PRINTC"
                        pseudoCodeString = String.Concat("print_char(", OperandStringPseudo(0), ");")
                    Case "@print_form / PRINTF"
                        pseudoCodeString = String.Concat("print_form(", OperandStringPseudo(0), ");")
                    Case "@print_num / PRINTN"
                        pseudoCodeString = String.Concat("print_num(", OperandStringPseudo(0), ");")
                    Case "@print_obj / PRINTD"
                        pseudoCodeString = String.Concat("print_obj(", OperandStringPseudo(0), ");")
                    Case "@print_paddr / PRINT"
                        Dim pseudoText As String = OperandStringPseudo(0)
                        If pseudoText.Contains(Chr(34)) Then
                            pseudoText = pseudoText.Substring(pseudoText.IndexOf(Chr(34)))
                            pseudoText = pseudoText.Replace("{", "").Replace("}", "").Replace(Chr(34), "")
                            If pseudoText.Length > 40 Then pseudoText = String.Concat(pseudoText.AsSpan(0, 40), "...")
                            pseudoText = String.Concat(Chr(34), pseudoText, Chr(34))
                        End If
                        pseudoCodeString = String.Concat("print_paddr ", pseudoText, ";")
                    Case "@print_table / PRINTT"
                        pseudoCodeString = String.Concat("print_table(", PseudoConcatOperands, ");")
                    Case "@print_unicode / PRINTU"
                        pseudoCodeString = String.Concat("print_unicode(", OperandStringPseudo(0), ");")
                    Case "@pull / POP"
                        pseudoCodeString = String.Concat(OperandStringPseudo(0), " = stack.pop()#;")
                    Case "@push / PUSH"
                        pseudoCodeString = String.Concat("stack.push(", OperandStringPseudo(0), ");")
                    Case "@put_prop / PUTP"
                        pseudoCodeString = String.Concat(OperandStringPseudo(0), ".", OperandStringPseudo(1), " = ", OperandStringPseudo(2), ";")
                    Case "@put_wind_prop / WINPUT"
                        pseudoCodeString = String.Concat("put_wind_prop(", PseudoConcatOperands, ");")
                    Case "@quit / QUIT"
                        pseudoCodeString = "quit;"
                    Case "@read / READ"
                        pseudoCodeString = String.Concat("read(", PseudoConcatOperands, ");")
                    Case "@read_mouse / MOUSE-INFO"
                        pseudoCodeString = String.Concat("read_mouse(", OperandStringPseudo(0), ");")
                    Case "@remove_obj / REMOVE"
                        pseudoCodeString = String.Concat("remove ", OperandStringPseudo(0), ";")
                    Case "@restart / RESTART"
                        pseudoCodeString = "restart;"
                    Case "@ret / RETURN"
                        pseudoCodeString = String.Concat("return ", OperandStringPseudo(0), ";")
                    Case "@ret_popped / RSTACK"
                        pseudoCodeString = "return stack.pop();"
                    Case "@rfalse / RFALSE"
                        pseudoCodeString = "rfalse;"
                    Case "@rtrue / RTRUE"
                        pseudoCodeString = "rtrue;"
                    Case "@scroll_window / SCROLL"
                        pseudoCodeString = String.Concat("scroll_window(", PseudoConcatOperands, ");")
                    Case "@set_attr / FSET"
                        pseudoCodeString = String.Concat("give ", OperandStringPseudo(0), " ", OperandStringPseudo(1), ";")
                    Case "@set_colour / COLOR"
                        pseudoCodeString = String.Concat("set_colour(", PseudoConcatOperands, ");")
                    Case "@set_cursor / CURSET"
                        pseudoCodeString = String.Concat("set_cursor(", PseudoConcatOperands, ");")
                    Case "@set_margins / MARGIN"
                        pseudoCodeString = String.Concat("set_margins(", PseudoConcatOperands, ");")
                    Case "@set_text_style / HLIGHT"
                        pseudoCodeString = String.Concat("set_text_style(", OperandStringPseudo(0), ");")
                    Case "@set_true_colour / *SET_TRUE_COLOUR*"
                        pseudoCodeString = String.Concat("set_true_colour(", PseudoConcatOperands, ");")
                    Case "@set_window / SCREEN"
                        pseudoCodeString = String.Concat("set_window(", OperandStringPseudo(0), ");")
                    Case "@show_status / USL"
                        pseudoCodeString = "show_status;"
                    Case "@sound_effect / SOUND"
                        pseudoCodeString = String.Concat("sound_effect(", PseudoConcatOperands, ");")
                    Case "@split_window / SPLIT"
                        pseudoCodeString = String.Concat("split_window(", OperandStringPseudo(0), ");")
                    Case "@store / SET"
                        pseudoCodeString = String.Concat(OperandStringPseudo(0), " = ", OperandStringPseudo(1), "#;")
                    Case "@storeb / PUTB"
                        pseudoCodeString = String.Concat(OperandStringPseudo(0), "->", OperandStringPseudo(1), " = ", OperandStringPseudo(2), ";")
                    Case "@storew / PUT"
                        pseudoCodeString = String.Concat(OperandStringPseudo(0), "-->", OperandStringPseudo(1), " = ", OperandStringPseudo(2), ";")
                    Case "@throw / THROW"
                        pseudoCodeString = String.Concat("throw(", PseudoConcatOperands, ");")
                    Case "@tokenise / LEX"
                        pseudoCodeString = String.Concat("tokenise(", PseudoConcatOperands, ");")
                    Case "@window_size / WINSIZE"
                        pseudoCodeString = String.Concat("window_size(", PseudoConcatOperands, ");")
                    Case "@window_style / WINATTR"
                        pseudoCodeString = String.Concat("window_style(", PseudoConcatOperands, ");")
                End Select
            End If
            pseudoCodeString = pseudoCodeString.Replace("sp = ", "stack.push(")
            If pseudoCodeString.Contains("stack.push") Then pseudoCodeString = pseudoCodeString.Replace("#;", ");") Else pseudoCodeString = pseudoCodeString.Replace("#;", ";")
            If stackPeek Then pseudoCodeString = pseudoCodeString.Replace("if (sp", "if (stack.peek()")
            pseudoCodeString = pseudoCodeString.Replace("sp", "stack.pop()")

            Return pseudoCodeString
        End Function

        Private Shared Function OperandDescription(operandValue As Integer) As String
            Select Case operandValue
                Case &H0
                    Return "large constant (long immediate)"
                Case &H1
                    Return "constant (immediate)"
                Case &H2
                    Return "variable"
                Case Else
                    Return "no more operands"
            End Select
        End Function

        Private Shared Function VariableDescription(variableValue As Integer) As String
            If variableValue = 0 Then
                Return "SP"
            ElseIf variableValue < 16 Then
                Return "L" & (variableValue - 1).ToString()
            Else
                Return "G" & (variableValue - 16).ToString()
            End If
        End Function

        Private Function PseudoBranchText() As String
            Select Case BranchAddr
                Case 0
                    Return "rfalse"
                Case 1
                    Return "rtrue"
                Case Else
                    Return "jump 0x" & BranchAddr.ToString("X4")
            End Select
        End Function
        Private Function PseudoConcatOperands(Optional startIndex As Integer = 0, Optional slutIndex As Integer = 3, Optional text As String = ", ") As String
            Dim sTmp As String = OperandStringPseudo(startIndex)
            For i As Integer = startIndex + 1 To slutIndex
                If Not OperandAddrMode(i) = EnumAddressMode.NONE Then sTmp &= text & OperandStringPseudo(i)
            Next
            Return sTmp
        End Function

    End Class

    Public Function DecodeRoutine(piAtAddress As Integer, gameData() As Byte, psAbbreviations() As String, silent As Boolean, zcodeSyntax As Integer,
                                  pValidStringList As List(Of StringData), pValidRoutineList As List(Of RoutineData), pDictEntriesList As DictionaryEntries,
                                  pAlpabet() As String, initialPC As Integer, propertyNumberMin As Integer, propertyNumberMax As Integer,
                                  showAbbrevsInsertionPoints As Boolean, inlineStringsList As List(Of InlineString), showZCodeVerbose As Boolean) As Integer
        startAddress = piAtAddress
        byteGame = gameData
        PC = piAtAddress
        high_pc = PC
        ZVersion = byteGame(0)
        sAbbreviations = psAbbreviations
        bSilent = silent
        syntax = zcodeSyntax
        validStringList = pValidStringList
        validRoutineList = pValidRoutineList
        DictEntriesList = pDictEntriesList
        alphabet = pAlpabet
        propertyMin = propertyNumberMin
        propertyMax = propertyNumberMax
        showAbbrevsInsertion = showAbbrevsInsertionPoints
        inlineStrings = inlineStringsList
        bShowZCodeVerbose = showZCodeVerbose

        If PC > byteGame.Length Then Return -1

        localsCount = byteGame(PC)

        If localsCount > 15 Then Return -1    ' Not a valid routine startIndex address

        If Not bSilent Then
            If initialPC - 1 = PC Then
                Console.Write("Main routine: 0x{0:X5}", PC)
            Else
                Console.Write("Routine: 0x{0:X5}", PC)
            End If
            Dim oRoutineData As RoutineData = validRoutineList.Find(Function(c) c.entryPoint = PC)
            If oRoutineData IsNot Nothing Then
                Dim first As Boolean = True
                For Each callFrom As String In oRoutineData.callsFrom
                    If first Then Console.Write(Space(15)) Else Console.Write(Space(31))
                    If callFrom.Length > 97 Then
                        Console.WriteLine(callFrom.Substring(0, 98))
                        Dim sRest As String = callFrom.Substring(98).Trim
                        Do
                            If sRest.Length > 71 Then
                                Console.WriteLine("{0}{1}", Space(57), sRest.Substring(0, 72).Trim)
                                sRest = sRest.Substring(72).Trim
                            Else
                                Console.WriteLine("{0}{1}", Space(57), sRest)
                                sRest = ""
                            End If
                        Loop Until sRest = ""
                    Else
                        Console.WriteLine(callFrom)
                    End If
                    first = False
                Next
                If oRoutineData.callsFrom.Count = 0 Then Console.WriteLine()
            End If
            If ZVersion > 4 Then
                If localsCount = 0 Then Console.WriteLine("{0:X5} {1:X2}                       No locals", PC, localsCount)
                If localsCount = 1 Then
                    Console.WriteLine("{0:X5} {1:X2}                       1 local", PC, localsCount)
                    Console.Write(Space(31))
                    Console.WriteLine("({0})", TextVariable(1, syntax))
                End If
                If localsCount > 1 Then
                    Console.WriteLine("{0:X5} {1:X2}                       {2} locals", PC, localsCount, localsCount)
                    Console.Write(Space(31) & "(")
                    For i = 0 To localsCount - 1
                        Console.Write("{0}", TextVariable(i + 1, syntax))
                        If i < localsCount - 1 Then Console.Write(" ")
                    Next
                    Console.WriteLine(")")
                End If
            Else
                If localsCount = 0 Then Console.WriteLine("{0:X5} {1:X2}                       No locals", PC, localsCount)
                If localsCount = 1 Then Console.WriteLine("{0:X5} {1:X2}                       1 local", PC, localsCount)
                If localsCount > 1 Then Console.WriteLine("{0:X5} {1:X2}                       {2} locals", PC, localsCount, localsCount)
                If localsCount > 0 Then
                    Console.Write("{0:X5} ", PC + 1)
                    Dim count As Integer = 0
                    Dim text As String = " "
                    For i = 0 To localsCount - 1
                        Console.Write("{0:X2} {1:X2} ", byteGame(PC + i * 2 + 1), byteGame(PC + i * 2 + 2))
                        Dim number As Integer = byteGame(PC + i * 2 + 1) * 256 + byteGame(PC + i * 2 + 2)
                        If syntax = 1 Then text = text & TextVariable(i + 1, syntax) & "=0x" & number.ToString("x4") & " " Else text = text & TextVariable(i + 1, syntax) & "=0x" & number.ToString("X4") & " "
                        count += 1
                        If count = 4 Then
                            count = 0
                            Console.WriteLine(text)
                            Console.Write(Space(6))
                            text = " "
                        End If
                    Next
                    If count > 0 Then
                        Console.Write(Space(6 * (4 - count)))
                        Console.WriteLine(text)
                    End If
                End If
            End If
            Console.WriteLine()
        End If

        If ZVersion < 5 Then PC += localsCount * 2

        PC += 1

        Dim oDecodeResult As DecodeResult
        Dim high_routine_old As Integer = highest_routine
        Dim low_routine_old As Integer = lowest_routine
        Dim lowest_string_old As Integer = lowest_string

        callsTo.Clear()
        Do
            oDecodeResult = DecodeCode(PC)
            PC = oDecodeResult.nextPC

            If oDecodeResult.status = EnumStatus.BAD_OPCODE Or oDecodeResult.status = EnumStatus.BAD_ENTRY Then
                highest_routine = high_routine_old
                lowest_routine = low_routine_old
                lowest_string = lowest_string_old
                callsTo.Clear()
                Return -1
            End If

        Loop Until oDecodeResult.status = EnumStatus.END_OF_ROUTINE

        Return PC
    End Function

    Private Function DecodeCode(piAtAddress As Integer) As DecodeResult
        Dim iPC As Integer = piAtAddress

        If piAtAddress > byteGame.Length - 10 Then
            Return New DecodeResult With {.status = EnumStatus.BAD_ENTRY}
        End If

        Dim oOpcode As New Opcode With {
            .Address = piAtAddress,
            .Code = byteGame(iPC),
            .OpcodeClass = EnumOpcodeClass.ZERO_OPERAND
        }

        If ZVersion > 4 And oOpcode.Code = 190 Then ' This is the EXTOP opcode
            oOpcode.OpcodeClass = EnumOpcodeClass.EXTENDED_OPERAND
            iPC += 1
            oOpcode.Code = byteGame(iPC)
            oOpcode.ExtModeByte = byteGame(iPC + 1)
        Else
            If oOpcode.Code < 128 Then
                oOpcode.OpcodeClass = EnumOpcodeClass.TWO_OPERAND
            ElseIf oOpcode.Code < 176 Then
                oOpcode.OpcodeClass = EnumOpcodeClass.ONE_OPERAND
            ElseIf oOpcode.Code < 192 Then
                oOpcode.OpcodeClass = EnumOpcodeClass.ZERO_OPERAND
            Else
                oOpcode.OpcodeClass = EnumOpcodeClass.VARIABLE_OPERAND
            End If
        End If

        ' decode instruction
        Dim iCode As Integer = oOpcode.Code
        Select Case oOpcode.OpcodeClass
            Case EnumOpcodeClass.EXTENDED_OPERAND
                iCode = iCode And &H3F
                Select Case iCode
                    Case &H0 : Return DecodeOperands(oOpcode, "SAVE", "SAVE", EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H1 : Return DecodeOperands(oOpcode, "RESTORE", "RESTORE", EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H2 : Return DecodeOperands(oOpcode, "LOG_SHIFT", "SHIFT", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H3 : Return DecodeOperands(oOpcode, "ART_SHIFT", "ASHIFT", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H4 : Return DecodeOperands(oOpcode, "SET_FONT", "FONT", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H5 : Return DecodeOperands(oOpcode, "DRAW_PICTURE", "DISPLAY", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H6 : Return DecodeOperands(oOpcode, "PICTURE_DATA", "PICINF", EnumOperand.P_NUMBER, EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                    Case &H7 : Return DecodeOperands(oOpcode, "ERASE_PICTURE", "DCLEAR", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H8 : Return DecodeOperands(oOpcode, "SET_MARGINS", "MARGIN", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H9 : Return DecodeOperands(oOpcode, "SAVE_UNDO", "ISAVE", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &HA : Return DecodeOperands(oOpcode, "RESTORE_UNDO", "IRESTORE", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H10 : Return DecodeOperands(oOpcode, "MOVE_WINDOW", "WINPOS", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H11 : Return DecodeOperands(oOpcode, "WINDOW_SIZE", "WINSIZE", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H12 : Return DecodeOperands(oOpcode, "WINDOW_STYLE", "WINATTR", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H13 : Return DecodeOperands(oOpcode, "GET_WIND_PROP", "WINGET", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H14 : Return DecodeOperands(oOpcode, "SCROLL_WINDOW", "SCROLL", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H15 : Return DecodeOperands(oOpcode, "POP_STACK", "FSTACK", EnumOperand.P_NUMBER, EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H16 : Return DecodeOperands(oOpcode, "READ_MOUSE", "MOUSE-INFO", EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H17 : Return DecodeOperands(oOpcode, "MOUSE_WINDOW", "MOUSE-LIMIT", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H18 : Return DecodeOperands(oOpcode, "PUSH_STACK", "XPUSH", EnumOperand.P_NUMBER, EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                    Case &H19 : Return DecodeOperands(oOpcode, "PUT_WIND_PROP", "WINPUT", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_ANYTHING, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H1A : Return DecodeOperands(oOpcode, "PRINT_FORM", "PRINTF", EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H1B : Return DecodeOperands(oOpcode, "MAKE_MENU", "MENU", EnumOperand.P_NUMBER, EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                    Case &H1C : Return DecodeOperands(oOpcode, "PICTURE_TABLE", "PICSET", EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case Else
                        Select Case ZVersion
                            Case 5, 7, 8
                                Select Case iCode
                                    Case &HB : Return DecodeOperands(oOpcode, "PRINT_UNICODE", "PRINTU", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)       'Added in standard 1.0
                                    Case &HC : Return DecodeOperands(oOpcode, "CHECK_UNICODE", "CHECKU", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)       'Added in standard 1.0
                                    Case &HD : Return DecodeOperands(oOpcode, "SET_TRUE_COLOUR", "*SET_TRUE_COLOUR*", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)     'Added in standard 1.1, not available in Zapf
                                    Case Else : Return DecodeOperands(oOpcode, "ILLEGAL", "ILLEGAL", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_ILLEGAL)
                                End Select
                            Case 6
                                Select Case iCode
                                    Case &HB : Return DecodeOperands(oOpcode, "PRINT_UNICODE", "PRINTU", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)       'Added in standard 1.0
                                    Case &HC : Return DecodeOperands(oOpcode, "CHECK_UNICODE", "CHECKU", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)       'Added in standard 1.0
                                    Case &HD : Return DecodeOperands(oOpcode, "SET_TRUE_COLOUR", "*SET_TRUE_COLOUR*", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)     'Added in standard 1.1, not available in Zapf
                                    Case &H1D : Return DecodeOperands(oOpcode, "BUFFER_SCREEN", "*BUFFER_SCREEN*", EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)  'Added in standard 1.1, not available in Zapf
                                    Case Else : Return DecodeOperands(oOpcode, "ILLEGAL", "ILLEGAL", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_ILLEGAL)
                                End Select
                        End Select
                End Select
            Case EnumOpcodeClass.TWO_OPERAND, EnumOpcodeClass.VARIABLE_OPERAND
                If oOpcode.OpcodeClass = EnumOpcodeClass.TWO_OPERAND Then iCode = iCode And &H1F Else iCode = iCode And &H3F
                Select Case iCode
                    Case &H1 : Return DecodeOperands(oOpcode, "JE", "EQUAL?", EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                    Case &H2 : Return DecodeOperands(oOpcode, "JL", "LESS?", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                    Case &H3 : Return DecodeOperands(oOpcode, "JG", "GRTR?", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                    Case &H4 : Return DecodeOperands(oOpcode, "DEC_CHK", "DLESS?", EnumOperand.P_VAR, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                    Case &H5 : Return DecodeOperands(oOpcode, "INC_CHK", "IGRTR?", EnumOperand.P_VAR, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                    Case &H6 : Return DecodeOperands(oOpcode, "JIN", "IN?", EnumOperand.P_OBJECT, EnumOperand.P_OBJECT, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                    Case &H7 : Return DecodeOperands(oOpcode, "TEST", "BTST", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                    Case &H8 : Return DecodeOperands(oOpcode, "OR", "BOR", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H9 : Return DecodeOperands(oOpcode, "AND", "BAND", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &HA : Return DecodeOperands(oOpcode, "TEST_ATTR", "FSET?", EnumOperand.P_OBJECT, EnumOperand.P_ATTRNUM, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                    Case &HB : Return DecodeOperands(oOpcode, "SET_ATTR", "FSET", EnumOperand.P_OBJECT, EnumOperand.P_ATTRNUM, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &HC : Return DecodeOperands(oOpcode, "CLEAR_ATTR", "FCLEAR", EnumOperand.P_OBJECT, EnumOperand.P_ATTRNUM, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &HD : Return DecodeOperands(oOpcode, "STORE", "SET", EnumOperand.P_VAR, EnumOperand.P_ANYTHING, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &HE : Return DecodeOperands(oOpcode, "INSERT_OBJ", "MOVE", EnumOperand.P_OBJECT, EnumOperand.P_OBJECT, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &HF : Return DecodeOperands(oOpcode, "LOADW", "GET", EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H10 : Return DecodeOperands(oOpcode, "LOADB", "GETB", EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H11 : Return DecodeOperands(oOpcode, "GET_PROP", "GETP", EnumOperand.P_OBJECT, EnumOperand.P_PROPNUM, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H12 : Return DecodeOperands(oOpcode, "GET_PROP_ADDR", "GETPT", EnumOperand.P_OBJECT, EnumOperand.P_PROPNUM, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H13 : Return DecodeOperands(oOpcode, "GET_NEXT_PROP", "NEXTP", EnumOperand.P_OBJECT, EnumOperand.P_PROPNUM, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    'TXD have LOW_ADDR for operands, why? 
                    'Case &H14 : Return DecodeOperands(oOpcode, "ADD", "ADD", EnumOperand.P_LOW_ADDR, EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H14 : Return DecodeOperands(oOpcode, "ADD", "ADD", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H15 : Return DecodeOperands(oOpcode, "SUB", "SUB", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H16 : Return DecodeOperands(oOpcode, "MUL", "MUL", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H17 : Return DecodeOperands(oOpcode, "DIV", "DIV", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H18 : Return DecodeOperands(oOpcode, "MOD", "MOD", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H21 : Return DecodeOperands(oOpcode, "STOREW", "PUT", EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_ANYTHING, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H22 : Return DecodeOperands(oOpcode, "STOREB", "PUTB", EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_ANYTHING, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H23 : Return DecodeOperands(oOpcode, "PUT_PROP", "PUTP", EnumOperand.P_OBJECT, EnumOperand.P_PROPNUM, EnumOperand.P_ANYTHING, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H25 : Return DecodeOperands(oOpcode, "PRINT_CHAR", "PRINTC", EnumOperand.P_PCHAR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H26 : Return DecodeOperands(oOpcode, "PRINT_NUM", "PRINTN", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H27 : Return DecodeOperands(oOpcode, "RANDOM", "RANDOM", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H28 : Return DecodeOperands(oOpcode, "PUSH", "PUSH", EnumOperand.P_ANYTHING, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H2A : Return DecodeOperands(oOpcode, "SPLIT_WINDOW", "SPLIT", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H2B : Return DecodeOperands(oOpcode, "SET_WINDOW", "SCREEN", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H33 : Return DecodeOperands(oOpcode, "OUTPUT_STREAM", "DIROUT", EnumOperand.P_PATTR, EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H34 : Return DecodeOperands(oOpcode, "INPUT_STREAM", "DIRIN", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H35 : Return DecodeOperands(oOpcode, "SOUND_EFFECT", "SOUND", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_ROUTINE, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case Else
                        Select Case ZVersion
                            Case 1, 2, 3
                                Select Case iCode
                                    Case &H20 : Return DecodeOperands(oOpcode, "CALL", "CALL", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumExtra.E_STORE, EnumType.T_CALL)
                                    Case &H24 : Return DecodeOperands(oOpcode, "READ", "READ", EnumOperand.P_LOW_ADDR, EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H29 : Return DecodeOperands(oOpcode, "PULL", "POP", EnumOperand.P_VAR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case Else : Return DecodeOperands(oOpcode, "ILLEGAL", "ILLEGAL", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_ILLEGAL)
                                End Select
                            Case 4
                                Select Case iCode
                                    Case &H19 : Return DecodeOperands(oOpcode, "CALL_2S", "CALL2", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_CALL)
                                    Case &H20 : Return DecodeOperands(oOpcode, "CALL_VS", "CALL", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumExtra.E_STORE, EnumType.T_CALL)
                                    Case &H24 : Return DecodeOperands(oOpcode, "READ", "READ", EnumOperand.P_LOW_ADDR, EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_ROUTINE, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H29 : Return DecodeOperands(oOpcode, "PULL", "POP", EnumOperand.P_VAR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H2C : Return DecodeOperands(oOpcode, "CALL_VS2", "XCALL", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumExtra.E_STORE, EnumType.T_CALL)
                                    Case &H2D : Return DecodeOperands(oOpcode, "ERASE_WINDOW", "CLEAR", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H2E : Return DecodeOperands(oOpcode, "ERASE_LINE", "ERASE", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H2F : Return DecodeOperands(oOpcode, "SET_CURSOR", "CURSET", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H31 : Return DecodeOperands(oOpcode, "SET_TEXT_STYLE", "HLIGHT", EnumOperand.P_VATTR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H32 : Return DecodeOperands(oOpcode, "BUFFER_MODE", "BUFOUT", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H36 : Return DecodeOperands(oOpcode, "READ_CHAR", "INPUT", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_ROUTINE, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                                    Case &H37 : Return DecodeOperands(oOpcode, "SCAN_TABLE", "INTBL?", EnumOperand.P_ANYTHING, EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumExtra.E_BOTH, EnumType.T_PLAIN)
                                    Case Else : Return DecodeOperands(oOpcode, "ILLEGAL", "ILLEGAL", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_ILLEGAL)
                                End Select
                            Case 5, 7, 8
                                Select Case iCode
                                    Case &H19 : Return DecodeOperands(oOpcode, "CALL_2S", "CALL2", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_CALL)
                                    Case &H1A : Return DecodeOperands(oOpcode, "CALL_2N", "ICALL2", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_CALL)
                                    Case &H1B : Return DecodeOperands(oOpcode, "SET_COLOUR", "COLOR", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H1C : Return DecodeOperands(oOpcode, "THROW", "THROW", EnumOperand.P_ANYTHING, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)               ' This is a valid opcode in v 5-
                                    Case &H20 : Return DecodeOperands(oOpcode, "CALL_VS", "CALL", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumExtra.E_STORE, EnumType.T_CALL)
                                    Case &H24 : Return DecodeOperands(oOpcode, "READ", "READ", EnumOperand.P_LOW_ADDR, EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_ROUTINE, EnumExtra.E_STORE, EnumType.T_PLAIN)
                                    Case &H29 : Return DecodeOperands(oOpcode, "PULL", "POP", EnumOperand.P_VAR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H2C : Return DecodeOperands(oOpcode, "CALL_VS2", "XCALL", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumExtra.E_STORE, EnumType.T_CALL)
                                    Case &H2D : Return DecodeOperands(oOpcode, "ERASE_WINDOW", "CLEAR", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H2E : Return DecodeOperands(oOpcode, "ERASE_LINE", "ERASE", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H2F : Return DecodeOperands(oOpcode, "SET_CURSOR", "CURSET", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H30 : Return DecodeOperands(oOpcode, "GET_CURSOR", "CURGET", EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H31 : Return DecodeOperands(oOpcode, "SET_TEXT_STYLE", "HLIGHT", EnumOperand.P_VATTR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H32 : Return DecodeOperands(oOpcode, "BUFFER_MODE", "BUFOUT", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H36 : Return DecodeOperands(oOpcode, "READ_CHAR", "INPUT", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_ROUTINE, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                                    Case &H37 : Return DecodeOperands(oOpcode, "SCAN_TABLE", "INTBL?", EnumOperand.P_ANYTHING, EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumExtra.E_BOTH, EnumType.T_PLAIN)
                                    Case &H38 : Return DecodeOperands(oOpcode, "NOT", "BCOM", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                                    Case &H39 : Return DecodeOperands(oOpcode, "CALL_VN", "ICALL", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumExtra.E_NONE, EnumType.T_CALL)
                                    Case &H3A : Return DecodeOperands(oOpcode, "CALL_VN2", "IXCALL", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumExtra.E_NONE, EnumType.T_CALL)
                                    Case &H3B : Return DecodeOperands(oOpcode, "TOKENISE", "LEX", EnumOperand.P_LOW_ADDR, EnumOperand.P_LOW_ADDR, EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H3C : Return DecodeOperands(oOpcode, "ENCODE_TEXT", "ZWSTR", EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_LOW_ADDR, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H3D : Return DecodeOperands(oOpcode, "COPY_TABLE", "COPYT", EnumOperand.P_LOW_ADDR, EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H3E : Return DecodeOperands(oOpcode, "PRINT_TABLE", "PRINTT", EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H3F : Return DecodeOperands(oOpcode, "CHECK_ARG_COUNT", "ASSIGNED?", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                                    Case Else : Return DecodeOperands(oOpcode, "ILLEGAL", "ILLEGAL", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_ILLEGAL)
                                End Select
                            Case 6
                                Select Case iCode
                                    Case &H19 : Return DecodeOperands(oOpcode, "CALL_2S", "CALL2", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_CALL)
                                    Case &H1A : Return DecodeOperands(oOpcode, "CALL_2N", "ICALL2", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_CALL)
                                    Case &H1B : Return DecodeOperands(oOpcode, "SET_COLOUR", "COLOR", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H1C : Return DecodeOperands(oOpcode, "THROW", "THROW", EnumOperand.P_ANYTHING, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H20 : Return DecodeOperands(oOpcode, "CALL_VS", "CALL", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumExtra.E_STORE, EnumType.T_CALL)
                                    Case &H24 : Return DecodeOperands(oOpcode, "READ", "READ", EnumOperand.P_LOW_ADDR, EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_ROUTINE, EnumExtra.E_STORE, EnumType.T_PLAIN)
                                    Case &H29 : Return DecodeOperands(oOpcode, "PULL", "POP", EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                                    Case &H2C : Return DecodeOperands(oOpcode, "CALL_VS2", "XCALL", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumExtra.E_STORE, EnumType.T_CALL)
                                    Case &H2D : Return DecodeOperands(oOpcode, "ERASE_WINDOW", "CLEAR", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H2E : Return DecodeOperands(oOpcode, "ERASE_LINE", "ERASE", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H2F : Return DecodeOperands(oOpcode, "SET_CURSOR", "CURSET", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H30 : Return DecodeOperands(oOpcode, "GET_CURSOR", "CURGET", EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H31 : Return DecodeOperands(oOpcode, "SET_TEXT_STYLE", "HLIGHT", EnumOperand.P_VATTR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H32 : Return DecodeOperands(oOpcode, "BUFFER_MODE", "BUFOUT", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H36 : Return DecodeOperands(oOpcode, "READ_CHAR", "INPUT", EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_ROUTINE, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                                    Case &H37 : Return DecodeOperands(oOpcode, "SCAN_TABLE", "INTBL?", EnumOperand.P_ANYTHING, EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumExtra.E_BOTH, EnumType.T_PLAIN)
                                    Case &H38 : Return DecodeOperands(oOpcode, "NOT", "BCOM", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                                    Case &H39 : Return DecodeOperands(oOpcode, "CALL_VN", "ICALL", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumExtra.E_NONE, EnumType.T_CALL)
                                    Case &H3A : Return DecodeOperands(oOpcode, "CALL_VN2", "IXCALL", EnumOperand.P_ROUTINE, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumOperand.P_ANYTHING, EnumExtra.E_NONE, EnumType.T_CALL)
                                    Case &H3B : Return DecodeOperands(oOpcode, "TOKENISE", "LEX", EnumOperand.P_LOW_ADDR, EnumOperand.P_LOW_ADDR, EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H3C : Return DecodeOperands(oOpcode, "ENCODE_TEXT", "ZWSTR", EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_LOW_ADDR, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H3D : Return DecodeOperands(oOpcode, "COPY_TABLE", "COPYT", EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H3E : Return DecodeOperands(oOpcode, "PRINT_TABLE", "PRINTT", EnumOperand.P_LOW_ADDR, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumOperand.P_NUMBER, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &H3F : Return DecodeOperands(oOpcode, "CHECK_ARG_COUNT", "ASSIGNED?", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                                    Case Else : Return DecodeOperands(oOpcode, "ILLEGAL", "ILLEGAL", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_ILLEGAL)
                                End Select
                        End Select
                End Select
            Case EnumOpcodeClass.ONE_OPERAND
                iCode = iCode And &HF
                Select Case iCode
                    Case &H0 : Return DecodeOperands(oOpcode, "JZ", "ZERO?", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                    Case &H1 : Return DecodeOperands(oOpcode, "GET_SIBLING", "NEXT?", EnumOperand.P_OBJECT, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BOTH, EnumType.T_PLAIN)
                    Case &H2 : Return DecodeOperands(oOpcode, "GET_CHILD", "FIRST?", EnumOperand.P_OBJECT, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BOTH, EnumType.T_PLAIN)
                    Case &H3 : Return DecodeOperands(oOpcode, "GET_PARENT", "LOC", EnumOperand.P_OBJECT, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H4 : Return DecodeOperands(oOpcode, "GET_PROP_LEN", "PTSIZE", EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case &H5 : Return DecodeOperands(oOpcode, "INC", "INC", EnumOperand.P_VAR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H6 : Return DecodeOperands(oOpcode, "DEC", "DEC", EnumOperand.P_VAR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H7 : Return DecodeOperands(oOpcode, "PRINT_ADDR", "PRINTB", EnumOperand.P_LOW_ADDR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H9 : Return DecodeOperands(oOpcode, "REMOVE_OBJ", "REMOVE", EnumOperand.P_OBJECT, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &HA : Return DecodeOperands(oOpcode, "PRINT_OBJ", "PRINTD", EnumOperand.P_OBJECT, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &HB : Return DecodeOperands(oOpcode, "RET", "RETURN", EnumOperand.P_ANYTHING, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_RETURN)
                    Case &HC : Return DecodeOperands(oOpcode, "JUMP", "JUMP", EnumOperand.P_LABEL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_RETURN)
                    Case &HD : Return DecodeOperands(oOpcode, "PRINT_PADDR", "PRINT", EnumOperand.P_STATIC, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &HE : Return DecodeOperands(oOpcode, "LOAD", "VALUE", EnumOperand.P_VAR, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                    Case Else
                        Select Case ZVersion
                            Case 1, 2, 3
                                Select Case iCode
                                    Case &HF : Return DecodeOperands(oOpcode, "NOT", "BCOM", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                                    Case Else : Return DecodeOperands(oOpcode, "ILLEGAL", "ILLEGAL", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_ILLEGAL)
                                End Select
                            Case 4
                                Select Case iCode
                                    Case &H8 : Return DecodeOperands(oOpcode, "CALL_1S", "CALL1", EnumOperand.P_ROUTINE, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_CALL)
                                    Case &HF : Return DecodeOperands(oOpcode, "NOT", "BCOM", EnumOperand.P_NUMBER, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                                    Case Else : Return DecodeOperands(oOpcode, "ILLEGAL", "ILLEGAL", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_ILLEGAL)
                                End Select
                            Case 5, 6, 7, 8
                                Select Case iCode
                                    Case &H8 : Return DecodeOperands(oOpcode, "CALL_1S", "CALL1", EnumOperand.P_ROUTINE, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_CALL)
                                    Case &HF : Return DecodeOperands(oOpcode, "CALL_1N", "ICALL1", EnumOperand.P_ROUTINE, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_CALL)
                                    Case Else : Return DecodeOperands(oOpcode, "ILLEGAL", "ILLEGAL", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_ILLEGAL)
                                End Select
                        End Select
                End Select
            Case EnumOpcodeClass.ZERO_OPERAND
                iCode = iCode And &HF
                Select Case iCode
                    Case &H0 : Return DecodeOperands(oOpcode, "RTRUE", "RTRUE", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_RETURN)
                    Case &H1 : Return DecodeOperands(oOpcode, "RFALSE", "RFALSE", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_RETURN)
                    Case &H2 : Return DecodeOperands(oOpcode, "PRINT", "PRINTI", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_TEXT, EnumType.T_PLAIN)
                    Case &H3 : Return DecodeOperands(oOpcode, "PRINT_RET", "PRINTR", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_TEXT, EnumType.T_RETURN)
                    Case &H4 : Return DecodeOperands(oOpcode, "NOP", "NOOP", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &H7 : Return DecodeOperands(oOpcode, "RESTART", "RESTART", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_RETURN)
                    Case &H8 : Return DecodeOperands(oOpcode, "RET_POPPED", "RSTACK", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_RETURN)
                    Case &HA : Return DecodeOperands(oOpcode, "QUIT", "QUIT", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_RETURN)
                    Case &HB : Return DecodeOperands(oOpcode, "NEW_LINE", "CRLF", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                    Case &HD : Return DecodeOperands(oOpcode, "VERIFY", "VERIFY", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                    Case Else
                        Select Case ZVersion
                            Case 1, 2, 3
                                Select Case iCode
                                    Case &H5 : Return DecodeOperands(oOpcode, "SAVE", "SAVE", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                                    Case &H6 : Return DecodeOperands(oOpcode, "RESTORE", "RESTORE", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                                    Case &H9 : Return DecodeOperands(oOpcode, "POP", "FSTACK", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case &HC : Return DecodeOperands(oOpcode, "SHOW_STATUS", "USL", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case Else : Return DecodeOperands(oOpcode, "ILLEGAL", "ILLEGAL", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_ILLEGAL)
                                End Select
                            Case 4
                                Select Case iCode
                                    Case &H5 : Return DecodeOperands(oOpcode, "SAVE", "SAVE", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                                    Case &H6 : Return DecodeOperands(oOpcode, "RESTORE", "RESTORE", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                                    Case &H9 : Return DecodeOperands(oOpcode, "POP", "FSTACK", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case Else : Return DecodeOperands(oOpcode, "ILLEGAL", "ILLEGAL", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_ILLEGAL)
                                End Select
                            Case 5, 7, 8
                                Select Case iCode
                                    Case &H9 : Return DecodeOperands(oOpcode, "CATCH", "CATCH", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                                    'From a bug in Wishbringer V23
                                    Case &HC : Return DecodeOperands(oOpcode, "SHOW_STATUS", "USL", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_PLAIN)
                                    Case Else : Return DecodeOperands(oOpcode, "ILLEGAL", "ILLEGAL", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_NONE, EnumType.T_ILLEGAL)
                                End Select
                            Case 6
                                Select Case iCode
                                    Case &H9 : Return DecodeOperands(oOpcode, "CATCH", "CATCH", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_STORE, EnumType.T_PLAIN)
                                    Case &HF : Return DecodeOperands(oOpcode, "PIRACY", "ORIGINAL?", EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumOperand.P_NIL, EnumExtra.E_BRANCH, EnumType.T_PLAIN)
                                End Select
                        End Select
                End Select
        End Select

        Dim oReturn As New DecodeResult With {.status = EnumStatus.BAD_OPCODE}
        Return oReturn
    End Function

    Private Function DecodeOperands(pOpcode As Opcode, psOpcodeNameInf As String, psOpcodeNameZil As String, pPar1 As EnumOperand, pPar2 As EnumOperand, pPar3 As EnumOperand, pPar4 As EnumOperand, pExtra As EnumExtra, pType As EnumType) As DecodeResult
        pOpcode.OperandType(0) = pPar1
        pOpcode.OperandType(1) = pPar2
        pOpcode.OperandType(2) = pPar3
        pOpcode.OperandType(3) = pPar4
        pOpcode.Extra = pExtra
        pOpcode.Type = pType
        pOpcode.OpcodeText = "@" & psOpcodeNameInf.ToLower & " / " & psOpcodeNameZil
        For i = 0 To 20
            pOpcode.OpcodeBytes(i) = byteGame(pOpcode.Address + i)
        Next

        ' zork0-beta-r242-s880830.z6 and zork0-r242-s880901.z6 contains code that uses 3 parameters for WINGET.
        ' Source code:
        '
        '   <CONSTANT WTBL <LTABLE 0>>
        '       <ROUTINE WINPROP(WIN PROP)
        '       <WINGET .WIN ,WTBL .PROP>
        '       <GET ,WTBL 1>>
        '
        ' Compiles to 02 BE 13 8B 01 70 8B 02 CF 1F 70 8B 01 00 B8 00 at address 0x1cc28.
        '
        ' This is outside the standards of the z-machine and probably only was legel during development
        ' of z6 in an unreleased compiler.
        ' For completeness unz can disassemble it even though probably no interpreter can run it.
        If pOpcode.OpcodeClass = EnumOpcodeClass.EXTENDED_OPERAND And pOpcode.Code = &H13 And pOpcode.ExtModeByte = &H8B Then
            pOpcode.OperandType(0) = EnumOperand.P_NUMBER
            pOpcode.OperandType(1) = EnumOperand.P_LOW_ADDR
            pOpcode.OperandType(2) = EnumOperand.P_NUMBER
            pOpcode.Extra = EnumExtra.E_NONE
            pOpcode.Text = "*** ILLEGAL SET OF PARAMETERS FOR THIS OPCODE, ONLY USED IN ZORK0 DURING DEVELOPMENT ***"
        End If

        If pType = EnumType.T_ILLEGAL Then
            Dim oReturn As New DecodeResult With {.status = EnumStatus.BAD_OPCODE}
            Return oReturn
        End If

        If pOpcode.Code = &HBA Or pOpcode.Code = &HB7 Then
            ' QUIT or RESTART (should this be expanded to other RETURN_types?)
            ' These are considered as RETURN-type if the next opcode is illegal, otherwise they are PLAIN.
            ' If the next address is a valid routine startIndex, it is also considered as RETURN.
            If Not (byteGame(pOpcode.Address + 1) < 16 And Helper.GetNextValidPackedAddress(byteGame, pOpcode.Address + 1) = pOpcode.Address + 1) Then
                Dim bOldSilent As Boolean = bSilent
                bSilent = True
                If DecodeCode(pOpcode.Address + 1).status = EnumStatus.BAD_OPCODE Then
                    pOpcode.Type = EnumType.T_RETURN
                Else
                    pOpcode.Type = EnumType.T_PLAIN
                End If
                bSilent = bOldSilent

            End If
        End If
        Dim iOffset As Integer = DumpOpcode(pOpcode)
        Dim sText As String = psOpcodeNameInf
        If syntax = 1 Then sText = sText.ToLower
        If syntax = 2 Then sText = psOpcodeNameZil
        sText &= pOpcode.OperandText
        If pOpcode.Text.Trim <> "" Then
            sText = sText & " " & Convert.ToChar(34) & pOpcode.Text & Convert.ToChar(34)
        End If
        If Not bSilent Then
            If syntax = 3 Then Console.WriteLine(pOpcode.PseudoCode) Else Console.WriteLine(sText)
            If bShowZCodeVerbose Then pOpcode.PrintVerbose()
        End If

        Dim oRet As New DecodeResult With {.status = EnumStatus.END_OF_INSTRUCTION}
        If pOpcode.Type = EnumType.T_ILLEGAL Then oRet.status = EnumStatus.BAD_OPCODE
        oRet.nextPC = pOpcode.Address + iOffset
        If pOpcode.Type = EnumType.T_RETURN And oRet.nextPC > high_pc Then oRet.status = EnumStatus.END_OF_ROUTINE

        Return oRet
    End Function

    Private Function DumpOpcode(pOpcode As Opcode) As Integer
        Dim count As Integer = 0
        Dim iParameterCount As Integer = 0
        Dim bBranchLong As Boolean = False

        If Not bSilent Then Console.Write("{0:X5} ", pOpcode.Address)

        If pOpcode.OpcodeClass = EnumOpcodeClass.EXTENDED_OPERAND Then
            If Not bSilent Then Console.Write("{0:X2} ", byteGame(pOpcode.Address))
            count += 1
        End If
        If Not bSilent Then Console.Write("{0:X2} ", byteGame(pOpcode.Address + count))
        count += 1

        If pOpcode.OpcodeClass = EnumOpcodeClass.ONE_OPERAND Then
            ' Bit 5, 4 determine operand type
            ' 00 = long immediate (2 bytes)
            ' 01 = immediate (1 byte 00xx)
            ' 10 = variable (1 byte)
            ' 11 = undefined
            iParameterCount = 1
            pOpcode.OperandLen(0) = 1
            If (pOpcode.Code And &H30) = 0 Then
                pOpcode.OperandLen(0) = 2
                pOpcode.OperandAddrMode(0) = EnumAddressMode.LONG_IMMEDIATE
                If Not bSilent Then Console.Write("{0:X2} ", byteGame(pOpcode.Address + count))
                pOpcode.OperandVal(0) = byteGame(pOpcode.Address + count)
                count += 1
            End If
            If Not bSilent Then Console.Write("{0:X2} ", byteGame(pOpcode.Address + count))
            pOpcode.OperandVal(0) = pOpcode.OperandVal(0) * 256 + byteGame(pOpcode.Address + count)
            If (pOpcode.Code And &H30) = &H10 Then pOpcode.OperandAddrMode(0) = EnumAddressMode.IMMEDIATE
            If (pOpcode.Code And &H30) = &H20 Then pOpcode.OperandAddrMode(0) = EnumAddressMode.VARIABLE
            count += 1

            If pOpcode.OperandType(0) = EnumOperand.P_LABEL Then
                ' JUMP
                Dim offset As Integer = Helper.GetAdressFromWord(byteGame, pOpcode.Address + count - 2)
                If offset > 32767 Then offset = (offset And 32767) * -1
                If offset > 0 Then
                    If (pOpcode.Address + count + offset - 2) > high_pc Then
                        high_pc = pOpcode.Address + count + offset - 2
                    End If
                End If
            End If
        End If

        If pOpcode.OpcodeClass = EnumOpcodeClass.TWO_OPERAND Then
            ' Bit 6, 5 determine operand type
            ' 00 = immediate, immediate (2 bytes)
            ' 01 = immediate, variable (2 bytes)
            ' 10 = variable, immediate (2 bytes)
            ' 11 = variable, variable (2 bytes)
            iParameterCount = 2
            pOpcode.OperandLen(0) = 1
            pOpcode.OperandLen(1) = 1
            pOpcode.OperandVal(0) = byteGame(pOpcode.Address + count)
            pOpcode.OperandVal(1) = byteGame(pOpcode.Address + count + 1)
            If (pOpcode.Code And &H60) = &H0 Then
                pOpcode.OperandAddrMode(0) = EnumAddressMode.IMMEDIATE
                pOpcode.OperandAddrMode(1) = EnumAddressMode.IMMEDIATE
            End If
            If (pOpcode.Code And &H60) = &H20 Then
                pOpcode.OperandAddrMode(0) = EnumAddressMode.IMMEDIATE
                pOpcode.OperandAddrMode(1) = EnumAddressMode.VARIABLE
            End If
            If (pOpcode.Code And &H60) = &H40 Then
                pOpcode.OperandAddrMode(0) = EnumAddressMode.VARIABLE
                pOpcode.OperandAddrMode(1) = EnumAddressMode.IMMEDIATE
            End If
            If (pOpcode.Code And &H60) = &H60 Then
                pOpcode.OperandAddrMode(0) = EnumAddressMode.VARIABLE
                pOpcode.OperandAddrMode(1) = EnumAddressMode.VARIABLE
            End If
            If Not bSilent Then Console.Write("{0:X2} ", byteGame(pOpcode.Address + count))
            If Not bSilent Then Console.Write("{0:X2} ", byteGame(pOpcode.Address + count + 1))
            count += 2
        End If

        If pOpcode.OpcodeClass = EnumOpcodeClass.VARIABLE_OPERAND Or pOpcode.OpcodeClass = EnumOpcodeClass.EXTENDED_OPERAND Then
            ' Each pair of bits in next 1 or 2 bytes (XCALL & IXCALL) determine operand type
            ' 00 = long immediate (2 bytes)
            ' 01 = immediate (1 byte 00xx)
            ' 10 = variable (1 byte)
            ' 11 = no more operands
            Dim iOperandBytes As Integer = 0
            If (pOpcode.Code And &H3F) = &H2C Or (pOpcode.Code And &H3F) = &H3A Then
                ' CALL_VS2 & CALL_VN2, next two bytes specify length of data bytes
                If Not bSilent Then Console.Write("{0:X2} ", byteGame(pOpcode.Address + count))
                If Not bSilent Then Console.Write("{0:X2} ", byteGame(pOpcode.Address + count + 1))
                Dim iWord As Integer = Helper.GetAdressFromWord(byteGame, pOpcode.Address + count)
                For shift As Integer = 14 To 0 Step -2
                    Dim iBits As Integer = ((iWord >> shift) And 3)
                    If iBits = 3 Then Exit For
                    iParameterCount += 1
                    If iBits = 0 Then
                        pOpcode.OperandLen(iParameterCount - 1) = 2
                        pOpcode.OperandVal(iParameterCount - 1) = byteGame(pOpcode.Address + count + iOperandBytes + 2) * 256 + byteGame(pOpcode.Address + +count + iOperandBytes + 3)
                        pOpcode.OperandAddrMode(iParameterCount - 1) = EnumAddressMode.LONG_IMMEDIATE
                        iOperandBytes += 2
                    ElseIf iBits = 1 Or iBits = 2 Then
                        pOpcode.OperandLen(iParameterCount - 1) = 1
                        pOpcode.OperandVal(iParameterCount - 1) = byteGame(pOpcode.Address + count + iOperandBytes + 2)
                        If iBits = 1 Then
                            pOpcode.OperandAddrMode(iParameterCount - 1) = EnumAddressMode.IMMEDIATE
                        Else
                            pOpcode.OperandAddrMode(iParameterCount - 1) = EnumAddressMode.VARIABLE
                        End If
                        iOperandBytes += 1
                    End If
                Next
                count += 2
            Else
                ' Next byte specify length of data bytes
                If Not bSilent Then Console.Write("{0:X2} ", byteGame(pOpcode.Address + count))
                Dim iByte As Integer = byteGame(pOpcode.Address + count)
                For shift As Integer = 6 To 0 Step -2
                    Dim iBits As Integer = ((iByte >> shift) And 3)
                    If iBits = 3 Then Exit For
                    iParameterCount += 1
                    If iBits = 0 Then
                        pOpcode.OperandLen(iParameterCount - 1) = 2
                        pOpcode.OperandVal(iParameterCount - 1) = byteGame(pOpcode.Address + count + iOperandBytes + 1) * 256 + byteGame(pOpcode.Address + +count + iOperandBytes + 2)
                        pOpcode.OperandAddrMode(iParameterCount - 1) = EnumAddressMode.LONG_IMMEDIATE
                        iOperandBytes += 2
                    ElseIf iBits = 1 Or iBits = 2 Then
                        pOpcode.OperandLen(iParameterCount - 1) = 1
                        pOpcode.OperandVal(iParameterCount - 1) = byteGame(pOpcode.Address + count + iOperandBytes + 1)
                        If iBits = 1 Then
                            pOpcode.OperandAddrMode(iParameterCount - 1) = EnumAddressMode.IMMEDIATE
                        Else
                            pOpcode.OperandAddrMode(iParameterCount - 1) = EnumAddressMode.VARIABLE
                        End If
                        iOperandBytes += 1
                    End If
                Next
                count += 1
            End If
            For i As Integer = 0 To iOperandBytes - 1
                PrintLF(count + i)
                If Not bSilent Then Console.Write("{0:X2} ", byteGame(pOpcode.Address + count + i))
            Next
            count += iOperandBytes
        End If

        ' Decode Extra
        If pOpcode.Extra = EnumExtra.E_STORE Or pOpcode.Extra = EnumExtra.E_BOTH Then
            ' One byte for variable to store result in
            pOpcode.StoreVal = byteGame(pOpcode.Address + count)
            PrintLF(count)
            If Not bSilent Then Console.Write("{0:X2} ", byteGame(pOpcode.Address + count))
            count += 1
        End If
        If pOpcode.Extra = EnumExtra.E_BRANCH Or pOpcode.Extra = EnumExtra.E_BOTH Then
            ' 1 or 2 bytes for address for branch.
            ' Bit 7 determine if branch on true or false
            '  0 = Branch when false
            '  1 = branch when true
            ' Bit 6 determine branch type
            '  0 = long address (2 bytes, 14 bits for long offset)
            '  1 = short address (1 byte, 6 bits for offset)
            PrintLF(count)
            If Not bSilent Then Console.Write("{0:X2} ", byteGame(pOpcode.Address + count))
            pOpcode.BranchTest = True
            If (byteGame(pOpcode.Address + count) And &H80) = 0 Then pOpcode.BranchTest = False
            Dim offset As Integer = 0
            If (byteGame(pOpcode.Address + count) And 64) = 0 Then
                bBranchLong = True
                PrintLF(count + 1)
                If Not bSilent Then Console.Write("{0:X2} ", byteGame(pOpcode.Address + count + 1))
                offset = (byteGame(pOpcode.Address + count) And &H3F) * 256 + byteGame(pOpcode.Address + count + 1)
                count += 2
            Else
                offset = (byteGame(pOpcode.Address + count) And &H3F)
                count += 1
            End If
            If offset > &H1FFF Then offset = ((offset And &H1FFF) Or (Not &H1FFF))
            pOpcode.BranchAddr = pOpcode.Address + count + offset - 2
            If pOpcode.BranchAddr < startAddress Then pOpcode.Type = EnumType.T_ILLEGAL
            If offset > 1 Then
                If pOpcode.BranchAddr > high_pc Then high_pc = pOpcode.BranchAddr
            End If
            ' offset = 0: RFALSE
            ' offset = 1: RTRUE
            If offset = 0 Or offset = 1 Then pOpcode.BranchAddr = offset
        End If

        If pOpcode.Extra = EnumExtra.E_TEXT Then
            ' z-string. Terminated when bit 15 of word is set.
            Dim textStart As Integer = pOpcode.Address + count
            pOpcode.Text = Helper.ExtractZString(byteGame, textStart, sAbbreviations, alphabet, showAbbrevsInsertion)
            Do
                PrintLF(count)
                If Not bSilent Then Console.Write("{0:X2} ", byteGame(pOpcode.Address + count))
                PrintLF(count + 1)
                If Not bSilent Then Console.Write("{0:X2} ", byteGame(pOpcode.Address + count + 1))
                count += 2
            Loop Until (byteGame(pOpcode.Address + count - 2) And 128) = 128
            If bSilent And ((pOpcode.Code And &HB2) = &HB2 Or (pOpcode.Code And &HB3) = &HB3) Then ' PRINTI (PRINT) or PRINTR (PRINT_RET)
                inlineStrings.Add(New InlineString With {.text = Helper.ExtractZString(byteGame, textStart, sAbbreviations, alphabet, False),
                                                         .size = pOpcode.Address + count - textStart})
            End If
        End If

        If (count Mod 8) = 0 Then
            If Not bSilent Then Console.Write(" ")
        Else
            If Not bSilent Then Console.Write("                                        ".Substring(0, (8 - (count Mod 8)) * 3 + 1))
        End If

        ' Find call to routine with highest address. 
        ' Only counts long_immediate and therefore excludes TWO_OPERAND calls
        If pOpcode.Type = EnumType.T_CALL And pOpcode.OperandType(0) = EnumOperand.P_ROUTINE Then
            Dim routine_address As Integer = 0
            If pOpcode.OpcodeClass = EnumOpcodeClass.ONE_OPERAND Then
                If (pOpcode.Code And &H30) = 0 Then
                    routine_address = Helper.GetAdressFromPacked(byteGame, pOpcode.Address + 1, True)
                End If
            ElseIf Not pOpcode.OpcodeClass = EnumOpcodeClass.TWO_OPERAND Then
                Dim bits As Integer = byteGame(pOpcode.Address + 1) And 192
                ' 00 = long immediate (2 bytes)
                ' 01 = immediate (1 byte 00xx)
                ' 10 = variable (1 byte)
                ' 11 = no more operands
                If bits = 0 Then
                    If (pOpcode.Code And &H3F) = &H2C Or (pOpcode.Code And &H3F) = &H3A Then
                        routine_address = Helper.GetAdressFromPacked(byteGame, pOpcode.Address + 3, True)
                    Else
                        routine_address = Helper.GetAdressFromPacked(byteGame, pOpcode.Address + 2, True)
                    End If
                End If
            End If
            If routine_address > highest_routine Then highest_routine = routine_address
            If routine_address > 0 And routine_address < lowest_routine Then lowest_routine = routine_address
            callsTo.Add(New CallsFromTo With {.fromAddress = startAddress, .toAddress = routine_address})
        End If

        ' Find call to packed string with lowest address
        If pOpcode.OpcodeClass = EnumOpcodeClass.ONE_OPERAND AndAlso pOpcode.Code = &H8D Then
            ' 8D = print_paddr with long immediate
            Dim string_address As Integer = Helper.GetAdressFromPacked(byteGame, pOpcode.Address + 1, False)
            If string_address < lowest_string Then lowest_string = string_address
        End If

        ' Format operands/parameter text
        For i As Integer = 0 To iParameterCount - 1
            Dim operandType As EnumOperand = pOpcode.OperandType(i)
            Dim operandVal As Integer = pOpcode.OperandVal(i)
            If i > 3 Then
                operandType = EnumOperand.P_ANYTHING
                pOpcode.OperandType(i) = EnumOperand.P_ANYTHING
            End If
            If pOpcode.OperandAddrMode(i) = EnumAddressMode.VARIABLE Then operandType = EnumOperand.P_VAR
            If iParameterCount > 1 And pOpcode.Type = EnumType.T_CALL And syntax < 2 And i = 1 Then pOpcode.OperandText &= " ("

            ' Comment from txd
            ' To make the code more readable, VAR type operands are Not translated
            ' as constants, eg. INC 5 Is actually printed as INC L05. However, if
            ' the VAR type operand _is_ a variable, the translation should look Like
            ' INC [L05], ie. increment the variable which Is given by the contents
            ' of local variable #5. Earlier versions of "txd" translated both cases
            ' as INC L05. This bug was finally detected by Graham Nelson.
            If pOpcode.OperandType(i) = EnumOperand.P_VAR And pOpcode.OperandAddrMode(i) = EnumAddressMode.VARIABLE Then
                operandType = EnumOperand.P_INDIRECT
                pOpcode.OperandType(i) = operandType
            End If

            Select Case operandType
                Case EnumOperand.P_ANYTHING
                    Dim oStringData As StringData = validStringList.Find(Function(x) x.entryPointPacked = operandVal)
                    Dim dictEntry As DictionaryEntry = DictEntriesList.GetEntryAtAddress(operandVal)
                    If oStringData IsNot Nothing Then
                        If syntax = 1 Then
                            pOpcode.OperandText = pOpcode.OperandText & " s" & oStringData.number.ToString("D4")
                        Else
                            pOpcode.OperandText = pOpcode.OperandText & " S" & oStringData.number.ToString("D4")
                        End If
                        pOpcode.OperandStringPseudo(i) = "s" & oStringData.number.ToString("D4")
                    End If
                    If oStringData IsNot Nothing And dictEntry IsNot Nothing Then
                        If syntax = 1 Then
                            pOpcode.OperandText &= " or"
                        Else
                            pOpcode.OperandText &= " OR"
                        End If
                        pOpcode.OperandStringPseudo(i) &= "/"
                    End If
                    If dictEntry IsNot Nothing Then
                        If syntax = 2 Then
                            pOpcode.OperandText = pOpcode.OperandText & " W?" & dictEntry.dictWord.ToUpper
                        Else
                            pOpcode.OperandText = pOpcode.OperandText & " " & Convert.ToChar(34) & dictEntry.dictWord & Convert.ToChar(34)
                        End If
                        pOpcode.OperandStringPseudo(i) &= Convert.ToChar(34) & dictEntry.dictWord & Convert.ToChar(34)
                    End If
                    If oStringData Is Nothing And dictEntry Is Nothing Then
                        pOpcode.OperandText = pOpcode.OperandText & " " & TextNumber(operandVal, pOpcode.OperandLen(i), syntax, True)
                        pOpcode.OperandStringPseudo(i) = TextNumber(operandVal, pOpcode.OperandLen(i), 1, True)
                    End If
                Case EnumOperand.P_ATTRNUM
                    If syntax = 0 Then pOpcode.OperandText = pOpcode.OperandText & " ATTRIBUTE" & operandVal.ToString
                    If syntax = 1 Then pOpcode.OperandText = pOpcode.OperandText & " attribute" & operandVal.ToString
                    If syntax = 2 Then pOpcode.OperandText = pOpcode.OperandText & " ATTRIBUTE" & operandVal.ToString
                    pOpcode.OperandStringPseudo(i) = "attribute" & operandVal.ToString
                Case EnumOperand.P_INDIRECT
                    pOpcode.OperandText = pOpcode.OperandText & " [" & TextVariable(operandVal, syntax) & "]"
                    pOpcode.OperandStringPseudo(i) = " [" & TextVariable(operandVal, 4) & "]"
                Case EnumOperand.P_LABEL
                    Dim Address As Integer = pOpcode.Address + 1
                    If operandVal > 32767 Then
                        Address -= (65536 - operandVal)
                    Else
                        Address += operandVal
                    End If
                    If syntax = 0 Then pOpcode.OperandText = pOpcode.OperandText & " 0x" & Address.ToString("X4")
                    If syntax = 1 Then pOpcode.OperandText = pOpcode.OperandText & " 0x" & Address.ToString("x4")
                    If syntax = 2 Then pOpcode.OperandText = pOpcode.OperandText & " 0x" & Address.ToString("X4")
                    pOpcode.OperandStringPseudo(i) = "0x" & Address.ToString("x4")
                Case EnumOperand.P_LOW_ADDR
                    Dim dictEntry As DictionaryEntry = DictEntriesList.GetEntryAtAddress(operandVal)
                    If dictEntry IsNot Nothing Then
                        pOpcode.OperandText = pOpcode.OperandText & " " & Convert.ToChar(34) & dictEntry.dictWord & Convert.ToChar(34)
                        pOpcode.OperandStringPseudo(i) = Convert.ToChar(34) & dictEntry.dictWord & Convert.ToChar(34)
                    Else
                        pOpcode.OperandText = pOpcode.OperandText & " " & TextNumber(operandVal, pOpcode.OperandLen(i), syntax, False)
                        pOpcode.OperandStringPseudo(i) = TextNumber(operandVal, pOpcode.OperandLen(i), 1, False)
                        If bSilent Then
                            ' Collect all possible startpoints for an array
                            If operandVal > 0 Then
                                If (pOpcode.OpcodeClass = EnumOpcodeClass.TWO_OPERAND And (pOpcode.Code And &H1F) = &HF) Or             ' LOADB
                                   (pOpcode.OpcodeClass = EnumOpcodeClass.TWO_OPERAND And (pOpcode.Code And &H1F) = &H10) Or            ' LOADW
                                   (pOpcode.OpcodeClass = EnumOpcodeClass.VARIABLE_OPERAND And (pOpcode.Code And &H3F) = &H21) Or       ' STOREB
                                   (pOpcode.OpcodeClass = EnumOpcodeClass.VARIABLE_OPERAND And (pOpcode.Code And &H3F) = &H22) Then     ' STOREW
                                    arraysStart.Add(operandVal)
                                End If
                            End If
                        End If
                    End If
                Case EnumOperand.P_NIL
                    pOpcode.OperandText &= " Illegal_parameter"
                    pOpcode.OperandStringPseudo(i) = "Illegal_parameter"
                Case EnumOperand.P_NUMBER
                    If syntax = 2 And (pOpcode.Code And &H3F) = &H3F Then       ' ASSIGNED?, the number points to a local variable
                        pOpcode.OperandText = pOpcode.OperandText & " '" & TextVariable(operandVal, syntax)
                    Else
                        pOpcode.OperandText = pOpcode.OperandText & " " & TextNumber(operandVal, pOpcode.OperandLen(i), syntax, True)
                    End If
                    pOpcode.OperandStringPseudo(i) = TextNumber(operandVal, pOpcode.OperandLen(i), 2, True)
                Case EnumOperand.P_OBJECT
                    If syntax = 0 Then pOpcode.OperandText = pOpcode.OperandText & " OBJECT" & operandVal.ToString
                    If syntax = 1 Then pOpcode.OperandText = pOpcode.OperandText & " object" & operandVal.ToString
                    If syntax = 2 Then pOpcode.OperandText = pOpcode.OperandText & " OBJECT" & operandVal.ToString
                    pOpcode.OperandStringPseudo(i) = "object" & operandVal.ToString
                Case EnumOperand.P_PATTR
                    Dim sDirOut As String = ""
                    Select Case operandVal
                        Case 1 : sDirOut = "OUTPUT_ENABLE"
                        Case 2 : sDirOut = "SCRIPTING_ENABLE"
                        Case 3 : sDirOut = "REDIRECT_ENABLE"
                        Case 4 : sDirOut = "RECORD_ENABLE"
                        Case -1 : sDirOut = "OUTPUT_DISABLE"
                        Case -2 : sDirOut = "SCRIPTING_DISABLE"
                        Case -3 : sDirOut = "REDIRECT_DISABLE"
                        Case -4 : sDirOut = "RECORD_DISABLE"
                        Case Else : sDirOut = TextNumber(operandVal, pOpcode.OperandLen(i), syntax, False)
                    End Select
                    If syntax = 1 Then sDirOut = sDirOut.ToLower
                    pOpcode.OperandText = pOpcode.OperandText & " " & sDirOut
                    pOpcode.OperandStringPseudo(i) = sDirOut.ToLower
                Case EnumOperand.P_PCHAR
                    Dim character As Char = CChar(Char.ConvertFromUtf32(operandVal))
                    If Not Char.IsControl(character) Then
                        pOpcode.OperandText = pOpcode.OperandText & " '" & character & "'"
                        pOpcode.OperandStringPseudo(i) = "'" & character & "'"
                    Else
                        pOpcode.OperandText = pOpcode.OperandText & " " & TextNumber(operandVal, pOpcode.OperandLen(i), syntax, False)
                        pOpcode.OperandStringPseudo(i) = TextNumber(operandVal, pOpcode.OperandLen(i), 1, False)
                    End If
                Case EnumOperand.P_PROPNUM
                    If operandVal < propertyMin Or operandVal > propertyMax Then
                        pOpcode.OperandText = pOpcode.OperandText & " " & "[Invalid property: 0x" & operandVal.ToString("X2") & "]"
                        pOpcode.OperandStringPseudo(i) = "[Invalid property: 0x" & operandVal.ToString("X2") & "]"
                    Else
                        If syntax = 0 Then pOpcode.OperandText = pOpcode.OperandText & " PROPERTY" & operandVal.ToString
                        If syntax = 1 Then pOpcode.OperandText = pOpcode.OperandText & " property" & operandVal.ToString
                        If syntax = 2 Then pOpcode.OperandText = pOpcode.OperandText & " P?" & operandVal.ToString
                        pOpcode.OperandStringPseudo(i) = "property" & operandVal.ToString
                    End If
                Case EnumOperand.P_ROUTINE
                    If operandVal <> 0 Then
                        Dim oRoutineData As RoutineData = validRoutineList.Find(Function(x) x.entryPointPacked = operandVal)
                        If oRoutineData IsNot Nothing Then
                            If syntax = 0 Then pOpcode.OperandText = pOpcode.OperandText & " 0x" & oRoutineData.entryPoint.ToString("X4")
                            If syntax = 1 Then pOpcode.OperandText = pOpcode.OperandText & " 0x" & oRoutineData.entryPoint.ToString("x4")
                            If syntax = 2 Then pOpcode.OperandText = pOpcode.OperandText & " 0x" & oRoutineData.entryPoint.ToString("X4")
                            pOpcode.OperandStringPseudo(i) = "0x" & oRoutineData.entryPoint.ToString("x4")
                        Else
                            pOpcode.OperandText = pOpcode.OperandText & " " & "[Invalid routine: 0x" & Helper.UnpackAddress(operandVal, byteGame, True).ToString("X4") & "]"
                            pOpcode.OperandStringPseudo(i) = "[Invalid routine: 0x" & Helper.UnpackAddress(operandVal, byteGame, True).ToString("X4") & "]"
                        End If
                    Else
                        pOpcode.OperandText &= " 0"
                        pOpcode.OperandStringPseudo(i) = "0"
                    End If
                Case EnumOperand.P_STATIC
                    Dim oStringData As StringData = validStringList.Find(Function(x) x.entryPointPacked = operandVal)
                    If oStringData IsNot Nothing Then
                        If syntax = 1 Then
                            pOpcode.OperandText = pOpcode.OperandText & " s" & oStringData.number.ToString("D4") & " " & Convert.ToChar(34) & oStringData.GetText(showAbbrevsInsertion) & Convert.ToChar(34)
                        Else
                            pOpcode.OperandText = pOpcode.OperandText & " S" & oStringData.number.ToString("D4") & " " & Convert.ToChar(34) & oStringData.GetText(showAbbrevsInsertion) & Convert.ToChar(34)
                        End If
                        pOpcode.OperandStringPseudo(i) = "s" & oStringData.number.ToString("D4") & " " & Convert.ToChar(34) & oStringData.GetText(showAbbrevsInsertion) & Convert.ToChar(34)
                    Else
                        pOpcode.OperandText = pOpcode.OperandText & " " & "[Invalid string: " & Helper.UnpackAddress(operandVal, byteGame, False).ToString("X5") & "]"
                        pOpcode.OperandStringPseudo(i) = "[Invalid string: " & Helper.UnpackAddress(operandVal, byteGame, False).ToString("X5") & "]"
                    End If
                Case EnumOperand.P_VAR
                    If syntax < 2 Then
                        pOpcode.OperandText = pOpcode.OperandText & " " & TextVariable(operandVal, syntax)
                    Else
                        If pOpcode.OperandType(i) = EnumOperand.P_VAR Then                        ' Original type was P_VAR
                            If (pOpcode.Code And &HF) = &HE And operandVal = 0 Then            ' VALUE and STACK
                                pOpcode.OperandText = pOpcode.OperandText & " " & TextVariable(operandVal, syntax)
                            Else
                                pOpcode.OperandText = pOpcode.OperandText & " '" & TextVariable(operandVal, syntax)
                            End If
                        Else
                            pOpcode.OperandText = pOpcode.OperandText & " " & TextVariable(operandVal, syntax)
                        End If
                    End If
                    pOpcode.OperandStringPseudo(i) = TextVariable(operandVal, 3)
                Case EnumOperand.P_VATTR
                    Dim sFontStyle As String = ""
                    Select Case operandVal
                        Case 0 : sFontStyle = "ROMAN"
                        Case 1 : sFontStyle = "REVERSE"
                        Case 2 : sFontStyle = "BOLDFACE"
                        Case 4 : sFontStyle = "EMPHASIS"
                        Case 8 : sFontStyle = "FIXED_FONT"
                        Case Else : sFontStyle = TextNumber(operandVal, pOpcode.OperandLen(i), syntax, False)
                    End Select
                    If syntax = 1 Then sFontStyle = sFontStyle.ToLower
                    pOpcode.OperandText = pOpcode.OperandText & " " & sFontStyle
                    pOpcode.OperandStringPseudo(i) = sFontStyle.ToLower
            End Select
            If syntax < 2 And iParameterCount > 1 And pOpcode.Type = EnumType.T_CALL And i < iParameterCount - 1 And i > 0 Then pOpcode.OperandText &= ","
            If syntax = 2 And iParameterCount > 0 And i < iParameterCount - 1 Then pOpcode.OperandText &= ","
        Next
        If iParameterCount > 1 And pOpcode.Type = EnumType.T_CALL And syntax < 2 Then
            pOpcode.OperandText = (pOpcode.OperandText & ")").Replace("( ", "(").Replace(", ", ",")
        End If
        If syntax = 2 Then pOpcode.OperandText = pOpcode.OperandText.Replace(", ", ",")
        If pOpcode.Extra = EnumExtra.E_BOTH Or pOpcode.Extra = EnumExtra.E_STORE Then
            Dim sStoreText As String
            sStoreText = TextVariable(pOpcode.StoreVal, syntax)
            If pOpcode.StoreVal = 0 And syntax = 0 Then sStoreText = "-(SP)"
            pOpcode.StoreStringPseudo = TextVariable(pOpcode.StoreVal, 3)

            ' Collect statistics on globals
            Dim value As Integer = pOpcode.StoreVal - 16
            If value > highest_global Then highest_global = value
            usedGlobals.Add(value)

            If syntax < 2 Then pOpcode.OperandText = pOpcode.OperandText & " -> " & sStoreText Else pOpcode.OperandText = pOpcode.OperandText & " >" & sStoreText
        End If
        If pOpcode.Extra = EnumExtra.E_BOTH Or pOpcode.Extra = EnumExtra.E_BRANCH Then
            Dim sBranchText As String = ""
            If pOpcode.BranchTest Then
                If syntax = 0 Then sBranchText = "[TRUE] "
                If syntax = 2 Then sBranchText = "/"
            Else
                If syntax = 0 Then sBranchText = "[FALSE] "
                If syntax = 1 Then sBranchText = "~"
                If syntax = 2 Then sBranchText = "\"
            End If
            If pOpcode.BranchAddr = 0 Then
                If syntax = 2 Then sBranchText &= "FALSE" Else sBranchText &= "RFALSE"
            ElseIf pOpcode.BranchAddr = 1 Then
                If syntax = 2 Then sBranchText &= "TRUE" Else sBranchText &= "RTRUE"
            Else
                sBranchText = sBranchText & "0x" & pOpcode.BranchAddr.ToString("X4")
            End If
            If syntax = 1 Then sBranchText = sBranchText.ToLower
            pOpcode.OperandText = pOpcode.OperandText & " " & sBranchText
        End If

        Return count
    End Function

    Private Function TextVariable(pVariable As Integer, toSyntax As Integer) As String
        If pVariable = 0 Then
            If toSyntax = 0 Then Return "(SP)+"
            If toSyntax = 1 Then Return "sp"
            If toSyntax = 2 Then Return "STACK"
            If toSyntax = 3 Then Return "sp"
        End If
        If pVariable < 16 Then
            If toSyntax = 0 Then Return "L" & (pVariable - 1).ToString("X2")
            If toSyntax = 1 Then Return "local" & (pVariable - 1).ToString
            If toSyntax = 2 Then Return "L" & (pVariable - 1).ToString
            If toSyntax = 3 Then Return "local" & (pVariable - 1).ToString
        Else
        End If
        If pVariable < 256 Then
            Dim value As Integer = pVariable - 16
            If value > highest_global Then highest_global = value
            usedGlobals.Add(value)
            If toSyntax = 0 Then Return "G" & value.ToString("X2")
            If toSyntax = 1 Then Return "g" & value.ToString
            If toSyntax = 2 Then Return "G" & value.ToString
            If toSyntax = 3 Then Return "g" & value.ToString
        End If
        Return "[Invalid variable: 0x" & pVariable.ToString("X2") & "]"
    End Function

    Private Function TextNumber(pNumber As Integer, pLength As Integer, toSyntax As Integer, toSigned As Boolean) As String
        If pLength = 1 Or (toSigned And syntax = 2) Then
            Dim signedInt As Integer
            If pNumber < 32768 Then
                signedInt = pNumber
            Else
                signedInt = -1 * (65536 - pNumber)
            End If
            If toSyntax = 0 Then Return "0x" & pNumber.ToString("X2")
            If toSyntax = 1 Then Return "0x" & pNumber.ToString("x2")
            If toSyntax = 2 Then
                If toSigned Then Return signedInt.ToString Else Return pNumber.ToString
            End If
        Else
            If toSyntax = 0 Then Return "0x" & pNumber.ToString("X4")
            If toSyntax = 1 Then Return "0x" & pNumber.ToString("x4")
            If toSyntax = 2 Then Return pNumber.ToString
        End If
        Return pNumber.ToString
    End Function

    Private Sub PrintLF(count As Integer)
        If (count Mod 8) = 0 Then
            If Not bSilent Then Console.WriteLine()
            If Not bSilent Then Console.Write("      ")
        End If
    End Sub
End Class
