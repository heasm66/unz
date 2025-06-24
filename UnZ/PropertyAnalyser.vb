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

Public Enum PropertyType
    ZIL_DIRECTION
    ZIL_SYNONYM
    ZIL_ADJECTIVE
    ZIL_THINGS
    ZIL_PSEUDO
    ZIL_GLOBAL
    ZIL_ACTION
    ZIL_HIGH_STRING
    ZIL_STRING
    ZIL_VALUE
    ZIL_TABLE
    ZIL_UNKNOWN
    UNINITIATED
    UNUSED
    AMBIGUITY
End Enum

Public Class PropertyAnalyser
    Public addressObjectTreeStart As Integer = 0
    Public addressObjectTreeEnd As Integer = 0
    Public objectTreeEntryLength As Integer = 0
    Public objectCount As Integer = 0
    Public addressObjectPropTableStart As Integer = 0
    Public addressObjectPropTableEnd As Integer = 0
    Public propertyNumberMax As Integer = 0
    Public propertyNumberMin As Integer = 0

    Private ReadOnly properties As New List(Of PropertyEntry)
    Private compilerSource As EnumCompilerSource
    Private ZVersion As Integer = 0

    Private Class PropertyData
        Public parentObject As Integer
        Public data As New List(Of Byte)
    End Class

    Private Class PropertyEntry
        Public number As Integer = 0
        Public data As New List(Of PropertyData)

        Private _type As PropertyType = PropertyType.UNINITIATED

        Public ReadOnly Property PropertyType As PropertyType
            Get
                Return _type
            End Get
        End Property

        Private ReadOnly _ambiguityTypeList As New List(Of PropertyType)
        Public ReadOnly Property AmbiguityTypes() As List(Of PropertyType)
            Get
                Return _ambiguityTypeList
            End Get
        End Property

        Public Sub AddPropertyType(pPropertyType As PropertyType)
            If _type = PropertyType.UNINITIATED Then
                _type = pPropertyType
            Else
                _type = PropertyType.AMBIGUITY
            End If
            _ambiguityTypeList.Add(pPropertyType)
        End Sub
    End Class

    Private Sub AddProperty(pObjectNumber As Integer, pPropertyNumber As Integer, pData As List(Of Byte))
        Dim entry As PropertyEntry = properties.Find(Function(x) x.number = pPropertyNumber)
        If entry Is Nothing Then
            entry = New PropertyEntry
            properties.Add(entry)
        End If

        entry.number = pPropertyNumber
        Dim propertyData = New PropertyData With {.parentObject = pObjectNumber, .data = pData}
        entry.data.Add(propertyData)
    End Sub

    Public Sub Init(pStoryData() As Byte, pAddrObjectTreeStart As Integer, pZVersion As Integer, pCompilerSource As EnumCompilerSource)
        ' Loop through all properties tables and collect them according to property number for later analysis.

        compilerSource = pCompilerSource
        ZVersion = pZVersion

        objectTreeEntryLength = 14
        If pZVersion <= 3 Then objectTreeEntryLength = 9

        ' Loop through all pointers to property data to find lowest (=start of property data tables)
        Dim lowestPropAddress As Integer = Helper.GetAdressFromWord(pStoryData, pAddrObjectTreeStart + objectTreeEntryLength - 2)
        Dim i As Integer = 1
        Do
            Dim currentPropAddress As Integer = Helper.GetAdressFromWord(pStoryData, pAddrObjectTreeStart + objectTreeEntryLength * i - 2)
            If currentPropAddress < pAddrObjectTreeStart + objectTreeEntryLength * i Then Exit Do           ' Ill formed table
            If currentPropAddress < lowestPropAddress Then lowestPropAddress = currentPropAddress
            i += 1
        Loop Until pAddrObjectTreeStart + objectTreeEntryLength * i - 2 >= lowestPropAddress
        objectCount = i - 1
        ' Object properties tables follow directly after so the address of the first objects properties divided by the entry-length gives the number of objects
        'objectCount = ((pStoryData(pAddrObjectTreeStart + objectTreeEntryLength - 2) * 256 + pStoryData(pAddrObjectTreeStart + objectTreeEntryLength - 1)) - pAddrObjectTreeStart) / objectTreeEntryLength
        addressObjectTreeEnd = pAddrObjectTreeStart + objectCount * objectTreeEntryLength - 1
        'addressObjectPropTableStart = addressObjectTreeEnd + 1
        addressObjectPropTableStart = lowestPropAddress

        Dim iAddrObjectProperties As Integer = 65536
        Dim iAddrProperties As Integer
        For i = 0 To objectCount - 1
            Dim iObject As Integer = i + 1
            iAddrProperties = pStoryData(pAddrObjectTreeStart + iObject * objectTreeEntryLength - 2) * 256 + pStoryData(pAddrObjectTreeStart + iObject * objectTreeEntryLength - 1)
            If iAddrProperties < iAddrObjectProperties Then iAddrObjectProperties = iAddrProperties
            Dim iPropDescLen As Integer = pStoryData(iAddrProperties)
            Dim iTmpProp As Integer = iAddrProperties + 2 * iPropDescLen + 1
            If iPropDescLen > 0 Then
                Do While pStoryData(iTmpProp) <> 0
                    Dim iPropNum As Integer
                    Dim iPropSize As Integer
                    Dim iPropStart As Integer = iTmpProp + 1
                    If pZVersion <= 3 Then
                        iPropNum = (pStoryData(iTmpProp) And &H1F)
                        iPropSize = 1 + (pStoryData(iTmpProp) And 224) \ 32
                    Else
                        iPropNum = (pStoryData(iTmpProp) And &H3F)
                        If (pStoryData(iTmpProp) And &H80) = &H80 Then
                            iPropSize = (pStoryData(iTmpProp + 1) And &H3F)
                            If iPropSize = 0 Then iPropSize = &H40
                            iPropStart += 1
                        Else
                            If (pStoryData(iTmpProp) And 64) = 64 Then iPropSize = 2 Else iPropSize = 1
                        End If
                    End If

                    Dim propData As New List(Of Byte)
                    For j As Integer = 0 To iPropSize + iPropStart - iTmpProp - 1
                        If j >= (iPropStart - iTmpProp) Then propData.Add(pStoryData(iTmpProp + j))
                    Next
                    iTmpProp = iPropStart + iPropSize
                    Me.AddProperty(iObject, iPropNum, propData)
                    If iPropNum < propertyNumberMin Then propertyNumberMin = iPropNum
                    If iPropNum > propertyNumberMax Then propertyNumberMax = iPropNum
                Loop
            End If
            If iTmpProp > addressObjectPropTableEnd Then addressObjectPropTableEnd = iTmpProp
        Next
    End Sub

    Private dictionary As DictionaryEntries = Nothing
    Private routineList As List(Of RoutineData) = Nothing
    Private stringList As List(Of StringData) = Nothing
    Private MemoryMap As List(Of MemoryMapEntry) = Nothing

    Public Sub Analyse(pDictionary As DictionaryEntries, pStringList As List(Of StringData), pRoutineList As List(Of RoutineData), pMemoryMap As List(Of MemoryMapEntry))
        dictionary = pDictionary
        routineList = pRoutineList
        stringList = pStringList
        MemoryMap = pMemoryMap

        If {EnumCompilerSource.ZILCH, EnumCompilerSource.ZILF}.Contains(compilerSource) Then
            For Each propEntry As PropertyEntry In properties
                If IdentifyZILDirection(propEntry) Then propEntry.AddPropertyType(PropertyType.ZIL_DIRECTION)
                If IdentifyZILAction(propEntry) Then propEntry.AddPropertyType(PropertyType.ZIL_ACTION)
                If IdentifyZILAdjective(propEntry) Then propEntry.AddPropertyType(PropertyType.ZIL_ADJECTIVE)
                If IdentifyZILSynonym(propEntry) Then propEntry.AddPropertyType(PropertyType.ZIL_SYNONYM)
                If IdentifyZILHighString(propEntry) Then propEntry.AddPropertyType(PropertyType.ZIL_HIGH_STRING)
                If IdentifyZILPseudo(propEntry) Then propEntry.AddPropertyType(PropertyType.ZIL_PSEUDO)
                If IdentifyZILGlobal(propEntry) Then propEntry.AddPropertyType(PropertyType.ZIL_GLOBAL)
                If IdentifyZILValue(propEntry) Then propEntry.AddPropertyType(PropertyType.ZIL_VALUE)
                If IdentifyZILTable(propEntry) Then propEntry.AddPropertyType(PropertyType.ZIL_TABLE)
                If propEntry.PropertyType = PropertyType.UNINITIATED Then propEntry.AddPropertyType(PropertyType.ZIL_UNKNOWN)
            Next
        ElseIf {EnumCompilerSource.INFORM5, EnumCompilerSource.INFORM6}.Contains(compilerSource) Then
        Else

        End If
    End Sub

    Private Function IdentifyZILDirection(pPropEntry As PropertyEntry) As Boolean
        If dictionary.GetDirection(pPropEntry.number).Count = 0 Then
            Return False
        Else
            For Each oPropData As PropertyData In pPropEntry.data
                If oPropData.data.Count < 2 And ZVersion > 3 Then Return False
                If oPropData.data.Count > 6 And ZVersion > 3 Then Return False
                If oPropData.data.Count > 5 And ZVersion < 4 Then Return False
            Next
        End If
        Return True
    End Function

    Private Function IdentifyZILSynonym(pPropEntry As PropertyEntry) As Boolean
        ' Length mod 2 = 0, Check if all are words
        For Each oPropData As PropertyData In pPropEntry.data
            If Not (oPropData.data.Count Mod 2) = 0 Then Return False
            For i As Integer = 0 To oPropData.data.Count - 2 Step 2
                If dictionary.GetEntryAtAddress(oPropData.data(i) * 256 + oPropData.data(i + 1)) Is Nothing Then Return False
            Next
        Next
        Return True
    End Function

    Private Function IdentifyZILAdjective(pPropEntry As PropertyEntry) As Boolean
        ' Version 1-3: List of adjective numbers (from dictionary)
        '              Adjectives are numbered from 255 downward
        ' Version 4- : List of dictionary words, all with wordtype = adjective
        If ZVersion < 4 Then
            For Each oPropData As PropertyData In pPropEntry.data
                For i As Integer = 0 To oPropData.data.Count - 1
                    If oPropData.data(i) = 0 Then Return False
                    If oPropData.data(i) < (256 - dictionary.AdjectiveCount) Then Return False
                Next
            Next
            Return True
        Else
            For Each oPropData As PropertyData In pPropEntry.data
                If Not (oPropData.data.Count Mod 2) = 0 Then Return False
                For i As Integer = 0 To oPropData.data.Count - 2 Step 2
                    Dim oWordEntry As DictionaryEntry = dictionary.GetEntryAtAddress(oPropData.data(i) * 256 + oPropData.data(i + 1))
                    If oWordEntry Is Nothing Then Return False
                    If (oWordEntry.Flags And &H20) = 0 Then Return False
                Next
            Next
            Return True
        End If
    End Function

    Private Function IdentifyZILAction(pPropEntry As PropertyEntry) As Boolean
        ' Length = 2, Check if all are valid routines
        ' 0 is valid routine IF there are other routines referenced by this property
        Dim bFound As Boolean = False
        For Each oPropData As PropertyData In pPropEntry.data
            If Not oPropData.data.Count = 2 Then Return False
            Dim iAddrRoutine As Integer = oPropData.data(0) * 256 + oPropData.data(1)
            If iAddrRoutine = 0 And Not bFound Then Return False
            If iAddrRoutine > 0 And routineList.Find(Function(x) x.entryPointPacked = iAddrRoutine) Is Nothing Then Return False
            bFound = True
        Next
        Return True
    End Function

    Private Function IdentifyZILHighString(pPropEntry As PropertyEntry) As Boolean
        ' Length = 2, Check if all are valid high strings
        ' 0 is valid string IF there are other strings referenced by this property
        Dim bFound As Boolean = False
        For Each oPropData As PropertyData In pPropEntry.data
            If Not oPropData.data.Count = 2 Then Return False
            Dim iAddrString As Integer = oPropData.data(0) * 256 + oPropData.data(1)
            If iAddrString = 0 And Not bFound Then Return False
            If iAddrString > 0 And stringList.Find(Function(x) x.entryPointPacked = iAddrString) Is Nothing Then Return False
            bFound = True
        Next
        Return True
    End Function

    Private Function IdentifyZILPseudo(pPropEntry As PropertyEntry) As Boolean
        ' Length mod 4 = 0, Check if all follows pattern WORD ROUTINE
        For Each oPropData As PropertyData In pPropEntry.data
            If Not (oPropData.data.Count Mod 4) = 0 Then Return False
            For i As Integer = 0 To oPropData.data.Count - 4 Step 4
                Dim iAddrWord As Integer = oPropData.data(i) * 256 + oPropData.data(i + 1)
                Dim iAddrRoutine As Integer = oPropData.data(i + 2) * 256 + oPropData.data(i + 3)
                If dictionary.GetEntryAtAddress(iAddrWord) Is Nothing Then Return False
                If routineList.Find(Function(x) x.entryPointPacked = iAddrRoutine) Is Nothing Then Return False
            Next
        Next
        Return True
    End Function

    Private Function IdentifyZILGlobal(pPropEntry As PropertyEntry) As Boolean
        ' Version 1-3: List of object numbers (one byte). Object 0 doesn't exists
        ' Version 4- : List of object numbers (two bytes). Object 0 doesn't exists
        ' The property should also be of different length between objects
        Dim bSameSize As Boolean = True
        For Each oPropData As PropertyData In pPropEntry.data
            If oPropData.data.Count <> pPropEntry.data.First.data.Count Then
                bSameSize = False
                Exit For
            End If
        Next
        If bSameSize Then Return False
        If ZVersion < 4 Then
            For Each oPropData As PropertyData In pPropEntry.data
                For i As Integer = 0 To oPropData.data.Count - 1
                    If oPropData.data(i) = 0 Then Return False
                    If oPropData.data(i) > objectCount Then Return False
                Next
            Next
            Return True
        Else
            For Each oPropData As PropertyData In pPropEntry.data
                If Not (oPropData.data.Count Mod 2) = 0 Then Return False
                For i As Integer = 0 To oPropData.data.Count - 2 Step 2
                    Dim objectNumber As Integer = oPropData.data(i) * 256 + oPropData.data(i + 1)
                    If objectNumber = 0 Then Return False
                    If objectNumber > objectCount Then Return False
                Next
            Next
            Return True
        End If
    End Function

    Private Shared Function IdentifyZILValue(pPropEntry As PropertyEntry) As Boolean
        ' Property length always = 2 and value inside -255-255 are identified as a value
        For Each oPropData As PropertyData In pPropEntry.data
            If Not oPropData.data.Count = 2 Then Return False
            If oPropData.data(0) > 0 And oPropData.data(0) < 255 Then Return False
        Next
        Return True
    End Function

    Private Function IdentifyZILTable(pPropEntry As PropertyEntry) As Boolean
        ' Property length always = 2 and value point to area in unidentified data
        For Each oPropData As PropertyData In pPropEntry.data
            If Not oPropData.data.Count = 2 Then Return False
            Dim iAddress As Integer = oPropData.data(0) * 256 + oPropData.data(1)
            If MemoryMap.Find(Function(x) x.addressStart <= iAddress And x.addressEnd >= iAddress And x.type = MemoryMapType.MM_UNIDENTIFIED_DATA) Is Nothing Then Return False
        Next
        Return True
    End Function

    Public Function GetPropertyType(pNumber As Integer) As PropertyType
        Dim propEntry As PropertyEntry = properties.Find(Function(x) x.number = pNumber)
        If propEntry Is Nothing Then Return PropertyType.UNUSED
        Return propEntry.PropertyType
    End Function

    Public Function GetPropertyAmbiguityTypes(pNumber As Integer) As List(Of PropertyType)
        Dim propEntry As PropertyEntry = properties.Find(Function(x) x.number = pNumber)
        If propEntry Is Nothing Then Return New List(Of PropertyType)
        Return propEntry.AmbiguityTypes
    End Function

    Public Shared Function GetPropertyTypeName(pPropertyType As PropertyType) As String
        Select Case pPropertyType
            Case PropertyType.UNINITIATED : Return "Not analysed"
            Case PropertyType.ZIL_ACTION : Return "ACTION"
            Case PropertyType.ZIL_ADJECTIVE : Return "ADJECTIVE"
            Case PropertyType.ZIL_DIRECTION : Return "DIRECTION"
            Case PropertyType.ZIL_GLOBAL : Return "GLOBAL"
            Case PropertyType.ZIL_PSEUDO : Return "PSEUDO"
            Case PropertyType.ZIL_SYNONYM : Return "SYNONYM"
            Case PropertyType.ZIL_HIGH_STRING : Return "HIGH STRING"
            Case PropertyType.ZIL_STRING : Return "STRING"
            Case PropertyType.ZIL_THINGS : Return "THINGS"
            Case PropertyType.ZIL_UNKNOWN : Return "UNKNOWN"
            Case PropertyType.ZIL_VALUE : Return "VALUE"
            Case PropertyType.ZIL_TABLE : Return "TABLE"
            Case PropertyType.AMBIGUITY : Return "AMBIGUITY"
            Case Else : Return "UNIDENTIFIED"
        End Select
    End Function

    Public ReadOnly Property Count As Integer
        Get
            Return properties.Count
        End Get
    End Property

    'Private ZilPropDirectionList As New List(Of Integer)

    'Private Sub DecodePropertyData(pStoryData() As Byte, pDictEntries As DictionaryEntries, pStringsList As List(Of StringData), pRoutineList As List(Of RoutineData), pPropNum As Integer, pPropSize As Integer, pPropStart As Integer, pPropData As String)
    '        Else
    '            If pDictEntries.GetDirection(pPropNum).Count > 0 Then
    '                ZilPropDirectionList.Add(pPropNum)
    '                DecodePropertyData(pStoryData, pDictEntries, pStringsList, pRoutineList, pPropNum, pPropSize, pPropStart, pPropData)
    '                Exit Sub
    '            End If
    '            ' Length = 2, Check is Routine, String or Word (in that order)
    '            ' Length mod 2 = 0, Check if all ar words
    '            If pPropSize = 2 Then
    '                Dim oStringData As StringData = pStringsList.Find(Function(c) c.entryPointPacked = Helper.GetAdressFromWord(pStoryData, pPropStart))
    '                Dim oRoutineData As RoutineData = pRoutineList.Find(Function(c) c.entryPointPacked = Helper.GetAdressFromWord(pStoryData, pPropStart))
    '                If oStringData IsNot Nothing Then
    '                    Console.WriteLine("(PROP-{0} {1}{2}{3})", pPropNum, Convert.ToChar(34), oStringData.text, Convert.ToChar(34))
    '                    Exit Sub
    '                End If
    '                If oRoutineData IsNot Nothing Then
    '                    Console.WriteLine("(PROP-{0} R{1:X5})", pPropNum, oRoutineData.entryPoint)
    '                    Exit Sub
    '                End If
    '            End If
    '            ' Length mod 2 = 0, Check if all ar words
    '            If (pPropSize Mod 2) = 0 Then
    '                Dim bValidWords As Boolean = True
    '                For i As Integer = 0 To pPropSize - 2 Step 2
    '                    If pDictEntries.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, pPropStart + i)) Is Nothing Then
    '                        bValidWords = False
    '                        Exit For
    '                    End If
    '                Next
    '                If bValidWords Then
    '                    Console.Write("(PROP-{0}", pPropNum)
    '                    For i As Integer = 0 To pPropSize - 2 Step 2
    '                        Console.Write(" {0}", pDictEntries.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, pPropStart + i)).dictWord.ToUpper)
    '                    Next
    '                    Console.WriteLine(")")
    '                    Exit Sub
    '                End If
    '            End If
    '            ' Length mod 2 = 0, Check if they are a pattern WORD FUNCTION, then PSEUDO
    '            If (pPropSize Mod 2) = 0 And pPropSize > 3 Then
    '                Dim bValidPseudo As Boolean = True
    '                For i As Integer = 0 To pPropSize - 4 Step 4
    '                    Dim iIdx As Integer = pPropStart + i + 2
    '                    If pDictEntries.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, pPropStart + i)) Is Nothing Or
    '                       pRoutineList.Find(Function(c) c.entryPointPacked = Helper.GetAdressFromWord(byteStory, iIdx)) Is Nothing Then
    '                        bValidPseudo = False
    '                        Exit For
    '                    End If
    '                Next
    '                If bValidPseudo Then
    '                    Console.Write("(PSEUDO")
    '                    For i As Integer = 0 To pPropSize - 4 Step 4
    '                        Dim iIdx As Integer = pPropStart + i + 2
    '                        Dim oRoutineData As RoutineData = pRoutineList.Find(Function(c) c.entryPointPacked = Helper.GetAdressFromWord(byteStory, iIdx))
    '                        Console.Write(" {0}{1}{2}", Convert.ToChar(34), pDictEntries.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, pPropStart + i)).dictWord.ToUpper, Convert.ToChar(34))
    '                        Console.Write(" R{0:X5}", oRoutineData.entryPoint)
    '                    Next
    '                    Console.WriteLine(")")
    '                    Exit Sub
    '                End If
    '            End If
    '            Console.WriteLine("Data = {0}", pPropData)
    '        End If
    '    ElseIf {eCompiler.INFORM5, eCompiler.INFORM6}.Contains(compilerSource) Then
    '        ' Length = 2, Check is Routine, String or Word (in that order)
    '        ' Length mod 2 = 0, Check if all ar words
    '        If pPropSize = 2 Then
    '            Dim oStringData As StringData = pStringsList.Find(Function(c) c.entryPointPacked = Helper.GetAdressFromWord(pStoryData, pPropStart))
    '            Dim oRoutineData As RoutineData = pRoutineList.Find(Function(c) c.entryPointPacked = Helper.GetAdressFromWord(pStoryData, pPropStart))
    '            If oStringData IsNot Nothing Then
    '                Console.WriteLine("prop-{0} {1}{2}{3}", pPropNum, Convert.ToChar(34), oStringData.text, Convert.ToChar(34))
    '                Exit Sub
    '            End If
    '            If oRoutineData IsNot Nothing Then
    '                Console.WriteLine("prop-{0} R{1:X5}", pPropNum, oRoutineData.entryPoint)
    '                Exit Sub
    '            End If
    '        End If
    '        If (pPropSize Mod 2) = 0 Then
    '            Dim bValidWords As Boolean = True
    '            For i As Integer = 0 To pPropSize - 2 Step 2
    '                If pDictEntries.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, pPropStart + i)) Is Nothing Then
    '                    bValidWords = False
    '                    Exit For
    '                End If
    '            Next
    '            If bValidWords Then
    '                Console.Write("prop-{0}", pPropNum)
    '                For i As Integer = 0 To pPropSize - 2 Step 2
    '                    Console.Write(" '{0}'", pDictEntries.GetEntryAtAddress(Helper.GetAdressFromWord(byteStory, pPropStart + i)).dictWord)
    '                Next
    '                Console.WriteLine()
    '                Exit Sub
    '            End If
    '            Console.WriteLine("Data = {0}", pPropData)
    '        End If
    '    Else
    '        Console.WriteLine("Data = {0}", pPropData)
    '    End If
    'End Sub

End Class
