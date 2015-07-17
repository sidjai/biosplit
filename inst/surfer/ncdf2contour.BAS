Sub Main


	Dim Surf, myPlot, States, frame, tem, doc As Object
	Set Surf = CreateObject("Surfer.Application")
	'Surf.Visible =True

	Dim yr As Double
	Dim addMap As Object
	Dim valid As Object
	Dim fillType As String
	Dim popDes(3) As String
	Dim myLevels() As Double
	Dim colArr() As Integer
	Dim bLimits() As Double
	Dim symTypes(4) As Integer
	Dim args(3) As String


	Set doc = Surf.Documents

inp = CStr(Command$)

'Defaults
args(0) = "no File"
args(1) = "22"
args(2) = "color"
args(3) = "no classPost overlay"

argSpl = Split(inp,Chr(34))
argInd = 1
While argInd < UBound(argSpl)
	args((argInd - 1)/2) = argSpl(argInd)
	argInd = argInd + 2
Wend
Debug.Print (args(0))
Debug.Print (args(1))

numLevels = CInt(args(1)) +1
shUseColor = CInt(CBool(InStr(args(2), "color")))
shDoClassPost = CBool(InStr(args(3), "/"))

lastSlashInd = InStrRev(args(0), "/")
runInd = InStr(args(0), "run")
relPart = Right(args(0), Len(args(0)) - runInd + 6)
runName = goUpDir(relPart)

ncName = args(0)
mainFol = goUpDir(args(0))

ncDes =  Replace(Right(args(0), Len(args(0)) - lastSlashInd), ".nc", "")
finalDes = runName & "  | " & ncDes

pathOutSrf = mainFol & "/" & ncDes & ".srf"
pathOutJpeg =  mainFol & "/jpeg/Contours/"  & ncDes & ".jpg"
fillType = "Solid"

ReDim colArr(0 To numLevels, 0 To 2) As Integer
ReDim myLevels(0 To numLevels) As Double
ReDim bLimits(0 To numLevels*2 - 2) As Double
templateName = "C:\Users\Siddarta.Jairam\Documents\Documentation\tempOutL.srf"
colorMapName = "C:\Users\Siddarta.Jairam\Documents\Documentation\Result Files\out.clr"
'Open template
Set myPlot = Surf.Documents.Open(FileName:=templateName)


frameIndex = 1
Set States = myPlot.Shapes.Item(frameIndex)
beg=1


legy = States.Top-.6*States.Height
legx = States.Left+(32.9/45)*States.Width
texy = States.Top-.85*States.Height
texx = States.Left+(5/45)*States.Width


CopyRight(myPlot.Shapes.Item(frameIndex), numName)

'Processing
Set frame = myPlot.Shapes.AddContourMap(GridFileName:=ncName)

If (IsObject(frame)) Then
	Set addMap = OverlayStuff(frameIndex,frame)
	zmax = addMap.Grid.zMax
	zmin = addMap.Grid.zMin
	slope = (zmax - zmin)/ (numLevels - 2)
	slopeRGB = 500 / (numLevels - 1)
	slopeBW = 200 / (numLevels - 1)
	eps = .0001

	myLevels(0) = zmin
	myLevels(1) = zmin + eps


	For i = 1 To numLevels
		If (i > 1) Then
			myLevels(i) = myLevels(i-1) + slope
		End If

		If (i = 2) Then
			myLevels(i) = myLevels(i) - eps
		End If
	Next
	myLevels(numLevels-1) = zmax - eps
	myLevels(numLevels) = zmax

	If (CBool(shUseColor)) Then

		colArr(0,0) = 250 :	colArr(0,1) = 0 : colArr(0,2) = 0
		For i = 1 To numLevels
			If (i/numLevels < 0.5) Then
				colArr(i,0) = colArr(i-1,0)  - slopeRGB
				colArr(i,1) = 0 : colArr(i,2) = 0
			ElseIf (i/numLevels = 0.5) Then
				colArr(i,1) = 50
				colArr(i,0) = 0 : colArr(i,2) = 0
			Else
				colArr(i,2) = colArr(i-1,2)  + slopeRGB
				colArr(i,0) = 0 : colArr(i,1) = 0
			End If
		Next
		colArr(numLevels, 0 ) = 0 : colArr(numLevels,1) = 0 : colArr(numLevels,2) = 250

	Else

		colArr(0,0) = 0 :	colArr(0,1) = 0 : colArr(0,2) = 0
		For i = 1 To numLevels

			colArr(i,0) = colArr(i-1,0)  + slopeBW
			colArr(i,1) = colArr(i-1,1)  + slopeBW
			colArr(i,2) = colArr(i-1,2)  + slopeBW
		Next

		colArr(numLevels, 0 ) = 200 : colArr(numLevels,1) = 200 : colArr(numLevels,2) = 200


	End If

	With addMap


		.Levels.Set(Levels:=myLevels)

		.LabelFont.Size = 9
		.LabelFont.Bold = True
		.LabelFont.BackColorRGBA.Opacity = 100
		.LabelLabelDist = 0.7
		.LabelEdgeDist = 0.3

		'.LabelFormat.Type = srfLabFixed
		.LabelFormat.TrimExponent = True
		.LabelFormat.NumDigits = 2

		.Levels.SetLabelFrequency( FirstIndex:=1, NumberToSet:=1, NumberToSkip:=0 )

		.FillContours =True
		.FillPattern = fillType
		.FillTransparent = True

		'.FillForegroundColorMap.Reverse()
		'.FillForegroundColorMap.SetDataLimits(5,1*10^15)
		.SmoothContours = srfConSmoothHigh
		.ApplyFillToLevels(FirstIndex:=1, NumberToSet:=1, NumberToSkip:=0)

		For eInd= 1 To addMap.Levels.Count
			With .Levels.Item(eInd)
				dat = addMap.Levels.Item(eInd).Value
				With .Fill.ForeColorRGBA

					.Red = colArr(eInd - 1, 0)
					.Green = colArr(eInd - 1, 1)
					.Blue = colArr(eInd - 1, 2)

					If (dat = -1 Or dat = (1- eps)) Then
						.Opacity = 50
					Else
						.Opacity = 50
					End If
				End With


				.Line.Width = 0.015

				If ( Abs(Abs(dat)-.5) < 0.4 ) Then
					.Line.ForeColor = srfColorBlack30
					.Line.Width = 0.01
				End If

				.ShowLabel = 1
			End With
		Next

	End With

	If (shDoClassPost) Then
		Set valid = myPlot.Shapes.AddClassedPostMap2(args(3), xCol:= 3, yCol:= 2, zCol:= 8)


		Set addMap = OverlayStuff(1, valid)
		numBins = CLng(numLevels - 1)
		addMap.NumClasses = numBins

		symTypes(0) = 12 'Circle
		symTypes(1) = 33 'Star
		symTypes(2) = 6 'Diamond
		symTypes(3) = 10 'Square
		symTypes(4) = 18 'Triangle


		bLimits(0) = 0
		bLimits(1) = .5

		For sym = 2 To numBins
			bLimits(sym*2-2) = bLimits(sym*2-3)
			bLimits(sym*2-1) = bLimits(sym*2-2)  + 0.5
		Next

		bLimits(numBins*2 -1) = 20

		With addMap
			.SetBinLimits(Limits:=bLimits)
			.ShowLegend = True


			'.LabelFont.Size = 10
			'.LabelFont.Bold = True
			'.LabelFormat.Type = srfLabFixed
			'.LabelFormat.TrimExponent = True
			'.LabelFormat.NumDigits = 2

		End With

		Debug.Print addMap.NumClasses
		For sym= 1 To numBins
			With addMap.BinSymbol(sym)
				.Set = "GSI Default Symbols"
				.Index = symTypes(CInt((sym Mod 5)))
				.Size =0.1
				.Color = RGB(colArr(sym+1, 0), colArr(sym+1, 1) , colArr(sym+1, 2))
				.LineColor = srfColorBlack
			End With
		Next
		With addMap.Legend
			.Title = "Haplotype Ratio (h2/h4)"
			.SetZOrder(srfZOToBack)
			.SetZOrder(srfZOForward)
			.Top =legy
			.Left = legx
			.FrameStyle = srfLegFrameNone

			With .LabelFormat
				.NumDigits = 2
				.Thousands = True
				.TrimExponent = True
			End With
		End With



	End If

	If (IsObject(addMap)) Then

		myPlot.Shapes.AddText(texx,texy, finalDes).Font.Size = 14
		outfile = outFol + ncDes + ".jpg"
		myPlot.Export2(pathOutJpeg,Options:="HDPI=300,VDPI=300, Quality=100", FilterId:="jpg")
	End If
End If
'Save result

myPlot.SaveAs(pathOutSrf)

End Sub

Function OverlayStuff(index As Double, mapframe As Object) As Object

' Retrieve the parent PlotDocument object
Set plot = mapframe.Parent
Set first = plot.Shapes.Item(index)

' Clear all selections and then select the two MapFrame objects
plot.Selection.DeselectAll
mapframe.Selected = True
first.Selected = True

' Overlay the selected maps
Set temp =plot.Selection.OverlayMaps

Set OverlayStuff = temp.Overlays(temp.Overlays.Count)


End Function

Function CopyRight( mapframe As Object, pop As String)
Set plot = mapframe.Parent

plot.Selection.DeselectAll
mapframe.Selected =True
plot.Selection.Copy
plot.Shapes.Paste(srfPasteSurfer)
plot.Selection.DeselectAll

mapframe.Name = pop

Set mapframe2 = plot.Shapes.Item(plot.Shapes.Count)

mapframe2.Top = mapframe.Top
mapframe2.Left = mapframe.Left

End Function
Function GetFileList(fol As String, pop As String)
Dim fileList(365) As String
file = Dir((fol+ pop + "*"))
k=1
While Len(file)>0

	fileList(k) = (fol & file)
	k=k+1
	file = Dir
Wend

ReDim GetFileList(365) As String
GetFileList = fileList
End Function
Function GetLast(list As Variant)
k=0
For Each i In list
	If (Len(i)>0) Then
		k=k+1
	End If
Next
GetLast = k
End Function
Function ArraySlice(arr As Variant, num As Integer)
Dim temp() As Variant
ReDim temp(0 To num) As Variant
For ki = 0 To num
	temp(ki) = arr(ki)
Next
ArraySlice = temp
End Function
Function goUpDir(Sin As String) As String
 goUpDir = Left(Sin, InStrRev(Sin, "/")-1)
End Function
