Sub Main


	Dim Surf, myPlot, States, frame, tem, doc As Object
	Set Surf = CreateObject("Surfer.Application")
	'Surf.Visible =True

	Dim yr As Double
	Dim addMap As Object
	Dim fillType As String
	Dim popDes(3) As String
	Dim myLevels() As Double
	Dim colorsRed() As Integer
	Dim colorsBlue() As Integer
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

numLevels = CInt(args(1))
shUseColor = CBool(InStr(args(2), "color"))
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

ReDim colorsRed(0 To numLevels) As Integer
ReDim colorsBlue(0 To numLevels) As Integer
ReDim myLevels(0 To numLevels) As Double

templateName = "C:\Users\Siddarta.Jairam\Documents\Documentation\tempOutL.srf"
colorMapName = "C:\Users\Siddarta.Jairam\Documents\Documentation\Result Files\out.clr"
'Open template
Set myPlot = Surf.Documents.Open(FileName:=templateName)


frameIndex = 1
Set States = myPlot.Shapes.Item(frameIndex)
beg=1


legy = States.Top-.68*States.Height
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
	slope = (zmax - zmin)/ numLevels
	slopeRGB = 500 / numLevels

	myLevels(0) = zmin
	colorsRed(0) = 250
	colorsBlue(0) = 0
	For i = 1 To numLevels
		myLevels(i) = myLevels(i-1) + slope
		If (i/numLevels <= 0.5) Then
			colorsRed(i) = colorsRed(i-1)  - slopeRGB
			colorsBlue(i) = 0
		Else
			colorsRed(i) = 0
			colorsBlue(i) = colorsRed(i-1)  +  slopeRGB
		End If
	Next
	myLevels(numLevels) = zmax
	colorsRed(numLevels) = 0
	colorsBlue(numLevels) = 250

	With addMap


		.Levels.Set(Levels:=myLevels)

		.LabelFont.Size = 10
		.LabelFont.Bold = True
		.LabelFont.BackColorRGBA.Opacity = 100

		'.LabelFormat.Type = srfLabFixed
		'.LabelFormat.TrimExponent = True
		'.LabelFormat.NumDigits = 0

		.Levels.SetLabelFrequency( FirstIndex:=1, NumberToSet:=1, NumberToSkip:=0 )

		.FillContours =True
		.FillPattern = fillType
		.FillTransparent = True

		'.FillForegroundColorMap.Reverse()
		'.FillForegroundColorMap.SetDataLimits(5,1*10^15)
		.SmoothContours = srfConSmoothLow
		.ApplyFillToLevels(FirstIndex:=1, NumberToSet:=1, NumberToSkip:=0)

		For eInd= 1 To addMap.Levels.Count
			With .Levels.Item(eInd)
				.Fill.ForeColorRGBA.Red = colorsRed(eInd -1)
				.Fill.ForeColorRGBA.Blue = colorsBlue(eInd -1)
				.Fill.ForeColorRGBA.Green = 0
				.Fill.ForeColorRGBA.Opacity = 50

				.Line.Width = 0.015
				.ShowLabel = 1
			End With
		Next

	End With

	If (IsObject(addMap)) Then

		myPlot.Shapes.AddText(texx,texy, finalDes).Font.Size = 14
		outfile = outFol + ncDes + ".jpg"
		myPlot.Export2(pathOutJpeg,Options:="HDPI=160,VDPI=160, Quality=100", FilterId:="jpg")
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
