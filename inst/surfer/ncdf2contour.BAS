Sub Main


	Dim Surf, myPlot, States, frame, tem, doc As Object
	Set Surf = CreateObject("Surfer.Application")
	'Surf.Visible =True

	Dim yr As Double
	Dim addMap As Object
	Dim fillType As String
	Dim popDes(3) As String
	Dim logLevels(22) As Double


	Set doc = Surf.Documents

inp = CStr(Command$)
Debug.Print (inp)


numLevels = 22
fillType = "Solid"
'inp = """C:/Users/Siddarta.Jairam/Documents/Hysplit Out Moth table/2013/runTOcuePrec"



inp = Split(inp,Chr(34))(1)
Debug.Print(inp)
mainFol = inp
runName = Split(inp,"run")(1)
Debug.Print(runName)
outFinal = mainFol & "/"
outFol =mainFol & "/jpeg/"
'MkDir(outFol)
outFol =outFol & "Contours/"
'MkDir(outFol)
ncDes = "Sim-TrapFirstOcc"
ncName = mainFol & "/" & ncDes & ".nc"
Debug.Print(ncName)

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

logLevels(0) = -200
logLevels(1) = -20
For i = 2 To numLevels
	logLevels(i) = logLevels(i-1)+2
Next
logLevels(numLevels) = 200

CopyRight(myPlot.Shapes.Item(frameIndex), numName)

'Processing
Set frame = myPlot.Shapes.AddContourMap(GridFileName:=ncName)

If (IsObject(frame)) Then
	Set addMap = OverlayStuff(frameIndex,frame)


	With addMap


		.Levels.Set(Levels:=logLevels)

		.LabelFont.Size = 10
		.LabelFont.Bold = True
		.LabelFont.BackColorRGBA.Opacity = 100

		.LabelFormat.Type = srfLabGeneral
		.LabelFormat.TrimExponent = True
		.LabelFormat.NumDigits = 0

		.Levels.SetLabelFrequency( FirstIndex:=1, NumberToSet:=1, NumberToSkip:=0 )

		.FillContours =True
		.FillPattern = fillType
		.FillTransparent = True

		'.FillForegroundColorMap.Reverse()
		'.FillForegroundColorMap.SetDataLimits(5,1*10^15)
		.SmoothContours = srfConSmoothLow
		.ApplyFillToLevels(FirstIndex:=1, NumberToSet:=1, NumberToSkip:=0)

		bop=27
		For eInd= 1 To addMap.Levels.Count
			wkDiff = addMap.Levels.Item(eInd).Value
			If (eInd=1) Then
				wkDiff = wkDiff/10
			End If
			If (eInd = numLevels+1) Then
				wkDiff = wkDiff/10
			End If
			With .Levels.Item(eInd)

				.Line.Width = 0.015
				.ShowLabel = 1
				If (eInd < 11 ) Then
					.Fill.ForeColorRGBA.Red = -wkDiff * 10
					.Fill.ForeColorRGBA.Blue = 0
				ElseIf (eInd > 11) Then
					.Fill.ForeColorRGBA.Red = 0
					.Fill.ForeColorRGBA.Blue =  wkDiff * 10
				End If

				If(Abs(wkDiff) <3) Then
					.Fill.ForeColorRGBA.Green = 0
				Else
					.Fill.ForeColorRGBA.Green = 10
				End If


				.Fill.ForeColorRGBA.Opacity = 50

			End With
		Next

		.FillBackgroundColorMap.LoadFile(colorMapName)

	End With

	If (IsObject(addMap)) Then

		myPlot.Shapes.AddText(texx,texy, runName).Font.Size = 14
		outfile = outFol + ncDes + ".jpg"
		myPlot.Export2(outFile,Options:="HDPI=160,VDPI=160, Quality=100", FilterId:="jpg")
	End If
End If
'Save result

myPlot.SaveAs(outFinal + ncDes + ".srf")

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
li = num
Dim temp As Variant
For ki = 0 To num
	temp(ki) = arr(ki)
Next
ArraySlice = temp
End Function
