Sub Main


	Dim Surf, myPlot, States, frame, tem, doc As Object
	Set Surf = CreateObject("Surfer.Application")
	'Surf.Visible =True

	Dim yr As Double
	Dim numPop(3) As Integer
	Dim Origin(3) As String
	Dim addMap(3) As Object
	Dim popDes(3) As String




	Set doc = Surf.Documents

inp = CStr(Command$)
Debug.Print (inp)
'inp = "C:/Users/Siddarta.Jairam/Documents/Hysplit Out Moth table/2011/runLimFlight2"
Debug.Print (inp)
'yr=2011
'runName = "runsecond"
'runName = "runcornThres"
'runName = "runIsobarQuickFix"
'runName = "runIsobar3"
'runName = "runLimFlight2"

Dim logLevels(5) As Double
numLevels = 5
Origin (0) ="Wide Downward Diagonal" 'TX
Origin (1)  = "Wide Upward Diagonal" 'FL
Origin (2) ="Wide Downward Diagonal" 'TXCohort
Origin (3)  = "Wide Upward Diagonal" 'FLCohort
'direc = "C:\Users\Siddarta.Jairam\Documents\Hysplit Out Moth table"

On Error GoTo coda

If (numLevels<0) Then
	coda:
				If (InStr(Err.Description,"planar")>0) Then
					Err = 0
					Resume Next
				ElseIf (InStr(Err.Description,"Nothing")>0) Then
					Err = 0
					Resume Next
				ElseIf (InStr(Err.Description,"Directory")>0) Then
					Err = 0
					Resume Next
				Else
					Debug.Print  "Err="; Err.Description ; Err
					Debug.Print  Err.Source
					Resume Next
					'Error Err
				End If
End If
 'Resume Next

'mainFol = direc & "\" &  Join(Array(yr)) & "\" & runName
mainFol = inp
outFinal = mainFol & "/"
outFol =mainFol & "/jpeg/"
MkDir(outFol)
outFol =outFol & "Contours/"
MkDir(outFol)
mainFol = mainFol &"/ncs/"

popDes(0) = "FLMoth"
popDes(1) = "TXMoth"
popDes(2) = "FLCohort"
popDes(3) = "TXCohort"

'Gather the file list

'fileList = GetFileList(mainFol , "TXMoth",  leList,0)
'Debug.Print(Len(fileList))
'file = Dir((mainFol+ "TXMoth*"))
'k=1
'While Len(file)>0

'	fileList(0,k) = (mainFol & file)
'	k=k+1
'	file = Dir
'Wend
'file = Dir(mainFol+ "FLMoth*")
'k=1
'While Len(file)>0
	'fileList(1,k) = (mainFol + file)
	'k=k+1
	'file = Dir
'Wend

'fileList = GetFileList(mainFol , "TXCohort", fileList, 2)
'fileList = GetFileList(mainFol , "FLCohort", fileList, 3)

templateName ="C:\Users\Siddarta.Jairam\Documents\Documentation\tempOut.srf"
'Open template
Set myPlot = Surf.Documents.Open(FileName:=templateName)


frameIndex = 1
Set States = myPlot.Shapes.Item(frameIndex)
beg=1


legy =States.Top-.68*States.Height
legx =States.Left+(32.9/45)*States.Width
texy =States.Top-.85*States.Height
texx =States.Left+(5/45)*States.Width

logLevels(0) =500
For i = 1 To numLevels
	logLevels(i) = logLevels(i-1)*100
Next
	logLevels(numLevels)=1*10^15
CopyRight(myPlot.Shapes.Item(frameIndex), "Cohort")
CopyRight(myPlot.Shapes.Item(frameIndex), "Moth")

While (frameIndex<5)
	If (frameIndex=1)Then
		popNum = 0
	Else
		popNum = 2
	End If
	fileList = GetFileList(mainFol,popDes(popNum))
For wk = 1 To GetLast(fileList)
	FLlist = GetFileList(mainFol,popDes(popNum))
	TXlist = GetFileList(mainFol,popDes(popNum+1))

'Algorithm:=srfKriging, _	'KrigType:= srfKrigBlock, _
	For oInd = popNum To popNum+1
		If oInd =popNum Then
			tfile = FLlist(wk)
		Else
			tfile = TXlist(wk)
		End If
		temp = Split(tfile,"/")
		k=0
		For Each i In temp
			k=k+1
		Next
		des = temp(k-1)
		des = Split(des,".")(0)

		Debug.Print ("File:" & des)
		'Surf.GridData2(fileList(oInd,wk), OutGrid:=(mainFol&"junk.grd"), _
			'ExclusionFilter:=("_E=" & Join(Array(oInd))), _
			'DupMethod:=srfDupSum, xDupTol:=0.1, yDupTol:=0.1, _
			'Algorithm:=srfRadialBasis, _
		'	RBBasisType:= srfMultiLog, _
			'ShowReport:=False)
		'Processing
		Surf.GridFilter(tfile,srfFilterGaussianLP,OutGrid:=(mainFol & des & ".grd"))
		Set frame = myPlot.Shapes.AddContourMap((mainFol & des & ".grd"))
		Kill (mainFol & des & ".grd")
		Kill (mainFol & des & ".prj")

		If (IsObject(frame)) Then
			Set addMap(oInd) = OverlayStuff(frameIndex,frame)

			With addMap(oInd)

				.Levels.Set(Levels:=logLevels)


				.LabelFont.Size = 10
				.LabelFont.Bold = True
				.LabelFont.BackColorRGBA.Opacity = 100

				.LabelFormat.Type = srfLabGeneral
				.LabelFormat.TrimExponent = True
				.LabelFormat.NumDigits = 0
				.LabelLabelDist = 0.7
				.LabelEdgeDist = 0.3

				.Levels.SetLabelFrequency( FirstIndex:=1, NumberToSet:=1, NumberToSkip:=0 )

				.FillContours =True
				.FillPattern = Origin(oInd)
				.FillTransparent = True
				.Opacity = 100
				.FillBackgroundColorMap.Reverse()
				'.FillForegroundColorMap.Reverse()
				.FillForegroundColorMap.SetDataLimits(5,logLevels(numLevels))
				.SmoothContours = srfConSmoothLow
				.ApplyFillToLevels(FirstIndex:=1, NumberToSet:=1, NumberToSkip:=0)

				bop=27
				For eInd= 1 To addMap(oInd).Levels.Count

					With .Levels.Item(eInd)
						.Line.Width = 0.015
						.Fill.Pattern = Origin(oInd)
						.Fill.ForeColorRGBA.Opacity = bop
						'.Fill.ForeColorRGBA.Color = srfColorBlack
					End With
					bop =1.3*bop
					If (bop>100) Then
						bop = 100
					End If
				Next

			End With

		End If


	Next

	If (IsObject(addMap(1))) Then

		'addMap(1).Levels.SetHachFrequency(FirstIndex:=2, NumberToSet:=1, NumberToSkip:=0)
		'With addMap(1)
		'	.HachClosedOnly = False
		'	.HachLength = 0.05
	'	End With

	'For lind = 1 To  numLevels
		'addMap(1).Levels(lind).Line.Style = "Solid - Ticks"
		'addMap(1).Levels(lind).Line.ForeColor = srfColorDarkRed
	'Next


	'Output the single jpeg file with annotation
	If (frameIndex =1) Then
		des = "Moth" & Split(des,"Moth")(1)
	Else
		des = "Cohort" & Split(des,"Cohort")(1)
	End If


	myPlot.Shapes.AddText(texx,texy,des).Font.Size = 14
	outfile =outFol+des+".jpg"
	myPlot.Export2(outFile,Options:="HDPI=160,VDPI=160, Quality=100", FilterId:="jpg")

	'Clean things up
	myPlot.Shapes("Text").Delete
	addMap(popNum).Visible = False
	addMap(popNum+1).Visible = False
	End If
Next
frameIndex = frameIndex+3
Wend
'Save result

myPlot.SaveAs(outFinal+"FinalPlotCont.srf")

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
