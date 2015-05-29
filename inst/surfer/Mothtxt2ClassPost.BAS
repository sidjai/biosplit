Sub Main

	Dim Surf, myPlot, addMap, States, frame, CohortFrame, MothFrame, tem As Object
	Set Surf = CreateObject("Surfer.Application")
	'Surf.Visible =True

	Dim yr, minMoth As Double
	Dim types(6) As Double
	Dim cArr(6,3) As Double
	'Dim coltypes(5) As Variant
	Dim limits(11) As Double
	Dim beg As Integer


inp = CStr(Command$)
'yr=2011
'direc = "C:\Users\Siddarta.Jairam\Documents\Hysplit Out Moth table"

'runName = "runwkend3standw"
'runName = "runlowerStart"
'runName = "runlowStartAll"
'runName = "runnoWind"
'runName = "runlowFL"
'runName = "runcornThres"
'runName = "runsecond"
'runName = "runIsobar2"
'runName = "runIsobar4"
'runName = "runIsobarQuickFix"
'runName = "runTiredFix"
'runName = "runTest4"
'runName = "runMaineBoost"
'runName = "runLimFlight2"

On Error GoTo coda

'direc = direc & "\" &  Join(Array(yr)) & "\" & runName & "\"
direc = Split(inp,Chr(34))(1) & "/"


If (yr<0) Then 'just to make this code not run in normal circumstances
	coda:
				If (InStr(Err.Description,"Directory")>0) Then
					Resume Next
				End If
End If

MkDir(direc & "jpeg")
MkDir(direc & "jpeg/ClassPost")

templateName ="C:\Users\Siddarta.Jairam\Documents\Documentation\tempOutL.srf"
'Open template
Set myPlot = Surf.Documents.Open(FileName:=templateName)

ptype ="Cohort"
frameIndex = 1
Set States = myPlot.Shapes.Item(frameIndex)
beg=1

legy =States.Top-.68*States.Height
legx =States.Left+(32.9/45)*States.Width
texy =States.Top-.85*States.Height
texx =States.Left+(5/45)*States.Width

For poptype =1 To 2
'Copies the State frame
CopyRight(myPlot.Shapes.Item(frameIndex), ptype)




file = Dir((direc+ptype + "*"))



While Len(file)>0
	Debug.Print ("File:" & file)
	'Add map to select index
	'Set States = myPlot.Shapes.Item(frameIndex)
	Set frame = myPlot.Shapes.AddClassedPostMap2((direc & file))

	Set addMap = OverlayStuff(frameIndex,frame)



	If ( beg=1) Then
		'Make new map
		'Set tem = myPlot.Shapes.Item(frameIndex)
		'Set States = tem.Overlays.BreakApart(tem.Overlays(1))
		'States.Name = "States"

		'tem.Name = ptype
		'Set addMap = tem.Overlays(1)

		'Add the legend for one of them
		addMap.ShowLegend = True
		With addMap.Legend
			.SetZOrder(srfZOToBack)
			.SetZOrder(srfZOForward)
			.Top =legy
			.Left = legx
			.FrameStyle = srfLegFrameNone
			'.Top =addMap.Top + .15* addMap.Height
			'.Left =addMap.Left +1.08* addMap.Width

			With .LabelFormat
				.NumDigits = 6
				.Thousands = True
				.TrimExponent = True
			End With
		End With
		'With addMap.Legend.LabelFormat

		'End With

		'Sets the ouput text location

		'xs =addMap.Left - .5*addMap.Width
		'ys = addMap.Top - 1.3*addMap.Height
		beg=0
	End If

	'change how the labels look
	With addMap
		.NumClasses = 6
		.LabCol = 5
		.LabelPos = srfPostPosCenter
		.LabelFont.Size = 2
		.LabelFont.Color = srfColorWhite
	End With



	'change the symbols
	minMoth = 500
	types(1) = 57
	types(2) = 6
	types(3) = 10
	types(4)  = 12
	types(5) = 15
	types(6) = 30

	cArr(1,1) = 0 : cArr(1,2) = 232 : cArr(1,3) = 242
	cArr(2,1) = 0 : cArr(2,2) = 0 : cArr(2,3) = 255
	cArr(3,1) = 0 : cArr(3,2) = 51 : cArr(3,3) = 51
	cArr(4,1) = 102 : cArr(4,2) = 51 : cArr(4,3) = 51
	cArr(5,1) = 255 : cArr(5,2) = 0 : cArr(5,3) = 0
	cArr(6,1) = 0 : cArr(6,2) = 0 : cArr(6,3) = 0

	'coltypes =  Array("nope","Blue", "Dark Green", "Dark Brown", "Red", "Black")


	For sym= 1 To 6
		With addMap.BinSymbol(sym)
			.Set = "GSI Default Symbols"
			.Index = types(sym)
			.Size =0.05
			.Color = RGB(cArr(sym,1),cArr(sym,2),cArr(sym,3))
		End With
		If (sym=1) Then
			limits(0) = 0
			limits(1) = minMoth
		Else
			limits(sym*2-2) = minMoth
			minMoth = minMoth*10
			limits(sym*2-1) = minMoth
		End If

	Next

	limits(11) = 1*10^15
	addMap.SetBinLimits(limits)


	'Output the single jpeg file with annotation
	des = Split(file,".")(0)

	myPlot.Shapes.AddText(texx,texy,des).Font.Size = 14
	outfile =direc+"jpeg/ClassPost/"+des+".jpg"
	myPlot.Export2(outFile,Options:="HDPI=900,VDPI=900", FilterId:="jpg")

	'Clean things up
	myPlot.Shapes("Text").Delete
	addMap.Visible = False
file = Dir
Wend
frameIndex =myPlot.Shapes.Count
ptype = "Moth"
Next

myPlot.SaveAs(direc+"FinalPlotClass.srf")
'Set clpost1 = mapframe2.Overlays("Classed Post-demogrid.dat")

'clpost1.ShowLegend = False

'For i = 1 To 5

'With clpost1.BinSymbol(i)

'.Set = "GSI Default Symbols"

'.Index = 12

'.Size = 0.1

'End With


'Next i

'clpost1.BinSymbol(1).Color = srfColorPastelBlue

'clpost1.BinSymbol(2).Color = srfColorGrassGreen

'clpost1.BinSymbol(3).Color = srfColorDeepYellow

'clpost1.BinSymbol(4).Color = srfColorLightOrange

'clpost1.BinSymbol(5).Color = srfColorBrickRed

'Description of the .cls file extension
'min, max, set,fill,border,border width, symbol number, symbol width
'defaults
'set="GSI Default Symbols",
'fill="Blue, Dark Green, Dark Brown, Red
'border= Black
'border width=1
'symbol number=6,10,12,15
'symbol width =0.05

'ClassedPostMap.LabelFormat.NumDigits = 5
'ClassedPostMap.LabelFormat.Thousands = 1
'ClassedPostMap.LabelFormat.TrimExponent = 1
'ClassedPostMap.LabelPos = srfPostPosCenter
'ClassedPostMap.LabelFont.Size = 2
'ClassedPostMap.LabelFont.Color = srfColorWhite
'addMap.LabelFormat.NumDigits = 5
	'addMap.LabelFormat.Thousands = 1
	'addMap.LabelFormat.TrimExponent = 1
	'addMap.LabelPos = srfPostPosCenter
	'addMap.LabelFont.Size = 2
	'addMap.LabelFont.Color = srfColorWhite


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
