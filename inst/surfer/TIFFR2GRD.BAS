Sub Main

Dim Surf As Object

Dim junkF As String

Dim yr, newArea,j As Double


Set Surf = CreateObject("Surfer.Application")

inp = CStr(Command$)
yr=2012
newArea = 40000
'direc="C:\Users\Siddarta.Jairam\Documents\Crop Data\"

'direc=direc&Join(Array(yr))&"\"
direc = inp & "/"
outGrd = direc&"Projected/"&"Crop_Grid_"&Join(Array(yr))

direc = direc & "RawNASS/"


'Get all the GeoTiff files in the NASS crop downloaded files

	Dim sorted(80)  As String
	Dim rawFiles(80)  As String
	Dim inFiles As String
	Dim temp,sectemp As Variant
	Dim ind, tot As Integer


	inFiles = Dir((direc+"*.tif"))

	j=1
	Do While Len(inFiles) > 0
		rawFiles(j)=inFiles
		inFiles= Dir
		j=j+1
	Loop

	tot=j-1

	For j =1 To tot

		temp=Split(rawFiles(j),"_")
		sectemp=Split(temp(2),".")
		ind=CInt(sectemp(0))
		sorted(ind)=direc & rawFiles(j)
	Next





Dim totGrid As Object
Set totGrid = Surf.Documents.Add(srfDocWks)


'Go through each file and add the large blocks to an updated matrix

'Do While j< 3

For j =1 To tot
	Debug.Print(sorted(j))
	'Go through the tif file and sum up the 30m2 blocks into 40km blocks

	sumMtoKM(sorted(j),newArea,totGrid,Surf)

	If j=1 Then
		totGrid.SaveAs(FileName:=(outGrd&".dat"),FileFormat:=srfGridFmtXYZ)
	Else
		totGrid.Save
	End If

Next


Surf.GridData2((outGrd&".dat"), OutGrid:=(outGrd&".grd"))



'Surf.GridMosaic(InGrids:=gridarray, xMin:=x, xMax:=x+s, yMin:=y, yMax:=y+s, outGrid:=junkF)

'Surf.GridExtract(larFile,rFreq:=skipr,cFreq:=skipc,outGrid:=smFile)

End Sub

Function sumMtoKM (File As String,s As Double,datum As Object,Surf As Object) As Object


	Dim temp, spc, spr, reg,k,top,r ,c, mx,my, skipr, skipc,smNum As Double

	Dim flagxReset, flagyReset As Integer

	Set lar =Surf.NewGrid
	lar.LoadFile(File,HeaderOnly:=False)
	Dim gridarray(1 To 1) As String
	gridarray(1)=File

	junkB = "C:\Users\Siddarta.Jairam\Downloads\junk_xyz.dat"
	'mx=lar.xMin+s
	mx=lar.xMax
	my=lar.yMax

	x=lar.xMin
	y=lar.yMin

	spc =lar.xSize
	spr = lar.ySize

	skipc=Round(s/spc)
	skipr=Round(s/spr)

	smCols=Round((mx-x)/s)
	smRows=Round((my-y)/s)
	smNum =smCols*smRows+2

	Dim  xs(), ys() As Double
	ReDim xs(smNum)
	ReDim ys(smNum)

	'Load up the xs and the ys of the large blocks
	k=1
	While Abs(my-y)>40
		While Abs(mx-x)>40
			xs(k)=x
			ys(k)=y
			x=x+s
			k=k+1
		Wend
		x=lar.xMin
		y=y+s
	Wend

	st = (datum.Columns(1,1)).RowCount -1
	'Go through the blocks and sum the 30 m2 segments
	For ki= 1 To k-1
		'Find the top and the right edge of the box for a given x and y
		w=ki
		top=1
		While top=1

			If w>smNum Then
				top= my
			Else
				If Abs(ys(w)-ys(ki))<40 Then
					w=w+1
				Else
					top=ys(w)
				End If

			End If
		Wend

		If (ki+1>(smNum)) Then
			lex = mx
		ElseIf (xs(ki+1)<xs(ki)) Then
			lex = mx
		Else
			lex=xs(ki+1)
		End If
		Debug.Print ki; "/"; smNum

			'Grid the 40km block into a temp file
			'Surf.GridMosaic(InGrids:=gridarray, xMin:=13000, xMax:=13500, yMin:=2123145, yMax:=2143145, outGrid:=junkB, OutFmt:=srfGridFmtXYZ)
			Surf.GridMosaic(InGrids:=gridarray, xMin:=xs(ki), xMax:=lex, xSpacing:=spc, ySpacing:=spr, yMin:=ys(ki), yMax:=top, outGrid:=junkB, OutFmt:=srfGridFmtXYZ)
			'Surf.GridConvert2(InGrid:=junkF, outGrid:=junkB, OutFmt:=srfGridFmtXYZ)

			'Get the stats of that temp file and save it
			Set data = Surf.Documents.Open(FileName:=junkB)
			Set stats = data.Columns(Col1:=3, Col2:=3).Statistics(Flags:=wksStatsSum)
			datum.Cells(st+ki,1).Value=xs(ki)
			datum.Cells(st+ki,2).Value=ys(ki)
			datum.Cells(st+ki, 3).Value=Round(stats.Sum*(0.09/.391019607843),0)
			data.Close

			Kill junkB

	Next

	Exit Function

End Function
