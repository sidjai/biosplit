Sub Main
	Dim Surf As Object

	Dim junkF, inFile As String

	Dim direc(3) As String

	Dim yr, k As Double

	Set Surf = CreateObject("Surfer.Application")

	yr = 2011
	direc(1) = "C:\Users\Siddarta.Jairam\Documents\Hysplit temp data\"
	direc(2) = "C:\Users\Siddarta.Jairam\Documents\Hysplit wind data\"
	direc(3) = "C:\Users\Siddarta.Jairam\Documents\Hysplit soilT data\"

	xmin = -1005735
	xmax = 2194265
	ymin = 284055
	ymax = 2924055
	s=40000

	For vInd =1 To UBound(direc)


		direcIn=direc(vInd)&Join(Array(yr))&"\Parsed\"
		direcOut=direc(vInd)&Join(Array(yr))&"\ParseAndExtract\"
		junkF= direc(vInd) & "junk.grd"

		inFile = Dir((direcIn+"*.grd"))
		Dim gridarray(1 To 1) As String

		k=1
		While Len(inFile)>0
			Debug.Print(inFile)
			gridarray(1) =direcIn & inFile
			temp=Split(inFile,".g")
			outFile=direcOut & temp(0)

			Surf.GridMosaic(InGrids := gridarray, xMin:=xmin, xMax:=xmax,xSpacing:=s, yMin:=ymin, yMax:=ymax-s,ySpacing:=s, outGrid:=(outFile & ".grd"))
			Surf.GridConvert2(InGrid:=(outFile & ".grd"), outGrid:=(outFile & ".dat"), OutFmt:=srfGridFmtXYZ)
			inFile=Dir
		Wend
	Next
End Sub
