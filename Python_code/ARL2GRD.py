import clr
clr.AddReferenceByPartialName("System")
#clr.AddReferenceByPartialName("System.IO")
clr.AddReferenceByPartialName("System.Windows.Forms")
clr.AddReferenceByPartialName("System.Drawing")
from System import *
from System.Globalization import *
from System.IO import *
from System.Windows.Forms import *
from System.Drawing import *
clr.AddReference("MeteoInfoC.dll")
from MeteoInfoC import *
from MeteoInfoC.Data import *
from MeteoInfoC.Layout import *
from MeteoInfoC.Data.MeteoData import *
from calendar import *
import os.path
import sys
#from win64com import client

#surf = client.Dispatch("Surfer.Application") 
Met = MIApp()
direcT = {"T":"C:/Users/Siddarta.Jairam/Documents/Hysplit temp data/", 
	"W":"C:/Users/Siddarta.Jairam/Documents/Hysplit wind data/", 
	"S":"C:/Users/Siddarta.Jairam/Documents/Hysplit soilT data/"}
#direcT[0] = "C:/Users/Siddarta.Jairam/Documents/Hysplit temp data/"
#direcT[1] = "C:/Users/Siddarta.Jairam/Documents/Hysplit wind data/"
#direcT[2] = "C:/Users/Siddarta.Jairam/Documents/Hysplit soilT data/"

packed = "C:/Users/Siddarta.Jairam/Documents/ARL packed files/"
metDataType = "edas"
Var = {"T":"T02M", 
	"W":"V10M", 
	"S":"SOLT"}
	
resName = {"T":"T2M", 
	"W":"VdirWind10M", 
	"S":"Tsoil"}

#Var[0]="T02M"
#Var[1]="V10M"
#Var[2]="SOLT"


months = 12
year = 11

for item in direcT:
	direcT[item] = direcT[item] +str(2000+year)+"/"
	#junkF[item] = direc[item]+"junk.grd"

#direcT=direcT+str(2000+year)+"/"

packed = packed+str(2000+year)+"/" + metDataType+ "/"

#from:lat 25, lon -105
#to:50,-80
NASSproj="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

xmin = -1005735
xmax = 2194265
ymin = 284055
ymax = 2924055

info = MeteoDataInfo()

def zstr(num):
	#return the string of the number including a zero at beg
	if num<10:
		val = "0" + str(num)
	else:
		val = str(num)
	return val


# Go through the months
for mInd in [d+1 for d in range(months)]:
	k=1
	inds=[None]*250
	hs=[None]*250
	ds=[None]*250
	# iterate through the days and get the days, hours and the time index
	# Grabs the index for midnight and 3PM as the min and max temps for the day
	for di in range(1,monthrange(2000+year,mInd)[1]+1):
		hs[k]=0
		ds[k]=di
		if k!=1:
			inds[k]=inds[k-1]+3
		else:
			inds[k]=2
			
		k+=1
		hs[k]=15
		ds[k]=di
		inds[k]=inds[k-1]+5
		k+=1
		
	inFile = packed +"edas."+(month_abbr[mInd]).lower() + str(year)
	sys.stdout.write(inFile)  # same as print
	sys.stdout.flush()
	if os.path.isfile(inFile):
		#Load the file and project it to NASS
		Met.OpenARLData(inFile)
		Met.ProjectLayers(NASSproj)
		#tTot = monthrange(2000+year,mInd)[1] *8
		#print tTot
		
		#Go through all the collected time indices and save the data in Parsed as a grid file.
		for tInd in range(1,k):
			Met.TimeIndex = inds[tInd]
			for vInd in Var:
				
				#print Met.GetTime(29).ToString("yyyy-MM-dd HH:00")
				#tim= info.GetTime(inds[tInd])
				#print tim.ToString("yyyy-MM-dd HH:00")
				outFile = direcT[vInd] + "Parsed/edas_"+ resName[vInd] +"_" + zstr(mInd)+"_"+zstr(ds[tInd])+"_"+str(year) +"_at_"+zstr(hs[tInd])+".grd"
				outFileDat = direcT[vInd] + "Parsed/edas_" + resName[vInd] +"_" + zstr(mInd)+"_"+zstr(ds[tInd])+"_"+str(year) +"_at_"+zstr(hs[tInd])+".dat"
				
				#print outFile
				datu = Met.GetGridData(Var[vInd])
				
				#datu = datu.Extract(ymin,ymax,xmin,xmax)
				datu.SaveAsSurferASCIIFile(outFile)
				
				#Surf.GridMosaic(InGrids= outFile, xMin=xmin, xMax=xmax, yMin=ymin, yMax=ymax, outGrid=outFileDat, OutFmt=srfGridFmtXYZ)
			