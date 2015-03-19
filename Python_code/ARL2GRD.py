
from org.meteoinfo.data import DataMath
from org.meteoinfo.data.meteodata import MeteoDataInfo
from org.meteoinfo.data import GridData
import org.meteoinfo.map.MapView
from org.meteoinfo.projection import ProjectionInfo, Reproject
from org.meteoinfo.global import Extent
from org.meteoinfo.geoprocess.analysis import ResampleMethods
from org.meteoinfo.data.meteodata.arl import ARLDataInfo
from calendar import *
import os.path
import sys
import os
import re

configLoc = os.path.dirname(os.path.realpath('__file__')).replace("Python_code","cfg.txt")
configTxt = open(configLoc)
raw = configTxt.readlines()[1:]
cfg = {"x":"hello"}
for line in raw:
    nice = line.strip('"').strip('"\n').split(' ="')

    key = nice[1].strip(' "')
    if re.search('c\(\\\\',key) is not None:
        key = re.findall('[^"]+',key)[1::2]
    
    cfg.update({nice[0]:key})

#surf = client.Dispatch("Surfer.Application") 
Met = MeteoDataInfo()
Repj = Reproject()
#direcT = {"T":"C:/Users/Siddarta.Jairam/Documents/Hysplit temp data/", 
	#"W":"C:/Users/Siddarta.Jairam/Documents/Hysplit wind data/", 
	#"S":"C:/Users/Siddarta.Jairam/Documents/Hysplit soilT data/"}
#direcT[0] = "C:/Users/Siddarta.Jairam/Documents/Hysplit temp data/"
#direcT[1] = "C:/Users/Siddarta.Jairam/Documents/Hysplit wind data/"
#direcT[2] = "C:/Users/Siddarta.Jairam/Documents/Hysplit soilT data/"

#packed = "C:/Users/Siddarta.Jairam/Documents/ARL packed files/"
#metDataType = "edas"
Var = {"T":"T02M", 
	"W":"V10M", 
	"S":"SOLT"}
Lookup = {"T":"AirTempFold", 
	"W":"WindFold", 
	"S":"SoilTempFold"}
resName = {"T":"T2M", 
	"W":"VdirWind10M", 
	"S":"Tsoil"}

#Var[0]="T02M"
#Var[1]="V10M"
#Var[2]="SOLT"


months = 12
shYear = str(int(cfg["year"])-2000)


#from:lat 25, lon -110
#to:50,-80

xmin = float(cfg["xmin"])
xmax = float(cfg["xmax"])
ymin = float(cfg["ymin"])
ymax = float(cfg["ymax"])
spc = float(cfg["spc"])

NASScrs="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#NASScrs = crsF.createFromEsriString(NASSproj)
NASSproj = ProjectionInfo(NASScrs)
ARLproj = ARLDataInfo().projInfo

info = MeteoDataInfo()

def zstr(num):
	#return the string of the number including a zero at beg
	if num<10:
		val = "0" + str(num)
	else:
		val = str(num)
	return val

#Make the ideal output for resampling
niceGrid = GridData(xmin, spc, int((xmax-xmin)/spc),
    ymin, spc, int((ymax-ymin)/spc))
# Go through the months
for mInd in [d+1 for d in range(months)]:
	k=1
	inds=[None]*250
	hs=[None]*250
	ds=[None]*250
	# iterate through the days and get the days, hours and the time index
	# Grabs the index for midnight and 3PM as the min and max temps for the day
	for di in range(1,monthrange(int(cfg["year"]),mInd)[1]+1):
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
		
	inFile = (cfg["MetARLFold"] + "/" + cfg["metDataType"] + "." +
		(month_abbr[mInd]).lower() + shYear)
	
	sys.stdout.write(inFile)
	sys.stdout.write("\n")
	sys.stdout.flush()
	if os.path.isfile(inFile):
		#Load the file and project it to NASS
		Met.openARLData(inFile)
		#tTot = monthrange(2000+year,mInd)[1] *8
		#print tTot
		orgProj = Met.getProjectionInfo()
		print(orgProj.toProj4String())
      
		#Go through all the collected time indices and save the data in Parsed as a grid file.
		for tInd in range(1,k):
			Met.setTimeIndex(inds[tInd])
			for vInd in Var:
				
				#print Met.GetTime(29).ToString("yyyy-MM-dd HH:00")
				#tim= info.GetTime(inds[tInd])
				#print tim.ToString("yyyy-MM-dd HH:00")
				fileName = (cfg["metDataType"] + "_" + resName[vInd] + "_" +
					zstr(mInd)+ "_" + zstr(ds[tInd]) + "_" + shYear
					+"_at_"+zstr(hs[tInd]))
				outFile = (cfg[Lookup[vInd]][0] + "/" + fileName )
				
				outFileDat = outFile + ".dat"
				outFileGrd = outFile + ".grd"
				
				#print outFile
				datu = Met.getGridData(Var[vInd])
				#datu.project(orgProj,ARLproj)
				datu.saveAsSurferASCIIFile(outFileGrd)
				#datu = datu.extract(float(cfg["xmin"]),float(cfg["xmax"]),
					#float(cfg["ymin"]),float(cfg["ymax"]))
				#datu = datu.resample(niceGrid,ResampleMethods.valueOf("Bilinear"))
				#outFile = (cfg[Lookup[vInd]][1] + "/" + fileName + ".grd")
				#datu.saveAsSurferASCIIFile(outFile)

				#Surf.GridMosaic(InGrids= outFile, xMin=xmin, xMax=xmax, yMin=ymin, yMax=ymax, outGrid=outFileDat, OutFmt=srfGridFmtXYZ)
