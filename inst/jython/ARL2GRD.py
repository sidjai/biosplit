
from org.meteoinfo.data import DataMath
from org.meteoinfo.data.meteodata import MeteoDataInfo
from org.meteoinfo.data import GridData
import org.meteoinfo.map.MapView
from org.meteoinfo.projection import ProjectionInfo
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

Met = MeteoDataInfo()

Var = {"T":"T02M", 
	"W":"V10M", 
	"S":"SOLT"}
Lookup = {"T":"AirTempFold", 
	"W":"WindFold", 
	"S":"SoilTempFold"}
resName = {"T":"T2M", 
	"W":"VdirWind10M", 
	"S":"Tsoil"}


months = 12
shYear = str(int(cfg["year"])-2000)


#from:lat 25, lon -110
#to:50,-80

xmin = float(cfg["xmin"])
xmax = float(cfg["xmax"])
ymin = float(cfg["ymin"])
ymax = float(cfg["ymax"])
spc = float(cfg["spc"])


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
	
	print(inFile)
	
	if os.path.isfile(inFile):
		#Load the file and project it to NASS
		Met.openARLData(inFile)

		orgProj = Met.getProjectionInfo()
		print(orgProj.toProj4String())
      
		#Go through all the collected time indices and save the data in Parsed as a grid file.
		for tInd in range(1,k):
			Met.setTimeIndex(inds[tInd])
			for vInd in Var:
				
				fileName = (cfg["metDataType"] + "_" + resName[vInd] + "_" +
					zstr(mInd)+ "_" + zstr(ds[tInd]) + "_" + shYear
					+"_at_"+zstr(hs[tInd]))
				outFile = (cfg[Lookup[vInd]][0] + "/" + fileName )
				
				outFileDat = outFile + ".dat"
				outFileGrd = outFile + ".grd"
				
				#save 
				datu = Met.getGridData(Var[vInd])
				datu.saveAsSurferASCIIFile(outFileGrd)
				
