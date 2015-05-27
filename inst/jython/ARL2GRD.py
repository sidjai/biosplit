
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

dirIn = sys.argv[1]
dirOut = sys.argv[2]
wantVars = sys.argv[3:]

takeOffDict = {'TPP3': 4, 'V10M': 4, 'T02M': 2}

Met = MeteoDataInfo()

yearstr = re.findall('\d{4}',dirIn)[0]
metType = os.path.split(dirIn)[1]

months = 12 
shYear = str(int(yearstr)-2000)

dirOut = (dirOut + '/' + yearstr + '/')

def zstr(num):
	#return the string of the number including a zero at beg
	if num<10:
		val = "0" + str(num)
	else:
		val = str(num)
	return val

def getTimes(indStart,numDays):
	k = 1
	inds=[None]*250
	hs=[None]*250
	ds=[None]*250
	# iterate through the days and get the days, hours and the time index
	# Grabs the index for midnight and 3PM as the min and max temps for the day
	for di in range(1,numDays+1):
		hs[k]=-8 + indStart * 3
		ds[k]=di
		if k!=1:
			inds[k] = inds[k-1]+3
		else:
			inds[k] = indStart
			
		k+=1
		hs[k] = hs[k-1] + 15
		ds[k] = di
		inds[k] = inds[k-1]+5
		k+=1
	return hs, ds, inds
# Go through the months
for mInd in [d+1 for d in range(months)]:
	
	inFile = (dirIn + "/" + metType + "." +
		(month_abbr[mInd]).lower() + shYear)
		
	
	print(inFile)
	
	if os.path.isfile(inFile):
		#Load the file
		Met.openARLData(inFile)
      
		#Go through all the collected time indices and save the data in Parsed as a grid file.
		for var in wantVars:
			hs, ds, inds = getTimes(takeOffDict[var], monthrange(int(yearstr),mInd)[1])
			for tInd in range(1,len(inds)):
				Met.setTimeIndex(inds[tInd])
				pathOut = (dirOut + var + "/Parsed/" + metType + "_" + var +"_" +
					zstr(mInd)+ "_" + zstr(ds[tInd]) + "_" + shYear
					+"_at_" + zstr(hs[tInd]) + ".grd")
				
				#save 
				datu = Met.getGridData(var)
				datu.saveAsSurferASCIIFile(pathOut)
				
