
from org.meteoinfo.data import DataMath
from org.meteoinfo.data.meteodata import MeteoDataInfo
from org.meteoinfo.data import GridData
import org.meteoinfo.map.MapView
from org.meteoinfo.projection import ProjectionInfo
from calendar import monthrange
import os.path
import sys
import os
import re

dirIn = sys.argv[1]
dirOut = sys.argv[2]
wantVars = sys.argv[3:]

takeOffDict = {'TPP3': 5, 'V10M': 5, 'T02M': 4}

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

# iterate through the days and get the days, hours and the time index
def getTimes(firstIndChange,yr, monthNum):
	dayVec = range(1,monthrange(yr, monthNum)[1] + 1)
	k = 1
	inds=[None]*250
	hs=[None]*250
	ds=[None]*250
	add = 0
	
	# If you want midnight UTC value it is in the next month file for the last day
	midnightFlag = firstIndChange * 3 + 9 == 24
	if(midnightFlag):
		add = -1
		if(monthNum >1):
			hs[1] = 19
			ds[1] = zstr(monthNum-1) + "_" + zstr(monthrange(yr, monthNum-1)[1])
			inds[1] = 0
			k += 1
	
	for di in dayVec:
		#conversion between UTC and CDT (during summer which is target time): -5
		#index 1 is at midnight UTC so starting indice for 4AM CDT (9 UTC) is 4
		#The steps are defined by the different variables (temp is 3 and 5 indices)
		
		hs[k] = 4
		ds[k] = zstr(monthNum) + "_" + zstr(di)
		if k != 1:
			inds[k] = inds[k-1] + (8-firstIndChange)
		else:
			inds[k] = 3
			
		k+=1
		hs[k] = hs[k-1] + (3 * firstIndChange)
		ds[k] = zstr(monthNum) + "_" + zstr(di)
		inds[k] = inds[k-1] + firstIndChange
		k+=1
	
	# On New Year's Eve you can't do Midnight UTC because its in a different file 
	# structure so do the one before.
	if(monthNum == 12 and midnightFlag):
		hs[k-1] = hs[k-1] - 3
		inds[k-1] = inds[k-1] - 1
		add = 0
	
	LI = k + add
	return hs[1:LI], ds[1:LI], inds[1:LI]

# Go through the months
for mInd in [d+1 for d in range(months)]:
	
	inFile = (dirIn + "/" + metType + "." +
		(month_abbr[mInd]).lower() + shYear)
		
	
	print month_abbr[mInd]
	
	if not os.path.isfile(inFile):
		exit("File:" & inFile & "Does not exist")
	
	#Load the file
	Met.openARLData(inFile)
    
	#Go through all the collected time indices and save the data in Parsed as a grid file.
	for var in wantVars:
		
		hs, ds, inds = getTimes(takeOffDict[var], int(yearstr), mInd)
		for tInd in range(len(inds)):
			
			Met.setTimeIndex(inds[tInd])
			pathOut = (dirOut + var + "/Parsed/" + metType + "_" + var +"_" +
				ds[tInd] + "_" + shYear
				+"_at_" + zstr(hs[tInd]) + ".grd")
			
			#save
			datu = Met.getGridData(var)
			datu.saveAsSurferASCIIFile(pathOut)
