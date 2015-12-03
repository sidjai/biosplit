from org.meteoinfo.data.meteodata import MeteoDataInfo, Dimension
from org.meteoinfo.data import GridData
from org.meteoinfo.data.meteodata.arl import ARLDataInfo, DataHead, DataLabel
from java.lang import String
from calendar import *
import datetime as dt
from operator import itemgetter
import os.path
import sys
import os
import re

def filterList(flter,lst):
	result = []
	for l in lst:
		match = re.search(flter,l)
		if match:
			result += [l]
	return result
	
def zstr(num, dig):
	#return the string of the number including a zero at beg
	guess = str(num)
	res = len(guess)
	if not res == dig:
		guess = ("0" * (dig-res)) + guess
	return guess

dirIn = sys.argv[1]
exampleNcSlice = sys.argv[2]
dirOut = sys.argv[3]



#take file name and splice into a dictionary of:
#{fullFile: var, ncvariable,level}

varDict = {'pr':'TPP3','tas':'T02M','ps':'PRSS','uas':'U10M','vas':'V10M','husnp':'RELH0m',\
	'hus':'RELH','ua':'UWND','va':'VWND', 'wa':'WWND','zg':'HGTS','ta':'TEMP'}
arl2ncDict = {str(v): str(k) for k, v in varDict.items()}

ncDict = {"x":"hello"}
lvVarDict = {"x":"hello"}
files = os.listdir(dirIn)

for va in os.walk(dirIn).next()[1]:
	if va in varDict:
		vLev = os.walk(os.path.join(dirIn,va)).next()[1]
		for lv in vLev:
			lv = int(lv)
			strVar = String(varDict[va])
			if lv in lvVarDict:
				varList = lvVarDict[lv]
				varList += [strVar]
			else:
				varList = [strVar]
	
			lvVarDict.update({lv:varList})
			tupAdd = (lv, varDict[va])
			if va in ncDict:
				idList = ncDict[va]
				idList += [tupAdd]
			else:
				idList = [tupAdd]
			ncDict.update({va:idList})

junk = ncDict.pop('x')
junk = lvVarDict.pop('x')


Met = MeteoDataInfo()

rightFullLvls = sorted(lvVarDict.keys(), reverse=True)
rightFullLvls = [rightFullLvls[-1]] + rightFullLvls[0:-1]

lvDict = dict(zip(range(len(rightFullLvls)), rightFullLvls))
hg2lvDict = dict(zip(rightFullLvls, range(len(rightFullLvls))))

Met = MeteoDataInfo()
exmPath = exampleNcSlice
Met.openNetCDFData(exmPath)
proj = Met.getProjectionInfo()
NCDI = Met.getDataInfo()

xs = NCDI.getXDimension()
ys = NCDI.getYDimension()
tDims = NCDI.getTimeDimension()
ts = NCDI.getTimes()

yrWant = 2066
spl = 1
rea = 1
realTs = [None]
stIndYr = None
endIndYr = None
while spl < ts.size():
	if(ts.get(spl).getYear() + 1900 == yrWant):
		realTs += [ts.get(spl)]

		endIndYr = spl
		if stIndYr is None:
			stIndYr = spl
	spl += 1
ts = realTs
tDims = tDims.extract(stIndYr, endIndYr, 1)

mns = [x.getMonth() + 1 for x in ts[1:]]

tDim = Dimension(tDims.getDimType(), 1, 1, tDims.getDimLength())
tDim.setDimName("time")
tDim.setDimId(3)
tDim.setValues(range(1, tDims.getDimLength()+1))

tind = 1
for mn in range(1, 13):
	print mn
	fileOut = dirOut + "/" + "narccap." + month_abbr[mn].lower() + "66"
	if os.path.isfile(fileOut):
		os.remove(fileOut)


	while not mns[tind-1] == mn:
		tind += 1
	startInd = tind

	ARLDI = ARLDataInfo()
	ARLDI.X = xs.getValues()
	ARLDI.Y = ys.getValues()
	
	for lind in range(len(rightFullLvls)):
		ARLDI.levels.add(lvDict[lind])
		ARLDI.LevelVarList.add(lvVarDict[lvDict[lind]])
		
	ARLDI.createDataFile(fileOut)

	while tind <= len(mns) and mns[tind-1] == mn:
		dataHead = ARLDI.getDataHead(proj, 'NRCP', 1)
		ARLDI.writeIndexRecord(ts[tind], dataHead)
		for hg in rightFullLvls:
			for arlV in lvVarDict[hg]:
				arlV = str(arlV)
				ncV = arl2ncDict[arlV]
				
				fileName = str(yrWant) + "Time"+ zstr(tind, 4) + ".asc"
				indvPath = os.path.join(dirIn, ncV, str(hg), fileName)
				Met.openASCIIGridData(indvPath)
				asData = Met.getGridData()


				label = DataLabel(ts[tind])
				label.setForecast(0)
				label.setGrid(99)
				label.setValue(tind)
				label.setLevel(hg2lvDict[hg])
				label.setVarName(String(arlV))
				ARLDI.levelNum = hg2lvDict[hg]
				ARLDI.writeGridData(label, asData)
				



		tind += 1
	ARLDI.setTimeDimension(tDim.extract(startInd, tind - 2, 1))
	ARLDI.closeDataFile()
