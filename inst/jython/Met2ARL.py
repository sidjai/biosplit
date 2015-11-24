from org.meteoinfo.data.meteodata import MeteoDataInfo
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

dirIn = sys.argv[1]
exampleNcSlice = sys.argv[2]
dirOut = sys.argv[3]



#take file name and splice into a dictionary of:
#{fullFile: var, ncvariable,level}

varDict = {'pr':'TPP3','tas':'T02M','ps':'PRSS','uas':'U10M','vas':'V10M','husnp':'RELH0m',\
	'hus':'RELH','ua':'UWND','va':'VWND', 'wa':'WWND','zg':'HGTS','ta':'TEMP'}

timeDict = {"x":"hello"}
ncDict = {"x":"hello"}
lvVarDict = {"x":"hello"}
files = os.listdir(dirIn)
for f in filterList('Time',files):
	tok = re.split("Time",f)
	timeInd = re.split(".nc", tok[1])[0]
	timeDict.update({int(timeInd):(dirIn + '/' + f)})
junk = timeDict.pop('x')

Met = MeteoDataInfo()
Met.openNetCDFData(timeDict[1])
NCDI = Met.getDataInfo()

vas = NCDI.getNCVariables()
for vaInd in range(3,vas.size()):
	allVar = re.split("\(", re.split(" ", vas.get(vaInd).toString())[1])[0]
	ncVar = re.sub("[0-9]", "", allVar)
	lv = int(re.sub("[a-zA-Z]", "", allVar))
	
	if ncVar in varDict:
		strVar = String(varDict[ncVar])
		if lv in lvVarDict:
			varList = lvVarDict[lv]
			varList += [strVar]
		else:
			varList = [strVar]

		lvVarDict.update({lv:varList})
		tupAdd = [(allVar.encode('ascii','ignore'), lv,  varDict[ncVar])]
		if ncVar in ncDict:
			idList = ncDict[ncVar]
			idList += tupAdd
		else:
			idList = tupAdd
		ncDict.update({ncVar:idList})
junk = ncDict.pop('x')
junk = lvVarDict.pop('x')

rightFullLvls = sorted(lvVarDict.keys(), reverse=True)
rightFullLvls = [rightFullLvls[-1]] + rightFullLvls[0:-1]

lvDict = dict(zip(range(len(rightFullLvls)), rightFullLvls))
hg2lvDict = dict(zip(rightFullLvls, range(len(rightFullLvls))))


Met = MeteoDataInfo()
exmPath = exampleNcSlice
Met.openNetCDFData(exmPath)
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


tind = 1
for mn in range(1, 2):
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
		ARLDI.levels.add(lind)
		ARLDI.LevelVarList.add(lvVarDict[lvDict[lind]])

	ARLDI.createDataFile(fileOut)

	while mns[tind-1] == mn:
		for ncV, ids in ncDict.iteritems():
			ids.sort(key=itemgetter(1), reverse=True)
			
			for ncV, hg, arlV in ids:
				Met.openNetCDFData(timeDict[tind])
				NCDI = Met.getDataInfo()
				ncData = NCDI.read(String(ncV), [1,1], [133, 108])
				gridData = NCDI.arrayToGrid(ncData, xs, ys)

				dataHead = ARLDI.getDataHead(Met.getProjectionInfo(), 'FNL1', hg2lvDict[hg])
				ARLDI.writeIndexRecord(ts[tind], dataHead)

				label = DataLabel(ts[tind])
				label.setForecast(0)
				label.setGrid(99)
				label.setValue(tind)
				label.setLevel(hg2lvDict[hg])
				label.setVarName(String(arlV))
				ARLDI.levelNum = hg2lvDict[hg]
				ARLDI.writeGridData(label, gridData)



		tind += 1
	ARLDI.setTimeDimension(tDims.extract(startInd, tind, 1))
	ARLDI.closeDataFile()
