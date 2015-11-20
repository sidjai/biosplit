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
dirOut = sys.argv[2]



#take file name and splice into a dictionary of:
#{fullFile: var, ncvariable,level}

varDict = {'pr':'TPP3','tas':'T02M','ps':'PRSS','uas':'U10M','vas':'V10M','husnp':'RELH0m',\
	'hus':'RELH','ua':'UWND','va':'VWND', 'wa':'WWND','zg':'HGTS','ta':'TEMP'}

ncDict = {"x":"hello"}
lvVarDict = {"x":"hello"}
files = os.listdir(dirIn)
for f in filterList('.nc$',files):
	tok = re.split("[_]",f)
	ncVar = tok[0]
	if len(tok) > 3:
		lv = re.match('p\d+',tok[3])
	if ncVar in varDict:
		if lv is None:
			lv = 0
		else:
			lv = int(tok[3].strip('p'))

		strVar = String(varDict[ncVar])
		if lv in lvVarDict:
			varList = lvVarDict[lv]
			varList += [strVar]
		else:
			varList = [strVar]

		lvVarDict.update({lv:varList})

		tupAdd = [((dirIn + '/' + f), lv)]
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
exmPath = ncDict.values()[0][0][0]
Met.openNetCDFData(exmPath)
NCDI = Met.getDataInfo()
xs = NCDI.getXDimension().getValues()
ys = NCDI.getYDimension().getValues()
tDims = NCDI.getTimeDimension()
ts = NCDI.getTimes()
mns = [x.getMonth() + 1 for x in ts.iterator()]
inYrs = [x.getYear() + 1900 for x in ts.iterator()]

yrWant = 2066
tind = 0

for mn in range(1, 2):
	print mn
	fileOut = dirOut + "/" + "narccap." + month_abbr[mn].lower() + "66"
	if os.path.isfile(fileOut):
		os.remove(fileOut)


	while not all([inYrs[tind] == yrWant, mns[tind] == mn]):
		tind += 1
	startInd = tind

	ARLDI = ARLDataInfo()
	ARLDI.X = xs
	ARLDI.Y = ys

	for lind in range(len(rightFullLvls)):
		ARLDI.levels.add(lind)
		ARLDI.LevelVarList.add(lvVarDict[lvDict[lind]])

	ARLDI.createDataFile(fileOut)

	while all([inYrs[tind] == yrWant, mns[tind] == mn]):
		for ncV, ids in ncDict.iteritems():
			ids.sort(key=itemgetter(1), reverse=True)
			for path, hg in ids:
				Met.openNetCDFData(path)
				Met.setTimeIndex(tind)
				ncData = Met.getGridData(ncV)
				
				dataHead = ARLDI.getDataHead(Met.getProjectionInfo(), 'FNL1', hg2lvDict[hg])
				ARLDI.writeIndexRecord(ts.get(tind), dataHead)

				label = DataLabel(ts.get(tind))
				label.setForecast(0)
				label.setGrid(99)
				label.setValue(tind - 54)
				label.setLevel(hg2lvDict[hg])
				label.setVarName(String(varDict[ncV]))
				ARLDI.levelNum = hg2lvDict[hg]
				ARLDI.writeGridData(label, ncData)
				


		tind += 1
	ARLDI.setTimeDimension(tDims.extract(startInd, tind, 1))
	ARLDI.closeDataFile()
