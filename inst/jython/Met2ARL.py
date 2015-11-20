from org.meteoinfo.data.meteodata import MeteoDataInfo
from org.meteoinfo.data import GridData
from org.meteoinfo.data.meteodata.arl import ARLDataInfo, DataHead, DataLabel
from java.lang import String
from calendar import *
import datetime as dt
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
		ncDict.update({(dirIn + '/' + f):\
		{'arlVar':varDict[ncVar], 'ncVar':ncVar, 'level':lv}})
junk = ncDict.pop('x')
junk = lvVarDict.pop('x')

rightFullLvls = sorted(lvVarDict.keys(), reverse=True)
lvDict = dict(zip(range(len(rightFullLvls)), rightFullLvls))
hg2lvDict = dict(zip(rightFullLvls, range(len(rightFullLvls))))
Met = MeteoDataInfo()

exmPath = list(ncDict.keys())[0]
Met.openNetCDFData(exmPath)
NCDI = Met.getDataInfo()
xs = NCDI.getXDimension().getValues()
ys = NCDI.getYDimension().getValues()
tDims = NCDI.getTimeDimension()
ts = NCDI.getTimes()
mns = [x.getMonth() + 1 for x in ts.iterator()]
inYrs = [x.getYear() + 1900 for x in ts.iterator()]

for mn in range(1, 13):
	print mn
	fileOut = dirOut + "/" + "narccap." + month_abbr[mn].lower() + "66"
	ind = 0
	yrWant = 2066
	while not all([inYrs[ind] == yrWant, mns[ind] == 1]):
		ind += 1
	startInd = ind

	while all([inYrs[ind] == yrWant, mns[ind] == 1]):
		ind += 1
	endInd = ind - 1

	goodDims = tDims.extract(startInd, endInd, 1)
	ARLDI = ARLDataInfo()

	for lind in range(len(rightFullLvls)):
		ARLDI.levels.add(lind)
		ARLDI.LevelVarList.add(lvVarDict[lvDict[lind]])

	ARLDI.createDataFile(fileOut)
	for path, ids in ncDict.iteritems():
		Met.openNetCDFData(path)
		ARLDI.setTimeDimension(goodDims)

		for ti in range(startInd, endInd + 1):
			Met.setTimeIndex(ti)
			ncData = Met.getGridData(ids['ncVar'])
			ARLDI.X = xs
			ARLDI.Y = ys
			atime = ts.get(ti)
			dataHead = ARLDI.getDataHead(Met.getProjectionInfo(), 'FNL1', hg2lvDict[ids['level']])
			ARLDI.writeIndexRecord(atime, dataHead)


			ncData = Met.getGridData(ids['ncVar'])

			label = DataLabel(ts.get(ti))
			label.setForecast(0)
			label.setGrid(99)
			label.setValue(ti - 54)
			label.setLevel(hg2lvDict[ids['level']])
			label.setVarName(String(ids['arlVar']))
			ARLDI.levelNum = hg2lvDict[ids['level']]
			ARLDI.writeGridData(label, ncData)
	ARLDI.closeDataFile()
