
from org.meteoinfo.data import DataMath
from org.meteoinfo.data.meteodata import MeteoDataInfo
from org.meteoinfo.data import GridData
from org.meteoinfo.data.meteodata.arl import ARLDataInfo, DataHead, DataLabel
import org.meteoinfo.map.MapView
from org.meteoinfo.projection import ProjectionInfo, Reproject
from org.meteoinfo.global import Extent
from org.meteoinfo.geoprocess.analysis import ResampleMethods
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
groundVars = []
atmVars = []
levels = []

files = os.listdir(dirIn)
for f in filterList('.nc$',files):
	tok = re.split("[_]",f)
	ncVar = tok[0]
	if len(tok) > 3:
		lv = re.match('p\d+',tok[3])
	if ncVar in varDict:
		if lv is None:
			lv = 0
			groundVars += [varDict[ncVar]]
		else:
			lv = int(tok[3].strip('p'))
			if lv not in levels:
				levels += [lv]
			if varDict[ncVar] not in atmVars:
				atmVars += [varDict[ncVar]]
		ncDict.update({(dirIn + '/' + f):\
		{'arlVar':varDict[ncVar], 'ncVar':ncVar, 'level':lv}})
junk = ncDict.pop('x')
levels += [0]

#start and end index
#for now just posit that only the first year is wanted


Met = MeteoDataInfo()

#---- Set output ARL data info
#RH = 0.263*p(pa)*spH*1/[exp((17.67*(T-273.15))/(T-29.65))]
# Write ARL data file


exmPath = list(ncDict.keys())[0]
Met.openNetCDFData(exmPath)
NCDI = Met.getDataInfo()
xs = NCDI.getXDimension().getValues()
ys = NCDI.getYDimension().getValues()
ts = NCDI.getTimes()
mns = [x.getMonth() + 1 for x in ts.iterator()]
inYrs = [x.getYear() + 1900 for x in ts.iterator()]

for mn in range(1, 13):
	print mn
	ind = 0
	yrWant = 2066
	while not all([inYrs[ind] == yrWant, mns[ind] == mn]):
		ind += 1
	startInd = ind
	
	while all([inYrs[ind] == yrWant, mns[ind] == mn]):
		ind += 1
	endInd = ind - 1 
	
	
	
	ARLDI = ARLDataInfo()
	for lv in levels:
		ARLDI.levels.add(lv)
		if lv == 0:
			ARLDI.LevelVarList.add(groundVars)
		else:
			ARLDI.LevelVarList.add(atmVars)
	ARLDI.createDataFile(dirOut + "/" + "narccap." + month_abbr[mns[startInd +2]].lower() + "66")
	
	for path, ids in ncDict.iteritems():
		
		Met.openNetCDFData(path)
		NCDI = Met.getDataInfo()
		ARLDI.X = xs
		ARLDI.Y = ys
		for ti in range(startInd,endInd):
			Met.setTimeIndex(ti)
			atime = ts.get(ti)
			dataHead = ARLDI.getDataHead(Met.getProjectionInfo(), 'FNL1', 2)
			ARLDI.writeIndexRecord(atime, dataHead)
			Met.setLevelIndex(ids['level'])
			ncData = Met.getGridData(ids['ncVar'])
			if ids['arlVar'] == 'PRSS' or ids['arlVar'] == 'WWND':
				ncData = ncData.div(100)
			if ids['arlVar'] == 'RELH':
				ncData = ncData
				#ncData = 0.263*p(pa)*ncData.mult(1/(exp((17.67*(T-273.15))/(T-29.65))))
		label = DataLabel(atime)
		label.setLevel(ids['level'])
		label.setVarName(ids['arlVar'])
		label.setGrid(99)
		label.setForecast(0)
	ARLDI.writeGridData(label, ncData)
	ARLDI.closeDataFile()
