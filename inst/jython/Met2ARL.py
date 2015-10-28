
from org.meteoinfo.data import DataMath
from org.meteoinfo.data.meteodata import MeteoDataInfo
from org.meteoinfo.data import GridData
from org.meteoinfo.data.meteodata.arl import ARLDataInfo, DataHead, DataLabel
import org.meteoinfo.map.MapView
from org.meteoinfo.projection import ProjectionInfo, Reproject
from org.meteoinfo.global import Extent
from org.meteoinfo.geoprocess.analysis import ResampleMethods
from calendar import *
import os.path
import sys
import os
import re

def filterList(filter,list):
	result = []
	for l in list:
		match = re.search(filter,l)
		if match:
			result += [l]
	return result

dirIn = sys.argv[1]
pathOut = sys.argv[2]

#pathOut = "C:/Users/Siddarta.Jairam/Documents/ARL packed files/2066/NARRCAP/narrcap2066"
#dirIn = "C:/Users/Siddarta.Jairam/Documents/NARCCAP/2066"

#take file name and splice into a dictionary of:
#{fullFile: var, ncvariable,level}

varDict = {'pr':'TPP3','tas':'T02M','ps':'PRSS','uas':'U10M','vas':'V10M','husnp':'RELH0m',\
	'hus':'RELH','ua':'UWND','va':'VWND', 'wa':'WWND','zg':'HGTS','ta':'TEMP'}
#groundVars = ['PRSS','T02M','U10M','V10M']
#atmVars = ['HGTS','TEMP','UWND','VWND','WWND','RELH']

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
print levels
print atmVars
print groundVars

#start and end index
#for now just posit that only the first year is wanted
startInd = 55
endInd = 2975

Met = MeteoDataInfo()

#---- Set output ARL data info
ARLDI = ARLDataInfo()
#RH = 0.263*p(pa)*spH*1/[exp((17.67*(T-273.15))/(T-29.65))]

for lv in levels:
	ARLDI.levels.add(lv)
	if lv == 0:
		ARLDI.LevelVarList.add(groundVars)
	else:
		ARLDI.LevelVarList.add(atmVars)

# Write ARL data file

ARLDI.createDataFile(pathOut)


modelName = 'NARRCAP'

for path, ids in ncDict.iteritems():
	print ids['ncVar']
	Met.openNetCDFData(path)
	NCDI = Met.getDataInfo()
	ARLDI.X = NCDI.getXDimension().getValues()
	ARLDI.Y = NCDI.getYDimension().getValues()
	for ti in range(startInd,endInd):
		Met.setTimeIndex(ti)
		atime = NCDI.getTimes().get(ti)
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
