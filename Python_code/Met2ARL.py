
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

#outFile = cfg['MetARLFold']
outFile = "C:/Users/Siddarta.Jairam/Documents/ARL packed files/2066/NARRCAP/narrcap2066"
#Get all the files in the directory

#take file name and splice into a dictionary of:
#{correspondingARLvariable: file, ncvariable,level,sttimeInd,endtimeInd}

varDict = {'pr':'TPP3','tas':'T02M','ps':'PRSS','uas':'U10M','vas':'V10M','husnp':'RELH0m',\
	'hus':'RELH','ua':'UWND','va':'VWND','zg':'HGTS','ta':'TEMP'}
groundVars = ['PRSS','T02M','U10M','V10M']
atmVars = ['HGTS','TEMP','UWND','VWND','WWND','RELH']

ncDict = {"x":"hello"}
#inFold = cfg['NARCCAPFold']
inFold = 'C:/Users/Siddarta.Jairam/Documents/NARCCAP/2066'

files = os.listdir(inFold)
for f in filterList('.nc$',files):
	print f
	tok = re.split("[_]",f)
	ncVar = tok[0]
	lv = re.match('p\d+',tok[3])
	if lv is None: 
		lv = 0
	else:
		lv = int(tok[3].strip('p'))
# 		if hgt < 700:
# 			lv = 26-(hgt-50)/50
# 		else:
# 			lv = 13-(hgt-700)/25
	ncDict.update({(inFold + '/' + f):\
		{'arlVar':varDict[ncVar],'ncVar':ncVar,'level':lv}})
junk = ncDict.pop('x')

print ncDict

#start and end index
#for now just posit that only the first year is wanted
startInd = 55
endInd = 2975

Met = MeteoDataInfo()

#---- Set output ARL data info
ARLDI = ARLDataInfo()
#RH = 0.263*p(pa)*spH*1/[exp((17.67*(T-273.15))/(T-29.65))]
#---- Set variable and level list
ncvars = ['Pressure_surface','Temperature_height_above_ground',\
	'u-component_of_wind_height_above_ground','v-component_of_wind_height_above_ground',\
	'Geopotential_height_isobaric','Temperature_isobaric',\
	'u-component_of_wind_isobaric','v-component_of_wind_isobaric','Vertical_velocity_pressure_isobaric',\
	'Relative_humidity_isobaric']

levels = [0,1000,975,950,925,900,875,850,825,800,775,750,700,\
	650,600,550,500,450,400,350,300,250,225,200,175,150,\
	125,100,70,50,30,20,10,7,5,3,2,1]
for lv in levels:
	ARLDI.levels.add(lv)
	if lv == 0:
		ARLDI.LevelVarList.add(groundVars)
	else:
		ARLDI.LevelVarList.add(atmVars)

#---- Write ARL data file

ARLDI.createDataFile(outFile)


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
		dataHead = ARLDI.getDataHead(Met.getProjectionInfo(), modelName, 2)
		ARLDI.writeIndexRecord(atime, dataHead)
		Met.setLevelIndex(ids['level'])
		ncData = Met.getGridData(ids['ncVar'])
		if ids['arlVar'] == 'PRSS' or ids['arlVar'] == 'WWND':
			ncData = ncData.divide(100)
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
