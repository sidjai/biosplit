
from org.meteoinfo.data import DataMath
from org.meteoinfo.data.meteodata import MeteoDataInfo
from org.meteoinfo.data import GridData
from org.meteoinfo.data.meteodata.arl import DataLabel
import org.meteoinfo.map.MapView
from org.meteoinfo.projection import ProjectionInfo, Reproject
from org.meteoinfo.global import Extent
from org.meteoinfo.geoprocess.analysis import ResampleMethods
from org.meteoinfo.data.meteodata.arl import ARLDataInfo
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

#Get all the files in the directory

#take file name and splice into a dictionary of:
#{correspondingARLvariable: file, ncvariable,level,sttimeInd,endtimeInd}

varDict = {'pr':'precipitation','tanp':'T02M','ps':'PRSS','uas':'U10M','vas':'V10M','husnp','RELH0m',\
	'hus':'RELH','ua':'UWND','va':'VWND','zg':'HGTS','ta':'TEMP'}
groundVars = ['PRSS','T02M','U10M','V10M']
atmVars = ['HGTS','TEMP','UWND','VWND','WWND','RELH']

ncDict = {"x":"hello"}
#files = os.listdir(cfg['NARCCAPFold'])
files = os.listdir('C:/Users/Siddarta.Jairam/Documents/NARCCAP')
for f in files:
	tok = re.split("[_]",f)
	ncVar = tok[0]
	lv = re.match('p\d+',tok[3])
	if lv is None: lv = 0
	
	#start and end index
		
	ncDict.update({varDict[ncVar]:\
		{'filename':f,'ncVar':ncVar,'stTimeInd':stInd,'endTimeInd':endInd,'level':lv})
print ncDict
Met = MeteoDataInfo()

#---- Set output ARL data info
ARLDI = ARLDataInfo()
#RH = 0.263*p(pa)*spH*1/[exp((17.67*(T−273.15))/(T−29.65))]
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

ncProj = Met.getProjectionInfo()
modelName = 'FNL1'
print ncProj
for var, ids in ncDict.iteritems():
	Met.openNetCDFData(ids['filename'])
	NCDI = Met.getDataInfo()
	for ti in range(ids['startInd'],ids['endInd']):
			Met.setTimeIndex(ti)
   		atime = NCDI.getTimes().get(ti)
    	dataHead = ARLDI.getDataHead(ncProj, modelName, 2)
			ARLDI.writeIndexRecord(atime, dataHead)
			Met.SetLevelIndex(ids['level'])
			ncData = Met.getGridData(ids['ncVarName'])
			if var == 'PRSS' or var == 'WWND':
				ncData = ncData.divide(100)
			if var == 'RELH':
				#ncData = 0.263*p(pa)*ncData.mult(1/(exp((17.67*(T−273.15))/(T−29.65))))
			label = DataLabel(atime)
      label.setLevel(lidx)
      label.setVarName(var)
      label.setGrid(99)
      label.setForecast(0)
      ARLDI.writeGridData(label, ncData)

ARLDI.X = NCDI.getXDimension().getValues()
ARLDI.Y = NCDI.getYDimension().getValues()
ARLDI.closeDataFile()
