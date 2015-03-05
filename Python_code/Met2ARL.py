#--------------------------------------------------------        
# Author: Yaqiang Wang                                           
# Date: 2012-12-31                                               
# Purpose: Convert GRIB data to ARL data  
# Note: Sample                                                   
#-----------------------------------------------------------
import clr
from System import *
from System.Collections.Generic import *
clr.AddReference("MeteoInfoC.dll")
from MeteoInfoC import *
from MeteoInfoC.Data import *
from MeteoInfoC.Data.MeteoData import *

#---- Set directories
dataDir = "D:\\Temp\\"

#---- Set output data file
outFile = dataDir + 'arl\\test1.arl'

#---- Read a GRIB data file
mydata = MeteoDataInfo()
infile = dataDir + 'grib\\fnl_20110416_00_00'
mydata.OpenGRIBData(infile)

#---- Set output ARL data info
arlDI = ARLDataInfo()

#---- Set variable and level list
gvars = ['Pressure@surface','Temperature@height_above_ground',\
   'U-component_of_wind@height_above_ground','V-component_of_wind@height_above_ground',\
   'Total_precipitation@surface','Geopotential_height@pressure','Temperature@pressure',\
   'U-component_of_wind@pressure','V-component_of_wind@pressure','Vertical_velocity@pressure',\
   'Relative_humidity@pressure']
avars = ['PRSS','T02M','U10M','V10M','TPP6','HGTS','TEMP','UWND','VWND','WWND','RELH']
levels = [0,10,20,30,50,70,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,\
   850,900,925,950,975,1000]
for l in levels:
   arlDI.levels.Add(l)
   if l == 0:
      arlDI.LevelVarList.Add(List[str](['PRSS','T02M','U10M','V10M','TPP6']))
   elif l < 100:
      arlDI.LevelVarList.Add(List[str](['HGTS','TEMP','UWND','VWND']))
   else:
      arlDI.LevelVarList.Add(List[str](['HGTS','TEMP','UWND','VWND','WWND','RELH']))

#---- Write ARL data file
arlDI.CreateDataFile(outFile)
arlDI.X = mydata.GetX()
arlDI.Y = mydata.GetY()
variables = mydata.GetVariables()
tNum = mydata.GetTimeNumber()
for t in range(0, tNum):
   mydata.TimeIndex = t
   atime = mydata.GetTime(t)
   print atime.ToString("yyyy-MM-dd HH")
   aDH = arlDI.GetDataHead(mydata.ProjInfo, 'FNL1', 2)
   arlDI.WriteIndexRecord(atime, aDH)
   lidx = 0
   for l in arlDI.levels:
      print l
      for v in arlDI.LevelVarList[lidx]:
         vName = gvars[avars.index(v)]
         print vName
         if lidx == 0:
            mydata.LevelIndex = lidx
         else:
            variable = mydata.GetVariable(vName)
            nlidx = variable.Levels.IndexOf(l)
            mydata.LevelIndex = nlidx
         gData = mydata.GetGridData(vName)
         if v == 'PRSS' or v == 'WWND':
            gData = gData / 100
         elif v == 'TPP6':
            gData = gData / 1000
         aDL = DataLabel(atime)
         aDL.Level = lidx
         aDL.Variable = v
         aDL.Grid = 99
         aDL.Forecast = 0
         arlDI.WriteGridData(aDL, gData)
      lidx += 1

arlDI.CloseDataFile()

print 'Finished!'