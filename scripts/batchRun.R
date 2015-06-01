library(biosplit)

meteoLoc <- "C:/Users/Siddarta.Jairam/Desktop/sid/MeteoInfo1.2"
goldLoc <- "C:/Program Files/Golden Software/Surfer 12"
RLoc <- "C:/Users/Siddarta.Jairam/Documents/R/R-3.2.0"

runScripter <- makeRunFun(goldLoc,'BAS')
runRscript <- makeRunFun(RLoc,"R")

cfg <- loadConfig()

boBox <- raster::extent(cfg$xmin, cfg$xmax, cfg$ymin, cfg$ymax)
cropGrid <- raster::raster(boBox,
													 crs = cfg$cropProj,resolution = cfg$spc)
niceGrid <- raster::projectExtent(cropGrid,cfg$niceProj)


doRun <- function(push = ""){
	cfg <- loadConfig()
	runRscript("iterateHYSPLIT.R")
	predObv <- ncdf2trapgrid(cfg$SimOutFold,
													 "C:/Users/Siddarta.Jairam/Documents/Documentation/Result Files/firstOccTrap.grd",
													 paste(cfg$SimOutFold, "Sim-TrapFirstOcc.nc",sep='/'),
													 niceGrid
	)
	#The spatial average distance between the weeks
	notes <- sprintf("The spatial average of the simulated distance away from the trap values is %.2f wks",
								 	mean(abs(predObv[,,]),na.rm=TRUE))
	ncdf2trapdata(cfg$SimOutFold, cfg$TrapLoc[paste0(cfg$year)], shDoSum = TRUE, notes = notes)
	ncdf2trapdata(cfg$SimOutFold, cfg$TrapLoc[paste0(cfg$year)], shDoSum = FALSE, notes = notes)
	runScripter("Mothtxt2contour.BAS",cfg$SimOutFold)
	runScripter("Mothtxt2ClassPost.BAS",cfg$SimOutFold)	
	if(nchar(push)>0){
		file.copy(cfg$SimOutFold, 
							paste(push, cfg$year, sep='/'),
							overwrite = TRUE,
							recursive = TRUE)
	}
}

pushLoc <- 'X:/2 WESTBROOK/Sid/Hysplit Out Moth table'

#changeConfig("runName","runBaseNightandFlight",
#						 "migCareerLimit",3)
#doRun(pushLoc)


changeConfig("runName","runTOcueTemp",
						 "tempTOThres", 10)

cfg <- loadConfig()
collectAprioriVars(cfg)
doRun(pushLoc)


changeConfig("runName","runTOcuePrec",
						 "tempTOThres",-9999,
						 "precTOThres",0.01)

collectAprioriVars(cfg)
doRun(pushLoc)
