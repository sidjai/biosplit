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
	runBiosplit(cfg)
	predObv <- ncdf2trapgrid(cfg$SimOutFold,
													 "C:/Users/Siddarta.Jairam/Documents/Documentation/Result Files/2013firstOccTrap.grd",
													 paste(cfg$SimOutFold, "Sim-TrapFirstOcc.nc",sep='/')
													 )
	
	notes <- sprintf("The spatial average of the simulated distance away from the trap values is %.2f wks",
								 	mean(abs(predObv[,,]),na.rm=TRUE))
	ncdf2trapdata(cfg$SimOutFold, cfg$TrapLoc[paste0(cfg$year)], shDoSum = TRUE, notes = notes)
	ncdf2trapdata(cfg$SimOutFold, cfg$TrapLoc[paste0(cfg$year)], shDoSum = FALSE, notes = notes)
	runScripter("Mothtxt2contour.BAS",cfg$SimOutFold)
	runScripter("Mothtxt2ClassPost.BAS",cfg$SimOutFold)
	runScripter("ncdf2contour.BAS",cfg$SimOutFold)
	
	if(nchar(push)>0){
		file.copy(cfg$SimOutFold, 
							paste(push, cfg$year, sep='/'),
							overwrite = TRUE,
							recursive = TRUE)
	}
}

pushLoc <- 'X:/2 WESTBROOK/Sid/Hysplit Out Moth table'



unitsDict <- list( T02M = 'K', V10M = 'm/s North', TPP3 = 'm', SOLT = 'K')

#doRun(pushLoc)

changeConfig("runName","runMultYearFv020",
						 "delNightDurFlag", 0,
						 "year", 2012)
cfg <- loadConfig()
doRun(pushLoc)


changeConfig("runName","runMultYearDv020",
						 "year", 2013,
						 "delNightDurFlag", 1)

cfg <- loadConfig()

doRun(pushLoc)


rmarkdown::render(system.file("docs", "AutoReport.rmd", package = "biosplit"),
									params = list(
										runs = rep("runMultYearDv020",4),
										years = c(2011, 2012, 2013, 2014)
									)
)

file.copy(system.file("docs", "AutoReport.docx", package = "biosplit"),
					"C:/Users/Siddarta.Jairam/Documents/Documentation/Result Files/AutoReportMultiD.docx",
					overwrite = TRUE)

					
rmarkdown::render(system.file("docs", "AutoReport.rmd", package = "biosplit"),
									params = list(
										runs = rep("runMultYearFv020",4),
										years = c(2011, 2012, 2013, 2014)
									)
)

file.copy(system.file("docs", "AutoReport.docx", package = "biosplit"),
					"C:/Users/Siddarta.Jairam/Documents/Documentation/Result Files/AutoReportMultiF.docx",
					overwrite = TRUE)



