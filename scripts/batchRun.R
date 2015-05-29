library(biosplit)

meteoLoc <- "C:/Users/Siddarta.Jairam/Desktop/sid/MeteoInfo1.2"
goldLoc <- "C:/Program Files/Golden Software/Surfer 12"
RLoc <- "C:/Users/Siddarta.Jairam/Documents/R/R-3.2.0"

runScripter <- makeRunFun(goldLoc,'BAS')
runRscript <- makeRunFun(RLoc,"R")

doRun <- function(push = ""){
	cfg <- loadConfig()
	runRscript("iterateHYSPLIT.R")
	ncdf2trapdata(cfg$SimOutFold, cfg$TrapLoc[paste0(cfg$year)], shDoSum = TRUE)
	ncdf2trapdata(cfg$SimOutFold, cfg$TrapLoc[paste0(cfg$year)], shDoSum = FALSE)
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

doRun(pushLoc)


changeConfig("runName","runTOcueTemp",
						 "migCareerLimit",99,
						 "tempTOThres", 10)
cfg <- loadConfig()
collectAprioriVars(cfg)
doRun(pushLoc)


changeConfig("runName","runTOcuePrec",
						 "tempTOThres",-9999,
						 "precTOThres",0.01)

collectAprioriVars(cfg)
doRun(pushLoc)
