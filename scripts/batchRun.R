library(biosplit)

meteoLoc <- "C:/Users/Siddarta.Jairam/Desktop/sid/MeteoInfo1.2"
goldLoc <- "C:/Program Files/Golden Software/Surfer 12"
RLoc <- "C:/Users/Siddarta.Jairam/Documents/R/R-3.2.0"

runScipter <- makeRunFun(goldLoc,'BAS')

doRun <- function(push = ""){
	loadConfig()
	runScript("iterateHYSPLIT.R")
	ncdf2trapdata(cfg$SimOutFold, cfg$TrapLoc, shDoSum = TRUE)
	ncdf2trapdata(cfg$SimOutFold, cfg$TrapLoc, shDoSum = FALSE)
	runScripter("Mothtxt2contour.BAS",cfg$SimOutFold)
	runScripter("Mothtxt2ClassPost.BAS",cfg$SimOutFold)	
	if(nchar(push)>0){
		endTag <-  paste(cfg$year, cfg$runName, sep= '/')
		file.copy(cfg$SimOutFold, paste(push, endTag, sep='/'), overwrite = TRUE)
	}
}

changeConfig("runName","runLimFlightLow",
						 "migCareerLimit",3,
						 "delNightDurFlag",0,
						 "topOfModel",3000)

doRun()


changeConfig("runName","runNightDurDel",
						 "migCareerLimit",99,
						 "topOfModel",3000,
						 "delNightDurFlag",1)
doRun()


changeConfig("runName","runFlightAndNight",
						 "migCareerLimit",3,
						 "topOfModel",3000,
						 "delNightDurFlag",1)
doRun()
