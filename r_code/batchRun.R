
realWd <- gsub("/r_code","",ifelse(!grepl("Moth",getwd()),dirname(sys.frame(1)$ofile),getwd()))

source(paste(realWd,"r_code","DataRoadmap.R",sep='/'))

changeConfig <- function(...){
	args <- list(...)
	varNames <- c(args[seq(1,length(args),2)],recursive=TRUE)
	newVals <- args[seq(2,length(args),2)]
	path <- paste(realWd,"config.txt",sep='/')
	befCon <- readLines(path)
	inds <- charmatch(varNames,befCon)
	invalidNames <- varNames[is.na(inds),drop=FALSE]
	if (length(invalidNames)>0){
		stop(paste(paste(invalidNames,collapse=','), "are not a valid variables in Config"))
	}
	befCon[inds] <- paste(varNames,newVals,sep=' = ')
	writeLines(befCon,path)
}

changeConfig("runName","runAbsFlightLimit",
						 "migCareerLimit",3,
						 "delNightDurFlag",0,
						 "topOfModel",3000)

runScript("loadConfig.R")
runScript("iterateHYSPLIT.R")

changeConfig("runName","runNightDurDel",
						 "migCareerLimit",99,
						 "topOfModel",3000,
						 "delNightDurFlag",1)

runScript("loadConfig.R")
runScript("iterateHYSPLIT.R")

changeConfig("runName","runFlightAndNight",
						 "migCareerLimit",3,
						 "topOfModel",3000,
						 "delNightDurFlag",1)

runScript("loadConfig.R")
runScript("iterateHYSPLIT.R")
