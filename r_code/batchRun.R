
realWd <- gsub("/r_code","",ifelse(!grepl("Moth",getwd()),dirname(sys.frame(1)$ofile),getwd()))

source(paste(realWd,"r_code","DataRoadmap.R",sep='/'))

changeConfig <- function(varName,newVal){
	path <- paste(realWd,"config",sep='/')
	newCon <- befCon <- readLines(path)
	
	newCon[charmatch(varName,befCon)] <- paste(varName,newval,sep=' = ')
	writeLines(newCon,path)
}

runScript("iterateHYSPLIT.R")

changeConfig("TopOfModel",3000)
changeConfig("delNightDurFlag",1)

runScript("loadConfig.R")
runScript("iterateHYSPLIT.R")