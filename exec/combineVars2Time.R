library(biosplit)
library(ncdf)
library(raster)
library(rgdal)
library(snowfall)
source("/home/computer/Documents/rpkgs/biosplit/biosplitBundle/R/utils.R")
dirVars <- "/home/computer/Documents/NARRCAP"
dirTimes <- "/home/computer/Documents/timeNARCCAP"
wantYr <- 2066
varDict <- list(
	pr = 'TPP3', tas = 'T02M', ps = 'PRSS', uas = 'U10M', vas = 'V10M', 
	husnp = 'RELH0m', hus = 'RELH', ua = 'UWND', 
	va = 'VWND', wa = 'WWND', zg = 'HGTS', ta = 'TEMP')

inFiles <- list.files(dirVars, pattern = ".nc$")
splitFiles <- strsplit(inFiles, "_")

allVar <- vapply(splitFiles, function(vin){
	if(length(vin) < 1 || is.null(varDict[[vin[1]]])) return(rep("", 2))
	
	if(length(vin) > 4){
		return(c(vin[1], gsub("p", "", vin[4])))
	} else {
		return(c(vin[1], 0))
	}
}, rep("e", 2))

goodSet <- nzchar(allVar[1,])
goodVars <- allVar[,goodSet]
rightPaths <- paste(dirVars, inFiles[goodSet], sep = "/")
rightDirs <- paste(dirTimes, goodVars[1,], goodVars[2,], sep = "/")
vapply(rightDirs, function(d){
	if(dir.exists(d)){
		TRUE
	} else {
		dir.create(d, recursive = TRUE, showWarnings = FALSE)
	}
	
},TRUE)

exmnc <- ncdf::open.ncdf(rightPaths[1])
tPos <- convDaysYr(exmnc$dim$time$val)
rightYrSet <- (as.numeric(strftime(tPos, "%Y")) == wantYr)
tPos <- NULL

dims <- list(
	exmnc$dim$yc,
	exmnc$dim$xc)
ncdf::close.ncdf(exmnc)

timeStamps <- paste0("2066Time",
	vapply(1:length(which(rightYrSet)), function(x){ zstr(x, dig = 4) } ,""),
	".asc")

timLen <- length(which(rightYrSet))
ind <- 1
prog <- txtProgressBar(style = 3)
while(ind < length(rightPaths)){
	setTxtProgressBar(prog,ind/length(rightPaths))
	if(length(list.files(rightDirs[ind])) < timLen){
		pathOuts <- paste(rightDirs[ind], timeStamps, sep="/")
		sfInit(parallel=TRUE,cpus=3)
		sfLibrary(raster)
		sfLibrary(rgdal)
		sfExport("rightYrSet", "rightPaths", "goodVars", "pathOuts", "ind")
		junk <- sfSapply(which(rightYrSet), function(ti){
			ras <- raster(rightPaths[ind], band = ti, varname = goodVars[1,ind])
			junk <-  writeRaster(ras, filename = pathOuts[which(which(rightYrSet) == ti)],
				format = "ascii", overwrite = TRUE)
			TRUE
		}, USE.NAMES = FALSE)
		sfStop()
	}
	ind <- ind + 1
	
}
