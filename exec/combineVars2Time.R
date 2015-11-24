library(biosplit)
library(ncdf)

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
	if(length(vin) < 1 || is.null(varDict[[vin[1]]])) return("")
	
	if(length(vin) > 4){
		return(gsub("p", vin[1], vin[4]))
	} else {
		return(paste0(vin[1], 0))
	}
}, "e")

goodSet <- nzchar(allVar)

goodVars <- vapply(splitFiles[goodSet], function(vin){ vin[1] }, "e")
rightPaths <- paste(dirVars, inFiles[goodSet], sep = "/")

exmnc <- ncdf::open.ncdf(rightPaths[1])
tPos <- biosplit:::convDaysYr(exmnc$dim$time$val)
rightYrSet <- (as.numeric(strftime(tPos, "%Y")) == wantYr)
tPos <- NULL

dims <- list(
	exmnc$dim$yc,
	exmnc$dim$xc)
ncdf::close.ncdf(exmnc)

ind <- 1
prog <- txtProgressBar(style = 3)
while(ind < length(rightPaths)){
	setTxtProgressBar(prog,ind/length(rightPaths))
	nc <- ncdf::open.ncdf(rightPaths[ind])
	
	for(tind in which(rightYrSet)){
		fileOut <- paste(dirTimes, paste0("2066Time", tind, ".nc"), sep="/")
		dat <- ncdf::get.var.ncdf(nc,
			varid = goodVars[ind], 
			start = c(1, 1, tind), 
			count = c(-1, -1, 1))
		ncVar <- ncdf::var.def.ncdf(allVar[goodSet][ind], 'Thing', dims, 9999)
		
		
		if(ind == 1 && !file.exists(fileOut)){
			onc <- create.ncdf(fileOut, ncVar)
			put.var.ncdf(onc, allVar[goodSet][ind], dat)
			
		} else {
			onc <- open.ncdf(fileOut, write = TRUE)
			if(!allVar[goodSet][ind] %in% names(onc$var)){
				var.add.ncdf(onc, ncVar)
			} else {
				ind <- length(names(onc$var))
				ncdf::close.ncdf(nc)
				nc <- ncdf::open.ncdf(rightPaths[ind])
			}
		}
		close.ncdf(onc)
		
	}
	tind <- tind + 1
	ncdf::close.ncdf(nc)
	
}



