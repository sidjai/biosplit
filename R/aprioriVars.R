#'A wrapper for aprioriVars
#'
#'Abstracts away the folder selection which allows a developer
#'	to insert more variables with easy to use names along with 
#'	the equations directly in aprioriVars
#'@param config The configuration object returned by loadConfig 
#'	or from loading the cfg.RData in the config.txt location
#'@return Works as a byproduct to save the ncdf file
#'@export
collectAprioriVars <- function(config){
	# The data folders inserted as a list. 
	# The names are the names used in apriori using the form dat$<name>$<H,L>[xi,yi,ti]
	dirTrees <- list(C = config$CropFold,
									 Air = config$T02MFold, 
									 Wind = config$V10MFold, 
									 Prec = config$TPP3Fold)
	aprioriVars(dirTrees,
							config$AprioriLoc,
							config$plantTimes,
							config$harvestTimes, 
							config$CornThres,
							config$TGDDbaseCrop,
							config$TGDDbaseFAW,
							config$windTOThres,
							config$tempTOThres,
							config$precTOThres)
}

#' Turn the data into abstracted variables for the model.
#' 
#' For computer time, memory and simplicity it is easier to deal with 
#' the aggregated variables in the actual biological model than the actual data.
#' The main variable calculated is the growth of corn over the year.
#' Other variables include Fall Army worm growth and wind mangitude northward.
#' Other variables that can be included is the decision to fly in the form of
#' a set of logicals including precipitation, biotic factors or temperature.
#' The parameters are saved with the variables as global attributes in the 
#' final ncdf file. This file is loaded at the begining of iterateHYSPLIT
#' 
#' @param dirTrees a list of all the character vectors of where the data lives
#' @param pathOut the path where the ncdf file will be located
#' @param plantTimes a vector of length 4, that defines the three sections of
#'  corn development. The first two numbers are latitudes that divide the 
#'  country into three parts. The second two numbers are the planting dates of 
#'  at those latitudes with the dates being interpolated between the two points 
#'  and held constant anywhere else.
#' @param harvestTimes a vector with the same stucture as panting dates but for
#'  the time of harvest.
#' @param CornThres a hectare amount of recognizable corn in one grid cell.
#' 	Represents a minimum desnsity of hectares /1600 km^2
#' @param TGDDbaseCrop The base temperature value for the degree day
#'  calculations of corn growth in units of celcius. 
#' @param TGDDbaseFAW The base temperature value for the degree day
#'  calculations of insect growth in units of celcius.
#' @param windTOThres If the wind speed (m/s) is greater than this value then don't fly
#' @param tempTOThres If the temp (degC) is lower than this value then don't fly
#' @param precTOThres If the precipitation (3hr acc. m) is greater than this value then don't fly
#' @return Works as a byproduct of the saving of the ncdf file.
#' @details The units for the ouput values are set up to reduce the size of the 
#' 	data. The degree day values are done in 0.1*C*day or 0.1*F*day so
#'  they can be stored as integers.
#' @import ncdf
#' @export
aprioriVars <- function(dirTrees, 
												pathOut,
												plantTimes,
												harvestTimes, 
												CornThres,
												TGDDbaseCrop,
												TGDDbaseFAW,
												windTOThres,
												tempTOThres,
												precTOThres){
	bio <- as.list(match.call())
	dat <- lapply(dirTrees, function(tree){
		cornFlag <- grepl("Crop", tree[1], ignore.case = TRUE)
		nc <- getNcFiles(tree, cornFlag)
		if(cornFlag){
			return(get.var.ncdf(nc))
		} else {
			return(getVar(nc))
		}
		close.ncdf(nc)
	})
												
	cornNc <- getNcFiles(dirTrees$C,1)
	close.ncdf(cornNc)
	
	xs <- cornNc$dim$lon$vals 
	ys <- cornNc$dim$lat$vals
	
	####
	#Test if everything is lined up
	####
	
	testDim <- vapply(dat[-1], function(x) dim(x[[1]])[c(1,2)],c(1,0))
	testDim <- cbind(testDim, t(t(dim(dat$C))))
	
	colnames(testDim)
	if(length(unique(testDim[1,])) > 1 ){
		stop(paste("longitude grid cells(xs) is messed up:",
							 paste(testDim[1,], collapse = '|')))
	}
	if(length(unique(testDim[2,])) > 1 ){
		stop(paste("latitude grid cells(ys) is messed up:",
							 paste(testDim[2,], collapse = '|')))
	}
	
	
	#grow corn
	
	
	#Early may planting season for the corn belt while in texas it goes earlier linerally moving south to mid January
	plantCorn <- getTimingSplit(plantTimes,ys)
	
	
	#harvest in mid july in south texas to early october for the corn belt
	harvestCorn <- getTimingSplit(harvestTimes,ys)
	
	dat$C[(is.na(dat$C)|dat$C<0)] <- 0
	# accumulating the growth degree days
	cornGDD <- array(0, dim = c(dim(dat$C),365))
	allDim <- dim(cornGDD)
	
	TailWind <- array(0, dim = allDim)
	fawGrw <- array(0, dim = allDim)
	
	for (xi in seq(1, allDim[1])){
		for(yi in seq(1, allDim[2])){
			#do corn production only when and where there is corn in the ground
			if (!is.na(dat$C[xi, yi]) && dat$C[xi, yi] > CornThres){
				dis <- seq(plantCorn[yi],harvestCorn[yi])
				
				cornGDD[xi, yi, dis] <- 1.8 * calcDegDay(dat$Air$H[xi, yi, dis],
																								 dat$Air$L[xi, yi, dis], 
																								 base = TGDDbaseCrop,
																								 highLim = 30,
																								 lowLim = 10)		 
			
			}
			
			#do other variables that occur everywhere
			dis <- 1:allDim[3]
			fawGrw[xi, yi, dis] <- calcDegDay(dat$Air$H[xi, yi, dis],
																					 dat$Air$L[xi, yi, dis], 
																					 base = TGDDbaseFAW,
																					 shGrow = FALSE)
			
		}
	}
	
	windStopTO <- (dat$Wind$H > windTOThres)[,,1:365]
	tempStopTO <- (dat$Air$H < tempTOThres)[,,1:365]
	precStopTO <- (dat$Prec$H > precTOThres)[,,1:365]
	cornGDD[is.na(cornGDD)] <- 0
	
	dims <-list(cornNc$dim$lon,
							cornNc$dim$lat,
							dim.def.ncdf("Time", "days", 1:365, unlim=TRUE ))
	
	fVars<- list(var.def.ncdf('CornGDD', '0.1*F*days', dims, 999, prec="integer"),
							 var.def.ncdf('FawGDD', '0.1*C*days', dims, 999, prec="integer"),
							 var.def.ncdf('Corn', 'Hectares', list(cornNc$dim$lon, cornNc$dim$lat), 999, prec="integer"),
							 var.def.ncdf('windStopTO', 'Boolean', dims,2, prec="short"),
							 var.def.ncdf('tempStopTO', 'Boolean', dims,2, prec="short"),
							 var.def.ncdf('precStopTO', 'Boolean', dims,2, prec="short")
							 )
	
	anc <- create.ncdf(pathOut,fVars)
	
	put.var.ncdf(anc, "CornGDD", cornGDD*10)
	put.var.ncdf(anc, "FawGDD", fawGrw*10)
	put.var.ncdf(anc, "Corn", dat$C)
	put.var.ncdf(anc, 'precStopTO', precStopTO)
	put.var.ncdf(anc, 'tempStopTO', tempStopTO)
	put.var.ncdf(anc, 'windStopTO', windStopTO)
	att.put.ncdf(anc, 0 , "PlantingTimes", plantTimes)
	att.put.ncdf(anc, 0 , "HarvestTimes", harvestTimes)
	att.put.ncdf(anc, 0 , "CornThres", CornThres)
	att.put.ncdf(anc, 0 , "TGDDbaseCrop", TGDDbaseCrop)
	att.put.ncdf(anc, 0 , "TGDDbaseFAW", TGDDbaseFAW)
	att.put.ncdf(anc, 0 , "windTOThres", windTOThres)
	att.put.ncdf(anc, 0 , "precTOThres", precTOThres)
	att.put.ncdf(anc, 0 , "tempTOThres", tempTOThres)
	
	
	junk <- close.ncdf(anc)
	
}


getNcFiles <- function(tree, cropFlag){
	direc <- rev(tree)[1]
	if (cropFlag){
		ncFile <- list.files(direc, '.nc', full.names = TRUE)
		ncObs <- ncdf::open.ncdf(ncFile, write=TRUE)
		
	} else {
		temp <- paste(direc, "Combined", sep="/")
		ncNames <- c(paste(temp, "H.nc", sep=""), paste(temp, "L.nc", sep=""))
		ncObs <- list()
		tph <- ncdf::open.ncdf(ncNames[1], write=TRUE)
		tpl <- ncdf::open.ncdf(ncNames[2], write=TRUE)
		ncObs <- list("H" = tph, "L" = tpl)
	}
	
	return(ncObs)
}

# Take a vector of the highs and low values 
# and do a average with some condition
calcDegDay <- function(highs,
											 lows,
											 base = 10,
											 shGrow = TRUE,
											 highLim = FALSE,
											 lowLim = FALSE){
	
	if(as.logical(highLim)){
		highs[highs > highLim] <- highLim
	}
	if(as.logical(lowLim)){
		lows[lows > lowLim] <- lowLim
	}
	add <- (((highs + lows)/2) - base)
	add[add < 0] <- 0
	if (shGrow){
		return(cumsum(add))
	} else {
		return(add)
	}
}

getVar <- function(objs){
	vh <- ncdf::get.var.ncdf(objs[[1]])
	vl <- ncdf::get.var.ncdf(objs[[2]])
	return(list("H" = vh, "L" = vl))
}

getTimingSplit <- function(times, ys){
	slope <- (times[4] - times[3]) / (times[2] - times[1])
	
	#Do three sections in the vector of ys
	effect <- ifelse(ys < times[1], times[3],
		ifelse(ys < times[2],  ceiling(slope*(ys - times[1]) + times[3]),
		times[4]))
	
	return(effect)
}

