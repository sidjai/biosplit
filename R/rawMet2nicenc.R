#'Process the daily grid files into a nice netCDF file
#'
#'Goes through the twice daily grid files for one variable and projects, crops and combines them
#'	into two managable nc files for the highs and the lows.
#'	The grids are produced by ARL2GRD.py and the result is used by aprioriVars.R.
#'@param dirTreeIn A character array of the three directories to use
#'	1) where the input files are located
#'	2) where the projected .nc slices are going to be outputted
#'	3) where the final combined output .nc file is outputted
#'
#'@param projKey A string with the projection info either in the form of
#'	a file location of a mapping table or proj4string for the input grids
#'
#'@param niceGrid A grid of the final extent and projection that is needed
#'@param unit A string designating the unit of the variable...use deg for degree
#'@param transCol A vector with column numbers for the mapping table, (lat,long),
#'	Only used if projKey is a mapping table file location
#'@return Saves the projected and cropped slices in the second entry in DirTreeIn,
#'	and two nc files with the stacked nc files for the Highs and the lows for the whole year
#'	
#'@import ncdf
#'@export
rawMet2nicenc <- function(dirTreeIn,
													projKey,
													niceGrid,
													unit = 'unitless',
													transCol = c(1,2)){
	
	#if dirTreeIn is a list of 3 out the second and final in last folder
	metFiles <- parseFiles(dirTreeIn[1], 'grd')
	reqAmt <- 2 * (365 + (file2year(metFiles[1]) %% 4 == 0))
	#check for the number of files
	if(length(metFiles) != reqAmt){
		stop(sprintf("Folder: %s only has %d grid files in it, needs %d",
								 dirTreeIn[1], length(metFiles),  reqAmt))
	}
	
	slicencFiles <- gsub('grd', 'nc', gsub(dirTreeIn[1], dirTreeIn[2], metFiles))
	
	
	KtoC <- grepl("K",unit)
	unit <- gsub("K", "degC", unit)
	
	if (file.exists(projKey)){
		#its a table with lat lon mapping in the first 2 columns
		projDict <- as.matrix(read.table(projKey))[,transCol]
	} else {
		projDict <- c(projKey,raster::projection(niceGrid))
	}
	
	prog <- txtProgressBar(style = 3)
	confirm <- vapply(1:length(metFiles), function(x){
		setTxtProgressBar(prog,x/length(metFiles))
		ras <- processMet(metFiles[x], projDict, niceGrid, convertKtoC = KtoC)
		sult <- try(raster::writeRaster(ras,
														filename = slicencFiles[x],
														format = "CDF", overwrite=TRUE),
								silent=TRUE)
		
		return(raster::isLonLat(sult))
		},1)
	
	
	badInd <- which(!confirm)
	if(length(badInd) != 0 ){
		stop(paste("These files did not project right:\n", 
							 paste(metFiles[badInd],collapse= '\n')))
	}
	
	type <- c("L", "H")
	for (time in 1:2){
		inFiles <- slicencFiles[seq(time,length(slicencFiles),2)]
		spliceStack <- raster::stack(inFiles,quick=TRUE)
		pathOut <- paste(dirTreeIn[3], paste0("Combined", type[[time]], ".nc"),sep="/")
		
		junk <- try(raster::writeRaster(spliceStack, filename = pathOut,
														format="CDF", varname = type[[time]], varunit = unit, overwrite=TRUE)
								,silent=TRUE)
	}
}

processMet <- function(pathMet, transDict, niceGrid, convertKtoC = FALSE){
	ras <- raster(pathMet)
	
	if(convertKtoC){
		ras <- calc(ras,function(x)x-273.15)
	}
	
	xyz <- projMet(ras,transDict)
	
	cut <- niceGrid@extent
	
	#manually extract 
	inboBox <- ((xyz[,1] > cut@xmin & xyz[,1]< cut@xmax) &
								(xyz[,2] > cut@ymin & xyz[,2]< cut@ymax))
	xyzwCrop <- xyz[inboBox,]
	
	ras <- raster::rasterize(xyzwCrop[,c(1,2)], niceGrid, field = xyzwCrop[,3], fun = mean)
	
	badCells <- is.na(values(ras))
	if(length(badCells) / raster::ncell(ras) > 0.2){
		wght <- focalWeight(ras, .8, "rectangle")
		wght <- wght * length(wght)
		rgas <- focal(ras, wght, fun = mean, na.rm = TRUE, pad = TRUE)
		values(ras)[badCells] <- values(rgas)[badCells]
	}
	return(ras)
}

projMet <- function(rs, dict){
	
	if(is.character(dict)){
		raster::projection(rs) <- dict[1]
		rs <- raster::projectRaster(rs, dict[2])
		datum <- raster::rasterToPoints(rs)
	} else {
		points <- raster::rasterToPoints(rs)
		datum <- cbind(dict, points[,3])
	}
	
	return(datum)
}

