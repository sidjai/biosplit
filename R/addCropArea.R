#' addCropArea
#'
#' This function is for adding an additional crop area that was not included in 
#' the NASS system. The most obvious use case is for areas just outside of U.S, 
#' like in the Ontario province in Canada. Another is for transcontinental
#' migration adding large splotches of Mexico, Carribean and Canada. The 
#' function works by looking at a shape file for a grid of points (lat, lon) and
#' using the cropAmt to create an additional area to add to the original file.
#' 
#' @param pathIn The path of the parsed and projected crop nc file
#' @param shapeAdd Either A shape file from Google Earth (kml) defining the area,
#' or an x,y matrix defining the added points.
#' @param cropAmt The amount of crop in that area which is either constant or 
#' the length of the input shape file
#' @param pathOut The path out of the final nc file
#'
#' @return The final raster and the new nc file if pathOut is specified.
#' @export
#'
addCropArea <- function(pathIn, shapeAdd, cropAmt, pathOut = ''){
	
	orgDat <- if(is.character(pathIn)){
		 raster::raster(pathIn)
	} else {
		pathIn
	}
	
	addLatLon <- readKML(shapeAdd, raster::res(orgDat))
	
	desLen <- length(addLatLon[,1])
	inLen <- length(cropAmt)
	
	if(inLen > 1){
		if(desLen != inLen){
			stop(sprintf("Need to supply %f number of points or a single point, you supplied %f points",
									 desLen,
									 inLen))
		}
	} else {
		cropAmt <- rep(cropAmt, desLen)
	}
	
	
	addDat <- raster::rasterize(addLatLon, orgDat,
															field = cropAmt,
															fun = sum,
															background = 0)
	
	newDat <- addDat + orgDat
	
	if (nchar(pathOut) > 0){
		
		try(raster::writeRaster(newDat,
														filename =pathOut,
														format = "CDF", overwrite=TRUE),
				silent=TRUE)
	}
	
	return(invisible(newDat))
}

#' simAreaStats: Interrogate the final simulation on specific areas
#' 
#' This post processing process allows the user to analyze regions instead of
#' the entire US. A number of statistics are outputted as elements in a list.
#'
#' @param pathShape The path to a kml polygon to define the extent
#' @inheritParams ncdf2trapdata
#' 
#' @return a list of the first occurance for both the populations and the
#' mixing area for the entire area and the entire year
#' @export
simAreaStats <- function(dirSim, pathShape){
	
	simTX <- raster::stack(paste0(dirSim, "/Final.nc"), varname = "TXMoth")
	simFL <- raster::stack(paste0(dirSim, "/Final.nc"), varname = "FLMoth")
	
	
	areaMask <- readKML(pathShape, res(simTX))
	
	resMask <- raster::rasterize(areaMask, simTX,
		field = 1,
		fun = sum,
		background = NA)
	
	
	sliceTX <- as.array(mask(simTX, resMask))
	sliceFL <- as.array(mask(simFL, resMask))
	
	firstOccFL <- min(calcFirstOcc(sliceFL), na.rm = TRUE)
	firstOccTX <- min(calcFirstOcc(sliceTX), na.rm = TRUE)
	totMix <- round(mean(calcMixingRatio(sliceFL, sliceTX), na.rm = TRUE), 2)
	
	return(list(firstOccFL = firstOccFL, firstOccTX = firstOccTX, totMix = totMix))
}

readKML <- function(shapeIn, outRes){
	latLon <- switch(class(shapeIn),
		character = if(!grepl("kml", shapeIn)){
			"not a supported extension"
		} else {
			if(!file.exists(shapeIn)){
				"not in existence"
			} else {
				layer <- gsub("[.]kml", '', basename(shapeIn))
				raster::rasterToPoints(
					raster::raster(
						suppressWarnings(rgdal::readOGR(shapeIn, layer, verbose=FALSE)),
						resolution = outRes
					)
				)
			}
		},
		matrix = shapeIn,
		"not a defined class")
	
	if(length(latLon) == 0) stop("Input grid is empty")
	if(is.character(latLon)) stop(paste("Input was", latLon))
	
	return(latLon)
	
}