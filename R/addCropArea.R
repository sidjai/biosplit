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
	
	addLatLon <- switch(class(shapeAdd),
											character = if(!grepl("kml", shapeAdd)){
												"not a supported extension"
											} else {
												if(!file.exists(shapeAdd)){
													"not in existence"
												} else {
													layer <- gsub("[.]kml", '', basename(shapeAdd))
													raster::rasterToPoints(
														raster::raster(
															rgdal::readOGR(shapeAdd, layer, verbose=FALSE),
															resolution = raster::res(orgDat)
														)
													)
												}
											},
											matrix = shapeAdd,
											"not a defined class")
	
	if(length(addLatLon) == 0 ) stop("Input grid is empty")
	
	if(is.character(addLatLon)) stop(paste("Input was", addLatLon))
	
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
