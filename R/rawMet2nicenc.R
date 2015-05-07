#'Process the daily grid files into a nice netCDF file
#'
#'@param dirTreeIn A character array of the three directories to use
#'	1) where the input files are located
#'	2) where the projected .nc slices are going to be outputted
#'	3) where the final combined output .nc file is outputted
#'
#'@param projKey A string with the projection info either in the form of
#'	a file location of a mapping table or proj4string for the input grids
#'
#'@param niceGrid A grid of the final extent and projection that is needed
#'@param unit A string designating the unit of the variable...use deg for degre
#'@param transCol A vector with column numbers for the mapping table, (lat,long),
#'	Only used if projKey is a mapping table file location
#'@import ncdf
#'@export
rawMet2nicenc <- function(dirTreeIn,
													projKey,
													niceGrid,
													unit = 'degC',
													transCol = c(1,2)){
	
	#if dirTreeIn is a list of 3 out the second and final in last folder
	metFiles <- parseFiles(dirTreeIn[1], 'grd')
	slicencFiles <- gsub('grd', 'nc', gsub(dirTreeIn[1], dirTreeIn[2], metFiles))
	
	KtoC <- grepl("deg",unit)
	
	if (file.exists(projKey)){
		#its a table with lat lon mapping in the first 2 columns
		projDict <- as.matrix(read.table(projKey))[,transCol]
	} else {
		projDict <- c(projKey,niceProj)
	}
	
	prog <- txtProgressBar(style = 3)
	confirm <- vapply(1:length(metFiles), function(x){
		setTxtProgressBar(prog,x/length(metFiles))
		ras <- processMet(metFiles[x], projDict, convertKtoC = KtoC)
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
		inFiles <- slicencFiles[seq(pe,length(slicencFiles),2)]
		spliceStack <- raster::stack(inFiles,quick=TRUE)
		pathOut <- paste(dirTreeIn[3], paste0("combined", type[[time]], ".nc"),sep="/")
		
		junk <- try(raster::writeRaster(spliceStack, filename = pathOut,
														format="CDF", varname = type[[time]], varunit = unit, overwrite=TRUE)
								,silent=TRUE)
	}
}

processMet <- function(pathMet, transDict, convertKtoC = FALSE,niceGrid){
	ras <- raster(pathMet)
	
	if(convertKtoC){
		ras <- calc(ras,function(x)x-273.15)
	}
	
	xyz <- projMet(ras,transDict)
	
	#manually extract 
	inboBox <- ((xyz[,1] > finExt@xmin & xyz[,1]< finExt@xmax) &
								(xyz[,2] > finExt@ymin & xyz[,2]< finExt@ymax))
	xyzwExt <- xyz[inboBox,]
	
	ras <- raster::rasterize(xyzwExt[,c(1,2)], finGrid, field = xyzwExt[,3], fun = mean)
	return(ras)
}

projMet <- function(rs, dict){
	
	if(is.character(dict)){
		raster::projection(rs) <- dict[1]
		rs <- raster::projectRaster(rs,dict[2])
		datum <- raster::rasterToPoints(rs)
	} else {
		points <- raster::rasterToPoints(rs)
		datum <- cbind(key, points[,3])
	}
	
	return(datum)
}
}

