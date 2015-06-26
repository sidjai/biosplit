#' Download the Cropscape files
#' 
#' Uses the Cropscape api to get download the crop .tiffs of the selected crop type
#' 
#' @param dirOut The directory where the .tifs are saved
#' @param year The year to grab from CropScape
#' @param cropGrid An grid of the downloaded area, need extent and spacing 
#' @param cropType The crop type you want to export
#' 
#' 
#' @return Horizontal slices of the area of interest in the path you specify. In R, nothing.
#' @export
NASS2TIFs <- function(dirOut, year, cropGrid, cropType="Corn"){
	ext <- raster::extent(cropGrid)
	cropProj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
	if (match(as.character(cropGrid@crs), cropProj)!=1){
		#project points into cropscape projection
	}
	
	cropNum <- switch(cropType,Corn=1)
	
	startCDL <- 'http://nassgeodata.gmu.edu:8080/axis2/services/CDLService/GetCDLFile?year='
	startCDL <- paste0(startCDL, year, '&bbox=', ext@xmin)
	
	startExt <- 'http://nassgeodata.gmu.edu:8080/axis2/services/CDLService/ExtractCDLByValues?file='
	
	ys <- seq(ext@ymin, (ext@ymax), by = raster::xres(cropGrid))

	bots <- rev(rev(ys)[-1])
	
	tops <- ys[-1]
	lasty <- length(tops)
	
	prog <- txtProgressBar(style = 3)
	#(ymin+(3*spc)
	for (ind in seq(1, lasty)){
		setTxtProgressBar(prog, ind/lasty)
		outName <- paste0(dirOut, "/", year, '_', cropType, '_', zstr(ind), '.tif')
		if (!file.exists(outName)){
			textCDL <- paste(startCDL, bots[ind], ext@xmax, tops[ind], sep=",")
			cdlURL <- parseURL(httr::GET(textCDL))
			
			textExt <- paste0(startExt, cdlURL, '&values=', cropNum)
			tifURL <- parseURL(httr::GET(textExt))
			download.file(tifURL, destfile=outName, mode='wb')
		}
	}
}

parseURL <- function(x){
	raw <- httr::content(x, as = 'text')
	parse <- strsplit(raw, 'returnURL>')
	text <- gsub('</', '', parse[[1]][2])
	return(text)
}

