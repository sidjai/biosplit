#' Download the Cropscape files
#' 
#' Uses the Cropscape api to get download the crop .tiffs of the selected crop type
#' 
#' @param year The year to grab
#' @param cropExtent An extent of the downloaded area
#' @param outPath The path where the .tifs are saved
#' @param cropType The crop type you want to export
#' @example 
#' NASS2TIFs(cfg$CropFold[1],cfg$cropExtent)
#' 
#' @return Horizontal slices of the area of interest in the path you specify. In R, nothing.
#' @export
NASS2TIFs <- function(outPath, year, cropExtent, cropType=1){
	cropProj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
	if (match(cropExtent@projection,cropProj)!=1){
		#project points into cropscape projection
	}
	
	name <- switch(cropType,1="Corn")
	
	startCDL <- 'http://nassgeodata.gmu.edu:8080/axis2/services/CDLService/GetCDLFile?year='
	startCDL <- paste0(startCDL, year, '&bbox=', cropExtent@xmin)
	
	startExt <- 'http://nassgeodata.gmu.edu:8080/axis2/services/CDLService/ExtractCDLByValues?file='
	
	ys <- seq(cropExtent@ymin,(cropExtent@ymax), by=cropExtent@spc)
	lasty <- length(ys)
	bots <- rev(rev(ys)[-1])
	
	tops <- ys[-1]
	
	#(ymin+(3*spc)
	for (ind in seq(1,lasty)){
		cat(ind/lasty,"\n")
		outName <- paste0(cfg$CropFold[1], "/", year, '_', name, '_', zstr(ind), '.tif')
		if (!file.exists(outName)){
			textCDL <- paste(startCDL, bots[ind], cropExtent@xmax, tops[ind], sep=",")
			cdlURL <- parseURL(httr::GET(textCDL))
			
			textExt <- paste0(startExt, cdlURL, '&values=1')
			tifURL <- parseURL(httr::GET(textExt))
			download.file(tifURL, destfile=outName, mode='wb')
		}
	}
}

parseURL <- function(x){
	raw <- content(x,as = 'text')
	parse <- strsplit(raw,'returnURL>')
	text <- gsub('</','',parse[[1]][2])
	return(text)
}

zstr <- function(num,dig=2){
	str <- toString(num)
	while (nchar(str)<dig){
		str <- paste("0",str,sep="")
	}
	return(str)
}

