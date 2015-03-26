#! C:/Program Files/R/R-3.1.1/bin/x64/Rscript.exe
realWd <- gsub("/r_code","",ifelse(grepl("ystem",getwd()),dirname(sys.frame(1)$ofile),getwd()))
load(paste(realWd,"cfg.Rout",sep="/"))

tic <- Sys.time()

require(rgdal)
require(raster)
require(ncdf)

args <- paste(commandArgs(),collapse=",")

cropProj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
translate <- as.matrix(read.table(paste(cfg$MetARLDir,paste0(cfg$metDataType,"Grid2.txt"),sep="/")))

goodProj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

boBox <- extent(cfg$xmin, cfg$xmax, cfg$ymin, cfg$ymax)
cropGrid <- raster(boBox,
	crs = cropProj,resolution = cfg$spc)
finGrid <- projectExtent(cropGrid,goodProj)
finExt <- extent(finGrid)

processMet <- function(f,tFlag){
	ras <- raster(f)
	
	if(tFlag){
		ras <- calc(ras,function(x)x-273.15)
	}

	points <- rasterToPoints(ras)
	xyz <- cbind(translate[,c(1,2)],points[,3])
	inboBox <- ((xyz[,1] > finExt@xmin & xyz[,1]< finExt@xmax) &
		(xyz[,2] > finExt@ymin & xyz[,2]< finExt@ymax))
	xyzwExt <- xyz[inboBox,]
	
	ras <- rasterize(xyzwExt[,c(1,2)],finGrid,field=xyzwExt[,3],fun=mean)
	return(ras)
}


metTree <- list(cfg$AirTempFold,cfg$WindFold,cfg$SoilTempFold)
tempFlag <- c(TRUE,FALSE,TRUE)

#All Met data in the ARL files that we want
for (ty in seq(1,length(metTree))){
	cat(metTree[[ty]][1],"\n")
	rawFiles <- list.files(metTree[[ty]][1])
	ncFiles <- gsub('grd','nc',rawFiles)
	confirm <- vapply(rawFiles,function(name){
		ras <- processMet(paste(metTree[[ty]][1],name,sep="/"),tempFlag[[ty]])
		sult <- try(writeRaster(ras,
			filename=paste(metTree[[ty]][2], gsub('grd','nc',name), sep="/"),
			format="CDF", overwrite=TRUE),silent=TRUE)
		return(isLonLat(sult))
		},1)

	if(!min(confirm)) stop("grd conversion failed: loadMetData.R line 64")

	# combine the slices
	type <- c("L","H")
	for (pe in 1:2){
		inFiles <- paste(metTree[[ty]][2],ncFiles[seq(pe,length(ncFiles),2)],sep="/")
		spliceStack <- stack(inFiles,quick=TRUE)
		nameOut <- paste(metTree[[ty]][3],paste0("combined", type[[pe]], ".nc"),sep="/")
		
		junk <- try(writeRaster(spliceStack,filename=nameOut,
			format="CDF",varname = type[[pe]], overwrite=TRUE),silent=TRUE)
	}
}

toc <- round(as.double(Sys.time() - tic, units = "mins"),2)