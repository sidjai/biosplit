#! C:/Program Files/R/R-3.1.1/bin/x64/Rscript.exe
rm(list=ls(all=TRUE))
realWd <- gsub("/r_code","",ifelse(grepl("ystem",getwd()),dirname(sys.frame(1)$ofile),getwd()))
load(paste(realWd,"cfg.Rout",sep="/"))

require(rgdal)
require(raster)
require(ncdf)

args <- paste(commandArgs(),collapse=",")


cropProj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

goodProj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

boBox <- extent(cfg$xmin, cfg$xmax, cfg$ymin, cfg$ymax)
cropGrid <- raster(boBox,
	crs = cropProj,resolution = cfg$spc)
finGrid <- projectExtent(cropGrid,goodProj)
finExt <- extent(finGrid)



lastx <- ncol(cropGrid)
xs <- xFromCol(cropGrid)
	xLft <- xs - xres(cropGrid)/2
	
	xRgt <- xs + xres(cropGrid)/2

processCrop <- function(f){
	ras <- raster(readGDAL(f,silent = TRUE))
	
	top <- ras@extent@ymax #get the top of the slice
	bot <- ras@extent@ymin #get the bottom of the slice
	cat(top/cfg$ymax,"\n")
	
	zvals <- vapply(1:lastx,function(xi){
		ext <- extent(xLft[xi], xRgt[xi], bot, top)
		sum(extract(ras, ext),na.rm=TRUE)
	},1)
	ras <- 999
	return(zvals)
}


rawFiles <- list.files(cfg$CropFold[1],pattern = "tif")
#make it go top to bottom
rawFiles <- rev(rawFiles)
zs <- vapply(rawFiles,function(name){
	processCrop(paste(cfg$CropFold[1],name,sep="/"))
},rep(1,ncol(cropGrid)),USE.NAMES = FALSE)

zs <- as.vector(zs)
translate <- rasterToPoints(cropGrid)

extDif <- dim(translate)[1] - length(zs)
if (extDif != 0){
	stop("Incomplete data download, Grid has more points than the aggregate result")
}

xyz <- cbind(translate,zs)

outName <- paste(cfg$CropFold[2],paste0("aggCrop_", cfg$year, ".dat"),sep="/")
write.table(xyz,
	file = outName,
	quote=FALSE,row.names=FALSE,col.names=FALSE)

ras <- rasterFromXYZ(xyz)
projection(ras) <- cropProj
ras <- projectRaster(ras,finGrid)
dataType(ras) <- 'INT4S'
sult <- try(writeRaster(ras,filename=gsub("dat","nc",outName),
	format="CDF", overwrite=TRUE),silent=TRUE)