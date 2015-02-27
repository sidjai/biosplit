
rm(list=ls(all=TRUE))
#direcC<-"C:\\Users\\Siddarta.Jairam\\Downloads\\Crop r"
direcs<-list()
direcs[[1]]<-"C:/Users/Siddarta.Jairam/Documents/Crop data"
direcs[[2]]<-"C:/Users/Siddarta.Jairam/Documents/Hysplit temp data"
direcs[[3]]<-"C:/Users/Siddarta.Jairam/Documents/Hysplit wind data"
direcs[[4]]<-"C:/Users/Siddarta.Jairam/Documents/Hysplit soilT data"

resName <- "Crop_Grid"

year <- 2011

cornFlag <- 1
tempFlag <- list(1,1,1)


#direcC <- paste(direcC,year, sep="/")
direcs <- lapply(direcs,function(x)paste(x,year, sep="/"))

orgFile <- paste(direcs[[1]],paste(year, "Corn", "1.tif", sep="_"), sep="/")


resFile <- paste(direcs[[1]],"Projected", paste(resName,year, sep="_"), sep="/")
cdatFile <- paste(resFile,".dat", sep="")

tdatFold <- lapply(direcs,function(x)paste(x,"ParseAndExtract", sep="/"))
tdatFold <- tdatFold[2:length(tdatFold)]
tresFold <- lapply(direcs,function(x)paste(x,"Projected/", sep="/"))
tresFold <- tresFold[2:length(tresFold)]

combFile <- lapply(tresFold,function(x)paste(x,"Combined", sep=""))

addList <- list("H.nc","L.nc","C.nc")

#resFile <- paste(direc,year,paste(resName,year, sep="_"), sep="/")
#resultFilenc <- paste(direc,resNamenc, sep="/")

require(rgdal)
require(raster)
require(sp)
require(ncdf)



#original coordinate system
#untouch <- readGDAL(orgFile)
#projCropScape <- proj4string(untouch)

projCropScape <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

convert<-function(f, mod=(1),off=0,proj,flagDat=0, flagkml=0,flagncdf=1,flagtif=0){
	

	temp <- strsplit(f,split='[.]dat')[[1]]
	
	
	temp <- strsplit(temp,split='[/]')[[1]]
	for (i in seq(1,length(temp))){
		if (nchar(temp[i])==4){
			base<-paste(paste(temp[1:i], collapse = '/'),'Projected/', sep='/')
		}
	}

	
	#base <- paste(paste(temp[1:(length(temp)-1)], collapse = '/'),'Projected/', sep='/')	
	name <- temp[length(temp)]
	
	
	
	if (mod!=1){
		rawd <- as.matrix(read.csv(f))
		rawd[,2]<-rawd[,2]-(rawd[,2]%%mod)+55

	}else rawd <- as.matrix(read.table(f))
	
	rawd[,3] <- rawd[,3]-off

	rast<-rasterFromXYZ(rawd, crs=proj)
	prast <- projectRaster(rast, crs='+proj=longlat +datum=WGS84', method='ngb')
	
	#dat <- rasterToPoints(prast)


	
	if (flagkml){
		KML(prast, file=paste(base,name,'.kmz', sep=''),zip='C:/Program Files/7-Zip/7z.exe',overwrite=TRUE)
	}

	if (flagncdf){
		writeRaster(prast, filename=paste(base,name,'.nc', sep=''), format='CDF',overwrite=TRUE)

	}

	if (flagtif){
		writeRaster(prast, filename=paste(base,name,'.tif', sep=''), format='GTiff',overwrite=TRUE)
	}
	
	if (flagDat==1){
		write.table(dat, file=paste(base,name,'.dat', sep=''),row.names=FALSE,col.names=FALSE)
		#writeRaster(prast,paste(base,name,'.rst', sep=''))
	} else if (flagDat==2){
		return(raw)
	}

}

##############################################################################


if (cornFlag){
	#outBase<- 
	convert(cdatFile,mod=100, proj=projCropScape)
} 
for (vInd in seq(1,length(tdatFold))){
	tdats<-list.files(tdatFold[[vInd]],pattern='[.]dat')
	cat("Variable #",vInd,"\n")
	if (tempFlag[[vInd]]){
		for (ind in seq(1,length(tdats))){
			fullFile <-paste(tdatFold[[vInd]],tdats[ind],sep='/')
			
			convert(fullFile,
				mod=1,
				off=ifelse(vInd==2,0,273.15),
				proj=projCropScape)
		}

	}
	
	#combine the nc files
	tncs<-list.files(tresFold[[vInd]],pattern="[.]nc",full.names = TRUE)
	tncs <- tncs[!grepl("Comb",tncs)]
	thncs <- tncs[seq(2,length(tncs),2)]
	tcncs <- tncs[seq(1,length(tncs),2)]

	ncs <- lapply(addList,function(x)paste(combFile[[vInd]],x,sep=""))

	hots<- stack(thncs,quick=TRUE)
	colds<- stack(tcncs,quick=TRUE)

	try(writeRaster(hots, filename=ncs[[1]], format="CDF",varname = "highs", overwrite=TRUE),silent=TRUE)
	try(writeRaster(colds, filename=ncs[[2]], format="CDF",varname = "lows", overwrite=TRUE),silent=TRUE)


	
}





#raw[2]<-raw[2]-(raw[2]%%100)


#final
#rast<-rasterFromXYZ(raw, crs=proj4string(untouch))
#prast <- projectRaster(rast, crs="+proj=longlat +datum=WGS84", method='ngb')

#KML(prast, file=paste(resFile,".kml", sep=""))

#writeRaster(prast, filename=paste(resFile,".nc", sep=""), format="CDF",overwrite=TRUE)
#writeRaster(prast, filename=paste(resFile,".tif", sep=""), format="GTiff",overwrite=TRUE)