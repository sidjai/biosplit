#!C:/Program Files/R/R-3.1.1/bin/x64/Rscript.exe
rm(list=ls(all=TRUE))
ptm <- proc.time()
realWd <- gsub("/r_code","",ifelse(grepl("ystem",getwd()),dirname(sys.frame(1)$ofile),getwd()))
load(paste(realWd,"cfg.Rout",sep="/"))

require(rgdal)
require(raster)
require(ncdf)


getNCFiles <- function(dirTree,cropFlag){
	direc <- rev(dirTree)[1]
	if (cropFlag){
		ncFile <- list.files(direc,'.nc',full.names = TRUE)
		ncObs <-open.ncdf(ncFile,write=TRUE)
		
	} else {
		temp <- paste(direc,"Combined", sep="/")
		ncNames <- c(paste(temp,"H.nc", sep=""),paste(temp,"L.nc", sep=""))
		ncObs <- list()
		tph <-open.ncdf(ncNames[1],write=TRUE)
		tpl <-open.ncdf(ncNames[2],write=TRUE)
		ncObs <- list("H"=tph,"L"=tpl)
	}
	
	return(ncObs)
}

getVar <- function(objs){
	vh<-get.var.ncdf(objs[[1]])
	vl<-get.var.ncdf(objs[[2]])
	return(list("H"=vh,"L"=vl))
}

getTimingSplit <- function(times,ys){
	slope <- (times[4]-times[3])/(times[2]-times[1])
	
	#Do three sections in the vector of ys
	effect <- ifelse(ys < times[1], times[3],
		ifelse(ys < times[2],  ceiling(slope*(ys-times[1])+times[3]),
		times[4]))
	
	return(effect)
}

cornNcs <- getNCFiles(cfg$CropFold,1)
datC <- get.var.ncdf(cornNcs)



airNcs <- getNCFiles(cfg$AirTempFold,0)
datAir<-getVar(airNcs)
soilNcs <- getNCFiles(cfg$SoilTempFold,0)
datSoil<-getVar(soilNcs)
windNcs <- getNCFiles(cfg$WindFold,0)
datWind<-getVar(windNcs)


####
#Test if everything is set



if(dim(datC)[1]!=dim(datAir$H)[1] ||
	dim(datC)[1]!=dim(datSoil$H)[1] ||
	dim(datC)[1]!=dim(datWind$H)[1]) stop("xs messed up, Abandon ship")

if(dim(datC)[2]!= dim(datAir$H)[2] ||
	dim(datC)[2]!= dim(datSoil$H)[2] ||
	dim(datC)[2]!= dim(datWind$H)[2]) stop("xs messed up, Abandon ship")

#grow corn


xs <- cornNcs[8]$dim$lon$vals #get the longitude that coresponds to the first dimension($lon$id can get the vector it corresponds to)
ys <- cornNcs[8]$dim$lat$vals

#Early may planting season for the corn belt while in texas it goes earlier linerally moving south to mid January
plantCorn <- getTimingSplit(cfg$plantTimes,ys)


#harvest in mid july in south texas to early october for the corn belt
harvestCorn <- getTimingSplit(cfg$harvestTimes,ys)

datC[(is.na(datC)|datC<0)] <- 0
# accumulating the growth degree days
cornGDD <- array(0, dim=c(dim(datC),365))

for (xi in seq(1,dim(cornGDD)[1])){
	for(yi in seq(1,dim(cornGDD)[2])){
		if (!is.na(datC[xi,yi]) && datC[xi,yi] > cfg$CornThres){
			for (di in seq(plantCorn[yi],harvestCorn[yi])){
				top <- ifelse(datAir$H[xi,yi,di]>30,30,datAir$H[xi,yi,di])
				#top <- datAir$H[xi,yi,di]
				bot <- ifelse(datAir$L[xi,yi,di]<10,10,datAir$L[xi,yi,di])
				
				add <- 1.8*(sort(c(((top+bot)/2)-cfg$TGDDbaseCrop,0))[2])
				cornGDD[xi,yi,di] <- cornGDD[xi,yi,di-1]+add
			}
		}
	}
}
cornGDD[is.na(cornGDD)] <- 0


#transfer to other livability and flight propensity

#Life <- array(0, dim=c(dim(datC),365))
#Flight <- array(1, dim=c(dim(datC),365))
TailWind <-array(0, dim=c(dim(datC),365))
fawGrw <- array(0, dim=c(dim(datC),365))

for (xi in seq(1,dim(TailWind)[1])){
	for(yi in seq(1,dim(TailWind)[2])){
		#do the vars that occur throughout the area
		for(di in seq(1,dim(TailWind)[3])){
			fawGrw[xi,yi,di] <- sort(c((datAir$H[xi,yi,di]+datAir$L[xi,yi,di])/2-cfg$TGDDbaseFAW,0))[2]
			TailWind[xi,yi,di] <- ifelse(datWind$H[xi,yi,di]>0,abs(datWind$H[xi,yi,di]),0)
		}
		
		#do the vars that only change from default in corn areas
# 		if (datC[xi,yi]>CornThres & !is.na(datC[xi,yi])){
# 			for(di in seq(1,dim(Life)[3])){
# 				cGrw <- cornGDD[xi,yi,di]
# 				Flight[xs,yi,di] <- ifelse(cGrw>1400, .9,.1)
# 				#Life[xi,yi,di] <-1-pweibull(2*cGrw/2500, 1.5 , scale = 1)
# 
# 				Life[xi,yi,di] <-ifelse(cGrw==0,0,
# 							ifelse(cGrw<1400,.0005*cGrw+.3,
# 							ifelse(cGrw<2000,1, 
# 							ifelse(cGrw<2400,4.2-.00175*cGrw,0))))
# 
# 			}
# 		}
	}
}




dims <-list(cornNcs[8]$dim$lon,
	cornNcs[8]$dim$lat,
	dim.def.ncdf( "Time", "days", 1:365, unlim=TRUE ))

fVars<- list(var.def.ncdf('CornGDD', '0.1*C*days',dims,999,prec="integer"),
	var.def.ncdf('FawGDD', '0.1*C*days',dims,999,prec="integer"),
	var.def.ncdf('Corn', 'Hectares',list(cornNcs[8]$dim$lon, cornNcs[8]$dim$lat),999,prec="integer"),
	var.def.ncdf('TailWind', 'm/s (north)',dims,999))

#list(cnc[8]$dim$lon,cnc[8]$dim$lat,dimT),1.e30),
#Gvardef <- var.def.ncdf('CornGDD', 'C*days',
	#list(cnc[8]$dim$lon,cnc[8]$dim$lat,dimT),1.e30)
#var.def.ncdf('Livability', 'ratio',dims,1.e30),
#var.def.ncdf('FlightProp', 'ratio',dims,1.e30),

anc <-create.ncdf(cfg$AprioriLoc,fVars)

put.var.ncdf(anc,"CornGDD",cornGDD*10)
put.var.ncdf(anc,"FawGDD",fawGrw*10)
put.var.ncdf(anc,"Corn",datC)
#put.var.ncdf(anc,"Livability",Life)
#put.var.ncdf(anc,"FlightProp",Flight)
put.var.ncdf(anc,"TailWind",TailWind)
att.put.ncdf(anc,0,"PlantingTimes",cfg$plantTimes)
att.put.ncdf(anc,0,"HarvestTimes",cfg$harvestTimes)
att.put.ncdf(anc,0,"CornThres",cfg$CornThres)
att.put.ncdf(anc,0,"TGDDbaseCrop",cfg$TGDDbaseCrop)
att.put.ncdf(anc,0,"TGDDbaseFAW",cfg$TGDDbaseFAW)

#put.var.ncdf(anc,"Lows",datL)
junk <- close.ncdf(anc)
#junk <- close.ncdf(hnc)
proc.time() - ptm
