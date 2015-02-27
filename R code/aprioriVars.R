
rm(list=ls(all=TRUE))
ptm <- proc.time()

direcC<-"C:/Users/Siddarta.Jairam/Documents/Crop Data"
direcT<-"C:/Users/Siddarta.Jairam/Documents/Hysplit temp data"
direcST<-"C:/Users/Siddarta.Jairam/Documents/Hysplit soilT data"
direcW<-"C:/Users/Siddarta.Jairam/Documents/Hysplit wind data"

resName <- "Crop_Grid"

year <- 2011

require(rgdal)
require(raster)
require(ncdf)

##############################################################################
#Assumptions
plantNums <-rbind(cbind(25,44),cbind(39,136)) #Warning: have to change the program if you want to change
harvestNums <-rbind(cbind(25,195),cbind(39,300))

CornThres <- 75 #Hectares

##############################################################################


outFile <- paste(direcT,year,"aprioriVars.nc",sep="/")

getNCFiles <- function(direc,year,res=""){

	direc <-paste(direc,year,"Projected/", sep="/")
	if (nchar(res)>0){
		files <-paste(direc,paste(res,year, sep="_"),".nc", sep="")
		ncObs <-open.ncdf(files,write=TRUE)
		
	} else {
		temp <- paste(direc,"Combined", sep="")
		files <- c(paste(temp,"H.nc", sep=""),paste(temp,"L.nc", sep=""))
		ncObs <- list()
		tph <-open.ncdf(files[1],write=TRUE)
		tpl <-open.ncdf(files[2],write=TRUE)
		ncObs <- list("H"=tph,"L"=tpl)
	}
	
	return(ncObs)
}

cornNcs <- getNCFiles(direcC,year,resName)
datC <- get.var.ncdf(cornNcs)

getVar<- function(objs){
	vh<-get.var.ncdf(objs[[1]])
	vl<-get.var.ncdf(objs[[2]])
	return(list("H"=vh,"L"=vl))
}

airNcs <- getNCFiles(direcT,year)
datAir<-getVar(airNcs)
soilNcs <- getNCFiles(direcST,year)
datSoil<-getVar(soilNcs)
windNcs <- getNCFiles(direcW,year)
datWind<-getVar(windNcs)

####
#Adjust



if(dim(datC)[1]!=dim(datAir$H)[1] ||
	dim(datC)[1]!=dim(datSoil$H)[1] ||
	dim(datC)[1]!=dim(datWind$H)[1]) cat("xs messed up, Abandon ship")

if(dim(datC)[2]!= dim(datAir$H)[2] ||
	dim(datC)[2]!= dim(datSoil$H)[2] ||
	dim(datC)[2]!= dim(datWind$H)[2]) cat("xs messed up, Abandon ship")

#grow corn


xs <- cornNcs[8]$dim$lon$vals #get the longitude that coresponds to the first dimension($lon$id can get the vector it corresponds to)
ys <- cornNcs[8]$dim$lat$vals

#Early may planting season for the corn belt while in texas it goes earlier linerally moving south to mid January

plantCorn <- ifelse(ys <25, 44,
   ifelse(ys < 39,  ceiling((90/13)*ys-(134)),136))


#harvest in mid july in south texas to early october for the corn belt

harvest <- ifelse(ys <25, 195,
   ifelse(ys < 39,  ceiling((20/3)*ys+(85/3)),300))


# accumulating the growth degree days
cornGDD <- array(0, dim=c(dim(datC),365))

for (xi in seq(1,dim(cornGDD)[1])){
	for(yi in seq(1,dim(cornGDD)[2])){
		if (!is.na(datC[xi,yi]) && datC[xi,yi]>CornThres){
			for (di in seq(plantCorn[yi],harvest[yi])){
				top <-ifelse(datAir$H[xi,yi,di]>30,30,datAir$H[xi,yi,di])
				#top <- datAir$H[xi,yi,di]
				bot <-ifelse(datAir$L[xi,yi,di]<10,10,datAir$L[xi,yi,di])
				
				add <-1.8*(sort(c(((top+bot)/2)-10,0))[2])
				cornGDD[xi,yi,di] <- cornGDD[xi,yi,di-1]+add
			}
		}
	}
}

#transfer to other livability and flight propensity

#Life <- array(0, dim=c(dim(datC),365))
#Flight <- array(1, dim=c(dim(datC),365))
TailWind <-array(0, dim=c(dim(datC),365))
fawGrw <- array(0, dim=c(dim(datC),365))

for (xi in seq(1,dim(TailWind)[1])){
	for(yi in seq(1,dim(TailWind)[2])){
		#do the vars that occur throughout the area
		for(di in seq(1,dim(TailWind)[3])){
			fawGrw[xi,yi,di]<-sort(c((datAir$H[xi,yi,di]+datAir$L[xi,yi,di])/2-13.8,0))[2]
			TailWind[xi,yi,di]<-ifelse(datWind$H[xi,yi,di]>0,abs(datWind$H[xi,yi,di]),0)
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

fVars<- list(var.def.ncdf('CornGDD', 'C*days',dims,1.e30),
	var.def.ncdf('FawGDD', 'C*days',dims,1.e30),
	var.def.ncdf('Corn', 'Hectares',list(cornNcs[8]$dim$lon, cornNcs[8]$dim$lat),1.e30),
	var.def.ncdf('TailWind', 'm/s (north)',dims,1.e30))

#list(cnc[8]$dim$lon,cnc[8]$dim$lat,dimT),1.e30),
#Gvardef <- var.def.ncdf('CornGDD', 'C*days',
	#list(cnc[8]$dim$lon,cnc[8]$dim$lat,dimT),1.e30)
#var.def.ncdf('Livability', 'ratio',dims,1.e30),
#var.def.ncdf('FlightProp', 'ratio',dims,1.e30),

anc <-create.ncdf(outFile,fVars)
	
put.var.ncdf(anc,"CornGDD",cornGDD)
put.var.ncdf(anc,"FawGDD",fawGrw)
put.var.ncdf(anc,"Corn",datC)
#put.var.ncdf(anc,"Livability",Life)
#put.var.ncdf(anc,"FlightProp",Flight)
put.var.ncdf(anc,"TailWind",TailWind)
att.put.ncdf(anc,0,"PlantingTimes",plantNums)
att.put.ncdf(anc,0,"HarvestTimes",harvestNums)
att.put.ncdf(anc,0,"CornThres",CornThres)

#put.var.ncdf(anc,"Lows",datL)
junk <- close.ncdf(anc)
#junk <- close.ncdf(hnc)
proc.time() - ptm
