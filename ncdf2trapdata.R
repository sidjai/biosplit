
rm(list=ls(all=TRUE))

ptm <- proc.time()

direcNC <- "C:/Users/Siddarta.Jairam/Documents/Hysplit Out Moth table"
#runName <-"runcornThres"
runName <- "runTiredFix"
runName <- "runMaineMultiLife"
year <- 11
trapDir <- "C:/Users/Siddarta.Jairam/Documents/Documentation"
trapName <- "FAW Data Locations 2011 (3nd version).csv"
#trapName  <- "Selected FAW Trap Locations.csv"
trapLoc <- paste(trapDir,trapName, sep="/")

totFlag <- 0 #Use the single nc file, or the single nc files
 
require(rgdal)
require(raster)
require(ncdf)



ncFile <- paste(direcNC,(year+2000),runName,"Final.nc",sep="/")
outFile <-paste(direcNC,(year+2000),runName,"TrapFinal",sep="/")
sliceFiles <- paste(direcNC,(year+2000),runName,"ncs",sep="/")
nc <- open.ncdf(ncFile)

Assump <- att.get.ncdf(nc,0,"Assumptions")$value
simData <- att.get.ncdf(nc,0,"simData")$value

varNames <- names(nc$var)
mod<-lapply(varNames, function(x) get.var.ncdf(nc,x))
names(mod)<- varNames

close.ncdf(nc)


trap2block <-function(vin,coor){
	if (coor ==1) {
		mapdim <-nc$dim$lon$val
	} else {
		mapdim <-nc$dim$lat$val
	}

	diff <- abs(mapdim -vin)
	return(which.min(diff))
}
addAppendix <- function(res){
	width <- dim(res)[2]
	res <- rbind(res,vapply(1:width,function(x) "",""), c(paste("#",Assump),vapply(1:(width-1),function(x) "","")))
	res <- rbind(res,c(paste("#",simData),vapply(1:(width-1),function(x) "","")))
	res <- rbind(res,c(paste("#Trap file",trapName),vapply(1:(width-1),function(x) "","")))
	return(res)
}

rebuildNc <- function(){
	di <- seq(8,365,7)
	dates <- vapply(di, function(x){
	strftime(
		strptime(paste(toString(x),toString(2000+year)),"%j %Y")
		,"Moth_%m%d%y.nc")
	},"")
	Txfiles <- vapply(dates,function(x)paste0(sliceFiles,"/TX",x),"")
	Flfiles <- vapply(dates,function(x)paste0(sliceFiles,"/FL",x),"")
	slfiles <- list(Txfiles,Flfiles)
	out <- list(array(0, dim=dim(mod$TXMoth)),array(0, dim=dim(mod$TXMoth)))
	for (type in 1:2){
		for (k in seq(1,length(di))){
			if(file.exists(slfiles[[type]][[k]])){
				sl <- open.ncdf(slfiles[[type]][[k]])
				out[[type]][,,k] <- get.var.ncdf(sl,"Count")
				close.ncdf(sl)
			}
			
		}
	}
	names(out) <- c("TXMoth", "FLMoth")
	return(out)
}

inputs <- read.csv(trapLoc,stringsAsFactors=FALSE)
LonIn <- inputs$Lon
LonIn[which(LonIn>0)] <- (-LonIn[which(LonIn>0)])
xb <- vapply(LonIn,function(x) trap2block(x,1),1)
yb <- vapply(inputs$Lat,function(y) trap2block(y,2),1)



inSize <- dim(inputs)
tab <- matrix(nrow=2*inSize[1],ncol=inSize[2]+1)
tSer <- matrix(nrow=2*inSize[1],ncol=52)
#intiatialize the out table
add <- c("TX","FL")
fi<-1

for (el in seq(1, inSize[1])){
	for(co in seq(1, inSize[2])){
		tab[fi,co]<-inputs[[co]][[el]]
		tab[fi+1,co]<-inputs[[co]][[el]]
	}
	
	tab[fi,inSize[2]+1] <-"FL"
	tab[fi+1,inSize[2]+1] <-"TX"

	#do Time series
	if (!totFlag) mod <- rebuildNc()
	tSer[fi,] <- mod$FLMoth[xb[el],yb[el],]
	tSer[fi+1,] <- mod$TXMoth[xb[el],yb[el],]


	fi<-fi+2

}
tSer <-tSer[,1:52]
colnames(tab) <-c(names(inputs),"Origin")
#write the dates as the column name
days <- seq(8,365,7)
colnames(tSer) <- vapply(days, function(x)
	strftime(
		strptime(paste(toString(x),toString(2000+year)),"%j %Y")
	," %m/%d/%y"),"")

outh <- cbind(tab,tSer)
outh <- addAppendix(outh)

#Now do the vertical casedim(tSer)[1]*2*inSize[1]
outv <- matrix(nrow=1,ncol=inSize[2]+5)
vertNeed <- seq(1,dim(tab)[2]-1)
r <- 1
blank <- vapply(1:(inSize[2]+1),function(x) "","")

while (r<=dim(tab)[1]){
	outv <- rbind(outv,c(tab[r,vertNeed],colnames(tSer)[1],days[1],tSer[r,1],tSer[r+1,1],"New station"))
	for (ti in 2:52){
		#outv <- rbind(outv,c(blank,colnames(tSer)[ti],tSer[st,ti]))
		outv <- rbind(outv,c(tab[r,vertNeed],colnames(tSer)[ti],days[ti],tSer[r,ti],tSer[r+1,ti],""))
	}
	r <- r+2
	
}
outv <- outv[2:dim(outv)[1],]
colnames(outv) <- c(colnames(tab)[vertNeed], "Date","Day","FL Moths","TX Moths","New")

outv <- addAppendix(outv)

write.csv(outh,paste0(outFile,"h.csv"),row.names=FALSE)
write.csv(outv,paste0(outFile,"v.csv"),row.names=FALSE)

print(proc.time() - ptm)
