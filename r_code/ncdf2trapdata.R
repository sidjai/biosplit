#! C:/Program Files/R/R-3.1.1/bin/x64/Rscript.exe
realWd <- gsub("/r_code","",ifelse(grepl("System",getwd()),dirname(sys.frame(1)$ofile),getwd()))
load(paste(realWd,"cfg.Rout",sep="/"))

 
require(rgdal)
require(raster)
require(ncdf)


ncFile <- paste(cfg$SimOutFold,"Final.nc",sep="/")
outFile <-paste(cfg$SimOutFold,"TrapFinal",sep="/")

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
	res <- rbind(res, fillWid("",width))
	res <- rbind(res, fillWid(paste("#",Assump),width))
	res <- rbind(res, fillWid(paste("#",simData),width))
	res <- rbind(res, fillWid(paste("# Trap file:",cfg$trapName),width))
	res <- rbind(res, fillWid("",width))
	res <- rbind(res, fillWid(paste("# Used Nearest neighbor:",
		paste(nnSet[-1],collapse=", ")),width))
	
	return(res)
}
fillWid <- function(str,wid){
	val <- c(str,vapply(1:(wid-1),function(x) "",""))
}

rebuildNc <- function(){
	di <- seq(8,365,7)
	dates <- vapply(di, function(x){
	strftime(
		strptime(paste(toString(x),toString(cfg$year)),"%j %Y")
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

inputs <- read.csv(cfg$TrapLoc,stringsAsFactors=FALSE)
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
nnSet <- ""

for (el in seq(1, inSize[1])){
	for(co in seq(1, inSize[2])){
		tab[fi,co]<-inputs[[co]][[el]]
		tab[fi+1,co]<-inputs[[co]][[el]]
	}
	
	tab[fi,inSize[2]+1] <-"FL"
	tab[fi+1,inSize[2]+1] <-"TX"

	#do Time series
	if (!cfg$totFlag) mod <- rebuildNc()
	tSer[fi,] <- mod$FLMoth[xb[el],yb[el],]
	tSer[fi+1,] <- mod$TXMoth[xb[el],yb[el],]
	
	#If no moths in area, try nearest neighbor
	#Reasons: Beach area, near national park, dead spot in corn
	totMoth <- sum(tSer[fi,],tSer[fi+1,])
	if (totMoth == 0){
		nnind <- matrix(data = 0, nrow = 1, ncol = 2)
		#load up inds
		dist <- ifelse(grepl("Miami",tab[fi,2]),3,1)
		for(xp in seq(xb[el]-dist,xb[el]+dist)){
			for(yp in seq(yb[el]-dist,yb[el]+dist)){
				nnind <- rbind(nnind,cbind(xp,yp))
			}
		}
		nnind <- nnind[-1,]
		
		nnk <- 1
		while(totMoth==0 && nnk <= dim(nnind)[1]){
			nns <- rbind(mod$FLMoth[nnind[nnk,1],nnind[nnk,2],],
				mod$TXMoth[nnind[nnk,1],nnind[nnk,2],])
			totMoth <- sum(nns)
			nnk <- nnk+1
		}
		if (nnk <= dim(nnind)[1]){
			tSer[fi,] <- nns[1,]
			tSer[fi+1,] <- nns[2,]
			nnSet <- c(nnSet,tab[fi,2])
		}
	}

	fi<-fi+2
}
tSer <-tSer[,1:52]
colnames(tab) <-c(names(inputs),"Origin")
#write the dates as the column name
days <- seq(8,365,7)
colnames(tSer) <- vapply(days, function(x)
	strftime(
		strptime(paste(toString(x),toString(cfg$year)),"%j %Y")
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
