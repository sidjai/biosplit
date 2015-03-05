#! C:/Program Files/R/R-3.1.1/bin/x64/Rscript.exe
# source("C:/Users/Siddarta.Jairam/Documents/MothMigrationModel/r_code/iterateHYSPLIT.R")
rm(list=ls(all=TRUE))
#options(show.error.locations=TRUE)
tic <- Sys.time()

require(rgdal)
require(raster)
require(ncdf)

realWd <- gsub("/r_code","",ifelse(grepl("System",getwd()),dirname(sys.frame(1)$ofile),getwd()))
load(paste(realWd,"cfg.Rout",sep="/"))
nc <- open.ncdf(cfg$AprioriLoc)

varNames <- names(nc$var)
Plant <- att.get.ncdf(nc,0,"PlantingTimes")$value
Harvest <- att.get.ncdf(nc,0,"HarvestTimes")$value
CornThres <- att.get.ncdf(nc,0,"CornThres")$value

apr<-lapply(varNames, function(x) get.var.ncdf(nc,x))
names(apr) <- varNames
close.ncdf(nc)
xmapvec <- nc$dim$lon$vals
ymapvec <- nc$dim$lat$vals

bndx <- c(min(xmapvec),max(xmapvec))
bndy <- c(min(ymapvec),max(ymapvec))

vars <- c('simType','stAmount','mothThres','cohortThres',
					'cohortGDDThres','windThres',
					'infestThres','infestLmt','lifeSpan','oviDay',
					'capEggs','eggsPerInfest','endDay',
					'metDataType','succFlightLim',
					'flightPropAfterSilk','flightPropBeforeSilk','skip')
Assump <- vapply(vars,function(x) paste0(x, " = ",toString(cfg[[match(x,names(cfg))]])),"")
Assump <- c(Assump,paste0("CornThres = ",CornThres),"Starting locations")

for (ff in seq(1,dim(intGrids)[1])){
	Assump <- c(Assump,toString(intGrids[ff,]))
}

Assump <- c(Assump,
	paste("Planting times", toString(Plant), sep= " = "),
	paste("Harvest times", toString(Harvest), sep= " = "))

names(Assump) <- NULL
Assump <-paste(Assump,collapse = '; ')

#get the simulation criteria as supplied in the setup.cfg file
#Should have been changed before simulation,
#Should be written if you want non-default conditions

simData <-readLines(paste(cfg$HyWorking,"setup.cfg",sep= "/"),warn=0)
simData <- as.matrix(simData)

# Get some data off the Control file too
intCon <- readLines(paste(cfg$HyWorking,"CONTROL",sep="/"))

simData <- rbind(simData,"Control Variables")
hourInd <- charmatch("12",intCon)
depo <- seq(length(intCon)-3,length(intCon))

simData <- rbind(simData,
	paste0("Vertical Motion routine = ", intCon[hourInd+1],","),
	paste0("Top of Model = ", intCon[hourInd+2],","))
simData <- rbind(simData,"Deposition = |", paste(intCon[depo] ,collapse = '|'))

simData <-paste(simData,collapse = ' ')
simData <- gsub(",",";",simData)

simEmploy <- switch(cfg$simType,HYSPLIT=2,Fake=3,1)

mMothOut <- CohortOut <- list(array(0, dim=c(dim(apr$Corn),52)),array(0, dim=c(dim(apr$Corn),52)))


############################################
#Lots of functions
############################################

makeLife <- function(flagMoth,loc,GDD,origin){
	
	life <- list()
	if (length(dim(loc))<2){
		loc <- t(as.matrix(loc))
	}
	life$grid <- loc
	life$origin <- origin
	
	if (flagMoth) {
		life$daysOld <- GDD
		life$numEggs <- cfg$capEggs
		life$succFlight <- 0
		
	} else life$GDD <- GDD
	
	return(life)
	
}

makeREADME <- function(readmeLoc,rundirec,masFlag){
	
	indvReadme <- rbind(
		cfg$runName,
		'\tDescription:',
		paste('\tRun Date:',strftime(Sys.time())),
		'\tRun Duration:',
		paste('\tCode Changes:',cfg$codeChanges),
		'\tResult notes:',
		paste('\tAssumptions:',Assump),
		paste('\tSimData:',simData),
		'#############################################')
	rownames(indvReadme) <- NULL
	if (masFlag){
		if ( file.exists(readmeLoc)){
			bef <- readLines(readmeLoc)
			strun <- grep(cfg$runName,bef)
			endrun <- grep('[#########]',bef)
			if(length(strun)>0){
				delvec <- seq(strun,endrun[[findInterval(strun,endrun)+1]])
			} else delvec <- integer(0)
			
			allSeq <- 1:length(bef)
			bef <- bef[!is.element(allSeq,delvec)]
			res <- rbind(indvReadme,as.matrix(bef))
		} else  res <- indvReadme
		
		writeLines(res,readmeLoc)
	}
	
	#Do the secondary condition file in every run
	CondFile <- paste(rundirec,"Conditions.txt",sep="/")
	writeLines(indvReadme[c(1,3,7,8,9)],CondFile)
}

map2block <-function(vin,coor, direction){
	tol <-.25
	box <-cbind(vin-tol,vin+tol)

	mapdim <- (if(coor ==1) xmapvec else ymapvec)
	
	if (direction ==1){
		#go from map units to a block number
		val <- vapply(1:length(vin),function(x){
			which((mapdim>box[x,1]) & mapdim<max(box[x,2]))[1]
			},1)
		
	} else {
		# go from block number to map units
		val<-floor((10^3)*mapdim[vin])/10^3

	}
	return(val)

}

testEnv <- function(lpop,w=-9999){
	
	#profName <- 
	#Rprof("C:/Users/Siddarta.Jairam/Documents/iterateProf3.out",memory.profiling = TRUE)
	#Rprof("")
	#summaryRprof("C:/Users/Siddarta.Jairam/Documents/iterateProf3.out")
	popType <- switch(names(lpop[[1]])[3],daysOld=1,GDD=0)
	if (w <0){
		lpop <- deconst(lpop)
	} else {
		lpop <- list(lpop[[w]])
	}
	tab <- makePopTable(lpop,verboseNames=1)
	
	xbs <- vapply(as.numeric(tab[,1]),function(x) map2block(x,1,1),1)
	ybs <- vapply(as.numeric(tab[,2]),function(x) map2block(x,2,1),1)
	ind  <- cbind(xbs,ybs)
	cAmt <- apr$Corn[ind]
	cGDD <- round(apr$CornGDD[cbind(ind,di)],2)
	Livability <- howLivable(cGDD)
	#Livability <- vapply(cGDD,function(x) howLivable(x),1)
	if(popType){
		eg <- as.matrix(lapply(lpop,function(x) x$numEggs))
		tab <- cbind(tab,xbs,ybs,cAmt,cGDD,Livability,eg)
	} else tab <- cbind(tab,xbs,ybs,cAmt,cGDD,Livability)
	return(tab)
			
}

findOnMap <- function(map,xl,xt,yl,yt,num=0){
	xbl <- map2block(xl,1,1)
	xbt <- map2block(xt,1,1)
	ybl <- map2block(yl,2,1)
	ybt <- map2block(yt,2,1)
	
	slice <- which(map[xbl:xbt,ybt:ybl]>num,arr.ind=TRUE)
	slice[,1]<-map2block(slice[,1]+xbl,1,2)
	slice[,2]<-map2block(slice[,2]+ybt,2,2)
	return(slice)
}
getxy <- function(bx,by,dir=2){

	return(c(map2block(bx,1,dir),map2block(by,2,dir)))

}

zstr<- function(num,dig=2){
	str <-toString(num)
	while (nchar(str)<dig){
		str <-paste("0",str,sep="")
	}
	return(str)
}

howLivable <- function(cGrowth){
	val <- ifelse(cGrowth < cfg$infestThres,0,
		ifelse(cGrowth<1000,.00075*cGrowth+.15,
		ifelse(cGrowth<1400,.9, 
		ifelse(cGrowth<2400,1.8-.00075*cGrowth,0))))
	return(val)
}

genFlightProp <- function(cGrowth){
	val <- ifelse(cGrowth==0,1,
		ifelse(cGrowth < 1400,cfg$flightPropBeforeSilk,
		ifelse(cGrowth < cfg$infestLmt,cfg$flightPropAfterSilk,1)))
	return(val)
	
}

changeInput <- function(dateChangeFlag, date,pop,PID=0){

	#get the place and amount of the population
	item <-pop$grid
	newSrc <- dim(item)[1]
	#write the emit file 
	newEmit<-c("YYYY MM DD HH    DURATION(hhhh) #RECORDS",
		"YYYY MM DD HH MM DURATION(hhmm) LAT LON HGT(m) RATE(/h) AREA(m2) HEAT(w)")
	base <- paste(strftime(date,"%Y %m %d"),"00")
	
	newEmit[3] <-paste(base,"0001",newSrc)
	

	xmap <- list()
	ymap <- list()

	for (ind in seq(1,newSrc)){
		xi <- map2block(item[ind,1],1,1)
		yi <- map2block(item[ind,2],2,1)
		xmap[ind] <- toString(map2block(xi,1,0))
		ymap[ind] <- toString(map2block(yi,2,0))
		areaCorn <- toString(ifelse(is.na(apr$Corn[xi,yi]),0,apr$Corn[xi,yi]*10000))
		
		newEmit[3+ind]  <- paste(base, "00 0100", ymap[[ind]], xmap[[ind]], "500.0", toString(item[ind,3]), areaCorn, "0.0")	
	}
	
	#done with emit file
	writeLines(newEmit,
		paste(cfg$HyWorking,paste0("EMITIMES",toString(PID)) ,sep="/"))
	#writeLines(newEmit,paste(direc,"archiveEmit",paste(cfg$HyBase,xmap[[1]],ymap[[1]],sep=""),sep="/"))

	#Control file

	#test <- file(paste(direc,"CONTROL",sep="/"),"r+")
	befCon <- readLines(paste(cfg$HyWorking,paste0("CONTROL.",toString(PID)),sep="/"))
	indMon <- charmatch("C:/",befCon)+1
	newCon <- befCon

	#Change the values that only change when the date changes
	if (dateChangeFlag==1){
		#change date at top
		newCon[1]<-strftime(date,"%y %m %d 00")

		#change the end date at the bottom
		#print(toString(charmatch("01 12 00",newCon)-1))
		newCon[charmatch("01 12 00",newCon)-1] <- strftime(date,"%y %m %d 12 00")
		
		#change month if it needs it
		newMon<- tolower(strftime(date,"%b"))
		if (!grepl(newMon,befCon[indMon])){
			#Change the month
			newCon[indMon] <- paste(cfg$metDataType, ".",newMon,cfg$year-2000,sep="")
		}

		#Change the year every time the date changes because it is easier to code
		#newCon[indMon-1] <- gsub("20[0-9][0-9]",strftime(date,"%Y"),newCon[indMon-1])
		newCon[indMon-1] <- paste0(cfg$MetARLFold,"/")
	}

	#change the vales that are always gonna be different
	newCon[2]<-newSrc
	befSrc <-as.numeric(befCon[2])
	diff <-befSrc-newSrc
	if (diff<0){
		#insert new places
		for (er in seq(1,-diff)){
			newCon <- append(newCon,"placeholder",2)
		}
		
	} else if (diff>0){
		#Delete lines
		for (er in seq(1,diff)){
			newCon <- newCon[-3]
		}
	}
	
	#now that the places are right, just assign the sources
	for (sr in seq(1,newSrc)){
		newCon[2+sr]=paste(ymap[[sr]],xmap[[sr]],"500.0")
	}



	

	#finished changing things
	writeLines(newCon,paste(cfg$HyWorking,paste0("CONTROL.",toString(PID)),sep="/")) 

		#/Starting time at top
		#/end time at the grid def to make output small
		#/starting locations
		#/met file if its crosses over a month
		#/EMITIMES start time and indv
}

callHysplit <- function(hold,PID){
	tryCatch(
		junk <- shell(paste(paste("CD",cfg$HyWorking),paste(paste(cfg$HyBase,cfg$HyConc,sep="/"),toString(PID)),sep=" && "),
			intern=hold,wait=hold),
		warning = function()cat("too little computing space, supply more or limit model"),
		error = function(cond){
			Sys.sleep(3)
			junk <- shell(paste(paste("CD",cfg$HyWorking),paste(paste(cfg$HyBase,cfg$HyConc,sep="/"),toString(PID)),sep=" && "),
				intern=hold,wait=hold)
			}
	)
	
	
}

runHysplit <- function(cutoff=.01, plotFlag=0, hold = TRUE, call=TRUE,PID=1){

	#change directory, then call hysplit
	if(call) callHysplit(hold,PID)
	cdump <- paste0("cdump",toString(PID))
	if (plotFlag==1){
		#Convert to plot
		sadg <- shell(paste(paste("CD",cfg$HyWorking),paste(cfg$HyBase,paste(cfg$HyPlt, cdump, "-k0"),sep="/"),sep=" && "),intern=TRUE)
		
		if (cfg$plotWriteFlag==1){
			
			endTok <- strftime(tPos,"%m%d%y")
			lst <- list.files(cfg$rawHyPlotFold,endTok)
			file.rename(paste(cfg$HyWorking,"concplot.ps",sep="/"),
					paste0(cfg$rawHyPlotFold,"/",strftime(tPos,"%m%d%y"),zstr(length(lst)),".ps"))
		} else {

			#display plot
			shell(paste(cfg$HyWorking,"concplot.ps",sep="/"))
		}
	}


	#make into a ascii


	repeat{ 
		textFile<-shell(paste(paste("CD",cfg$HyWorking),
			paste(cfg$HyBase,
				paste("con2asc.exe",cdump, "-m -d")
				,sep="/")
			,sep=" && ")
			,intern=TRUE)
		if (length(textFile)<1){
			callHysplit(hold=TRUE,PID)
		} else break
	}
		

	datum<-read.csv(paste(cfg$HyWorking,gsub("^\\s+|\\s+$", "", textFile[1]),sep="/"),header = FALSE)
	niceDatum <- as.matrix(datum)
	#swap cols so it goes lat lon instead of lon lat
	niceDatum <- niceDatum[,c(2,1,3),drop=FALSE]
	
	#Cutoff the amount and add to the largest one
	if (length(dim(niceDatum))==2){
	badInd <- which(niceDatum[,3] < cutoff)
	if (length(badInd)>0){
		allInd <- seq(1,dim(niceDatum)[1])
		goodInd <- allInd[!is.element(allInd,badInd)]
		maxInd <- which.max(niceDatum[,3])
		add <- sum(niceDatum[badInd,3])
		niceDatum[maxInd,3] <- niceDatum[maxInd,3]+add
		niceDatum <- niceDatum[goodInd,,drop=FALSE]
	}
	}
	
	#round to the nearest digit in cutoff
	if ((cutoff %% 1) != 0) {
        	dig <-nchar(strsplit(sub('0+$', '', as.character(cutoff)), ".", fixed=TRUE)[[1]][[2]])
    	} else {
        	dig <- 0
    	}
	if (dim(niceDatum)[1]>1){
		niceDatum[,3] <- round(niceDatum[,3],dig)
	}
	

#	names(niceDatum)[1]<-"Lon"
# 	names(niceDatum)[2]<-"Lat"
# 	names(niceDatum)[3]<-"Amt"
	
	return(niceDatum)
}
round2number <- function(val,num){
	val <- val - val%%(num)
	return(val)
}
#test <- multiHysplit(gog,0,tPos,1)
#changeInput(0,tPos,gog,PID=1)
#test2 <- runHysplit(.1,1,hold=TRUE,call=TRUE,PID=1)

multiHysplit <- function(pop,dateChange,date,shPlotFlag){
	totPopLen <- dim(pop$grid)[1]
	if (totPopLen>9){
		inPop <- list(pop,pop,pop)
		int1 <- floor(totPopLen/3)
		int2 <- floor(totPopLen*2/3)
		inPop[[1]]$grid <- pop$grid[1:int1,,drop=FALSE]
		inPop[[2]]$grid <- pop$grid[int1:int2,,drop=FALSE]
		inPop[[3]]$grid <- pop$grid[int2:totPopLen,,drop=FALSE]
		holdvec <- c(FALSE,FALSE,TRUE)
	} else {
		inPop <- list(pop)
		holdvec <- c(TRUE)
	}
	out <- matrix(nrow=1,ncol=3)
	for (gg in seq(1,length(inPop))){
		changeInput(dateChange,date,inPop[[gg]],PID=gg)
		callHysplit(hold=holdvec[gg],PID=gg)	
	}
	prc <- shell("tasklist",intern=TRUE)
	while(length(which(grepl("hycs",prc)))>0){
		prc <- shell("tasklist",intern=TRUE)
	}
	
	for (gg in seq(1,length(inPop))){
		out <- rbind(out,runHysplit(.1,shPlotFlag,hold=holdvec[gg],call=FALSE,PID=gg))
	}
	outTot <- dim(out)[1]
	if(outTot>2){
		
		out <- out[2:outTot,,drop=FALSE]
		r <-1
		while (r<dim(out)[1]){
			#same location, within the threshold of GDD, and same origin
			vec <- which((out[,1]==out[r,1] &
											out[,2]==out[r,2]))
			if (length(vec)>1){
				out[r,3] <- round(sum(out[vec,3]),1)
				delVec <- vec[which(vec!=r)]
				out <- out[-delVec,,drop = FALSE]
			}
			r <- r+1
			
		}
	}
	return(out)
}

testFakeHysplit <- function(tgrd){
	ogrd <- tgrd
	for (gj in seq(1,dim(tgrd)[1])){
		xrandOffset <- runif(1,-5,5)
		yrandOffset <- runif(1,0,5.0)
		xrandOffset <- round2number(xrandOffset,.455)
		yrandOffset <- round2number(yrandOffset,.357)
		xfake <- tgrd[[gj,1]] + xrandOffset
		yfake <- tgrd[[gj,2]] + yrandOffset
		ogrd[[gj,1]] <- xfake 
		ogrd[[gj,2]] <- yfake
	}
	return(ogrd)
}

cleanGrid <-function(pop,thres=1){
	amt <- pop$grid[,3]
	gmax <- which.max(amt)
	allSet <- 1:length(amt)
	testDieHard <- which(is.na(amt)| amt <0)
	testDieSoft <- which(amt < thres)
	
	softSet <- testDieSoft[!is.element(testDieSoft,testDieHard)]
	pop$grid[gmax,3] <- pop$grid[gmax,3] + sum(pop$grid[softSet,3])
	goodSet <- allSet[!is.element(allSet,union(testDieHard,testDieSoft))]
	pop$grid <- pop$grid[goodSet,,drop = FALSE]
	
# 	while(gg>0){
# 		amt <- pop$grid[gg,3]
# 		
# 		if ((is.na(amt) || amt<0)){
# 			pop$grid <- pop$grid[-gg,,drop = FALSE]
# 		} else if (amt<thres){
# 		  	pop$grid[gmax,3] <- pop$grid[gmax,3]+pop$grid[gg,3]
# 				pop$grid <- pop$grid[-gg,,drop = FALSE]			
# 		} else {
# 			pop$grid[gg,3] <- round(pop$grid[gg,3],1)
# 			gg <- gg-1
# 		}
# 	}

	return(pop)

}

cleanPop <- function(stPop){
	test <- vapply(stPop,function(x) dim(x[[1]])[1],1)
	ind <- which(test==0)
	if (length(ind)>0) stPop <- stPop[-ind,drop = FALSE]
	if (length(stPop)==0) stPop=list()

	return(stPop)
}

cleanAll<- function(lpop,thres=1){
	if(length(lpop)>0){
		lpop <- lapply(lpop,function(x) cleanGrid(x,thres))
		lpop <- cleanPop(lpop)
	}
	return(lpop)
}
willFly <- function(pop, day, genFlag){
	#From the population figure out how many fly than make two knew populations (stayFAW and mFAW)
	
	stayFaw <- mFaw <- pop
	xs <- map2block(pop$grid[,1],1,1)
	ys <- map2block(pop$grid[,2],2,1)
	inds <- cbind(xs,ys,day)
	wind <- apr$TailWind[inds]
	cGDD <- apr$CornGDD[inds]
	
	if (genFlag) {expVal <- pop$grid[,3] * genFlightProp(cGDD)
	} else expVal <- pop$grid[,3] * (1-howLivable(cGDD))
	
	expVal[which(is.na(expVal)| expVal < 0 | is.na(wind))] <- 0
	
	numFly <- vector("numeric",length(expVal))
	
	larSet <- which(expVal>10^6.5)
	smallSet <- which((expVal>0 & expVal<10^6.5))
	if (length(larSet)>0){
		numFly[larSet] <- vapply(larSet,function(x) rnorm(1,mean=expVal[x],sd=sqrt(expVal[x])),1)
	}
	if (length(smallSet)>0){
		numFly[smallSet] <- vapply(smallSet,function(x) rpois(1, expVal[x]),1)
	}
	stayFaw$grid[,3] <- stayFaw$grid[,3] - numFly
	mFaw$grid[,3] <- numFly
	
# 	for (g in seq(1,dim(pop$grid)[1])){
# 		ind <- cbind(xs[g],ys[g],day)
# 		if (!is.na(apr$TailWind[ind])){
# 			cGDD <- apr$CornGDD[ind]
# 			#can fly
# 			if (genFlag) expVal <- pop$grid[g,3]*genFlightProp(cGDD)
# 			else expVal <-pop$grid[g,3]*(1-howLivable(cGDD))
# 			expVal <- ifelse(is.na(expVal) || expVal<0,0,expVal)
# 
# 			#rpois can't get over the maximum of the integer or it has NAs
# 			numFly <- ifelse(expVal<10^6.5,
# 					rpois(1, expVal),
# 					rnorm(1,mean=expVal,sd=sqrt(expVal)))
# 
# 			numFly <- sort(c(numFly,pop$grid[g,3]))[1]
# 			if (numFly <1) numFly <- 0
# 
# 			stayFaw$grid[g,3] <- (stayFaw$grid[g,3]-numFly)
# 			#print(c(numFly,stayFaw$grid[g,3]))
# 			mFaw$grid[g,3] <- numFly
# 		
# 		}
# 	}
	stayFaw <- cleanGrid(stayFaw)
	mFaw <- cleanGrid(mFaw)
	
	#spread out over 7 days if its a generational flight component
	if (genFlag){
		spread <- vector("list",7)
		s <-1:6
		amt <- vapply(s, function(x) exp(-(x-1)/2)-exp(-(x)/2),1)
		amt <- c(amt,1-sum(amt))
		for (si in seq(1,7)){
			spread[[si]] <- mFaw
			spread[[si]]$daysOld <- (si-1)
			spread[[si]]$grid[,3] <- round(spread[[si]]$grid[,3,drop = FALSE]*amt[si],2)
		}
		val <- list("stay"=stayFaw,"migrant"=spread)
	} else val <- list("stay"=stayFaw,"migrant"=mFaw)

	
	
	return(val)

}

lappend <- function(lst, obj) {
	num<-length(obj)
	if (num>0){
		if (length(names(obj))!=0) obj <- list(obj)
		lst <- c(lst,obj)
	}
	return(lst)
}

growMoths <- function(pop,day){
	newEggs <- list()
	grd <- pop$grid
	grdLen <- dim(grd)[1]
	remEggs <- rep.int(pop$numEggs,grdLen)

	#first do growth and death
	pop$daysOld <- pop$daysOld+1
	
	xs <- map2block(grd[,1],1,1)
	ys <- map2block(grd[,2],2,1)
	
	#Death
	if (pop$daysOld > cfg$lifeSpan){
		deathTest <- !logical(grdLen)
	} else {
		deathTest <- (is.na(xs) | is.na(ys))
	}
	
	pop$grid[deathTest,3] <- (-9999)
	
	
	#Lay Eggs?
	if (pop$daysOld>cfg$oviDay 
			&& pop$numEggs > cfg$eggsPerInfest){
		#row specific checks
		orind <- cbind(xs,ys,rep.int(day,grdLen))
		grCorn <- apr$CornGDD[orind]
		layTest <- (grd[,3] > 0
			& grCorn < cfg$infestLmt
			& grCorn > cfg$infestThres)
		
		
		remEggs[layTest] <- pop$numEggs-cfg$eggsPerInfest
		
		nEggs <- cfg$eggsPerInfest*grd[layTest,3]
		for (r in which(layTest)){
			nEggs = pop$grid[r,3]*cfg$eggsPerInfest
			newEggs <- lappend(newEggs, 
				makeLife(0,cbind(grd[r,1],grd[r,2],nEggs),0,pop$origin))
		}

	}
	
	opop <- pop
	#seperate the population if a portion did not lay eggs and the others did
	if (length(newEggs)>0){
		niq <- unique(remEggs)
		if( length(niq) > 1){
			opop <- list()
			for (q in seq(1,length(niq))){
				opop[[q]] <- pop
				ind <- which(remEggs==niq[q])
				opop[[q]]$grid <- pop$grid[ind,,drop=FALSE]
				opop[[q]]$numEggs <- niq[q]
			}
		} else opop$numEggs <- niq
	}
	
	if (length(names(opop))>0) opop <- cleanGrid(opop,thres=cfg$mothThres)

	
	out <- list(opop,newEggs)
	return(out)
}

growCohort <- function(lpop,di){
	
	tab <- makePopTable(lpop)
	xs <- map2block(tab[,1],1,1)
	ys <- map2block(tab[,2],2,1)
	org <- ifelse(tab[,5],"FL","TX")
	
	#growth
	aDD <- tab[,4]
	for (t in seq(di,di+6)){
		aDD <- aDD + apr$FawGDD[cbind(xs,ys,t)]
	}
	
	#tests for death and adulthood
	if (di<cfg$altCornDay) {deadTest <- (apr$Corn[cbind(xs,ys)] < 1)
	}	else deadTest <- (apr$CornGDD[cbind(xi,yi,di)] < cfg$infestThres)
	
	adultTest <- (aDD > cfg$cohortGDDThres)
	
	removeTest <- (adultTest | deadTest)
	
	#Creat adults
	tadult <- lapply(which(adultTest),function(x) makeLife(1,tab[x,1:3],0,org[x]))
	
	#set GDD for those that remain
	for (gg in which(!removeTest)){
		lpop[[gg]]$GDD <- aDD[gg]
	}
	
	#remove the ones that died or turned into an adult
	lpop <- lpop[!removeTest]
	
	return(list(lpop,tadult))
}

migrateDeath <- function(pop,day){
	
	grd <- pop$grid
	grdLen <- dim(grd)[1]
	
	xs <- map2block(grd[,1],1,1)
	ys <- map2block(grd[,2],2,1)
	ind <- cbind(xs,ys,rep.int(day,grdLen))
	
	outOfMap  <- (is.na(xs) | is.na(ys))
	
	grCorn <- apr$Corn[cbind(xs,ys)]
	grCornGDD <- apr$CornGDD[ind]
	
	if(pop$origin=="FL" && di < cfg$altCornDay){
		bakersfield <- (is.na(grCorn) | grCorn < 0.01)
	} else {
		bakersfield <- howLivable(grCornGDD)==0
	}
	
	deathTest <- (outOfMap | bakersfield)
	
	pop$grid[deathTest,3] <- (-9999)
	pop$grid[!deathTest,3] <- round(pop$grid[!deathTest,3],2)
	
	pop <- cleanGrid(pop,thres=cfg$mothThres)
	return(pop)
}

deconst <- function(lpop){

	if (length(lpop)>0){
	numRow <- vapply(lpop, function(x) dim(x$grid)[1],1)
	needsDecon <- which(numRow>1)
	if (length(needsDecon)>0){
	for (n in needsDecon){
		for (r in seq(2,numRow[n])){
			lpop<-lappend(lpop,lpop[[n]])
			lpop[[length(lpop)]]$grid<-lpop[[n]]$grid[r,,drop=FALSE]
		}
		lpop[[n]]$grid<-lpop[[n]]$grid[1,,drop = FALSE]
	}
	}
	}
	
	return(lpop)
}

makeRowVec<-function(x){
	vec <-as.matrix(x)
	if (dim(vec)[1]>1 && dim(vec)[2]==1) vec<-t(vec)
	return(vec)
}

makePopTable <- function(lpop,desOrigin=0,verboseNames=FALSE){
	if ( is.numeric(desOrigin))subpop <- lpop
	else {
		orgs <- vapply(lpop,function(x)x$origin,"")
		vec <- charmatch(orgs,desOrigin)
		subpop <-lpop[vec]
	}
	if ( length(subpop)>0){

	subpop <- deconst(subpop)
	

	mat <- t(vapply(subpop, function(x){
		cbind(x$grid,
			x[[3]],
			switch(x$origin,TX=0,FL=1))
		},c(1,1,1,1,1)))
	
	if(verboseNames){
		colnames(mat) <- c("Lon","Lat","Amt","Age","Origin")
		mat[,5] <- vapply(mat[,5],function(x) ifelse(x!=0,"FL","TX"),"TE")
	}
	
	return(mat)
	}
}

combineEggs <- function(lpop,thres=1){

	if (length(lpop)>1){
	tab <- makePopTable(lpop)
	xs <- map2block(tab[,1],1,1)
	ys <- map2block(tab[,2],2,1)
	mat <- cbind(xs,ys,tab[,5])
	
	allDups <- duplicated(mat)
	needComb <- allDups
	
	baseVals <- which(!allDups & duplicated(mat,fromLast=TRUE))
	for (r in baseVals){
		
		vec <- (mat[needComb,1]==mat[r,1] &
							mat[needComb,2]==mat[r,2] & 
							mat[needComb,3]==mat[r,3])
		vec <- which(needComb)[vec]
		
		if (length(vec)>0){
			lpop[[r]]$grid[1,3] <- tab[r,3] + sum(tab[vec,3])
			needComb[vec] <- FALSE
		}
				
	}
	lpop <- lpop[!allDups,drop = FALSE]
	}
	return(lpop)
	
}

combinelPop <- function(lpop){
  #Combine the large populations without lowering the grid items
  #Mostly to make the Hysplit run much shorter
  #don't use if changing GDD instead of Days old or any other var with memory
  olpop <- lpop
  mat <- t(vapply(lpop, function(x){
  	cbind(x[[3]],
			switch(x$origin,TX=0,FL=1),
  		x$succFlight)
  	},c(1,1,1)))
  
  allDups <- duplicated(mat)
  needComb <- allDups
  
  baseVals <- which(!allDups & duplicated(mat,fromLast=TRUE))
  for (r in baseVals){
  	
  	vec <- (mat[needComb,1]==mat[r,1] &
						mat[needComb,2]==mat[r,2] & 
						mat[needComb,3]==mat[r,3])
  	vec <- which(needComb)[vec]
  	
  	if (length(vec)>0){
  		#If they have the same stuff, paste the grids together
  		lpop[[r]]$grid <- makePopTable(lpop[c(r,vec)])[,1:3,drop=FALSE]
  		needComb[vec] <- FALSE
  	}
  	
  }
  lpop <- lpop[!allDups,drop = FALSE]
  
  if (length(lpop)==0) {return(olpop)
  } else{
  	
  	#go through the grids to see if any are at the same location ->add the amt
  	for (gg in seq(1,length(lpop))){
  		xs <- map2block(lpop[[gg]]$grid[,1],1,1)
  		ys <- map2block(lpop[[gg]]$grid[,2],2,1)
  		mat <- cbind(xs,ys)
  		
  	
  		allDups <- duplicated(mat)
  		needComb <- allDups
  		
  		baseVals <- which(!allDups & duplicated(mat,fromLast=TRUE))
  		for (r in baseVals){
  			
  			vec <- (mat[needComb,1]==mat[r,1] &
  								mat[needComb,2]==mat[r,2])
  			vec <- which(needComb)[vec]
  			
  			if (length(vec)>0){
  				#If they have the same stuff, add amounts
  				lpop[[gg]]$grid[r,3] <- sum(lpop[[gg]]$grid[c(r,vec),3])
  				needComb[vec] <- FALSE
  			}
  			
  		}
  		
  		lpop[[gg]]$grid <- lpop[[gg]]$grid[!allDups,,drop = FALSE]
  	}
		return(lpop)
  }
}



makeOutput <- function(lpop,out, lpop2=0, txtonlyFlag =0){
	#lpop is for migrants, lpop2 is for stationary moths

	tab <-makePopTable(lpop)
	#if (length(tab)<7) tab<-t(as.matrix(tab))

	if (!is.numeric(lpop2)){
    poptype <- "Moth"
		tab2 <- makePopTable(lpop2)

		#add the second population with an additional flag
		tab <- cbind(tab, t(t(rep(1,dim(tab)[1]))))
		tab2 <- cbind(tab2, t(t(rep(0,dim(tab2)[1]))))
		tab <- rbind(tab, tab2)

	} else poptype <- "Cohort"
	
	
	xs <- map2block(tab[,1],1,1)
	ys <- map2block(tab[,2],2,1)
	week<-as.integer(strftime(tPos,"%U"))
	#reorganize sparse matrix into full matrix
	slice <- list(array(0, dim=c(dim(apr$Corn))),array(0, dim=c(dim(apr$Corn))))
	
	for (ae in seq(1,dim(tab)[1])){
		type <-tab[ae,5]+1
		slice[[type]][xs[ae],ys[ae]]  <- slice[[type]][xs[ae],ys[ae]] + tab[ae,3]
		#out[[type]][xs[ae],ys[ae],week] <- out[[type]][xs[ae],ys[ae],week] + tab[ae,3]
    
	}
	if (!txtonlyFlag){
  	for (pt in seq(1:length(out))){
  		out[[pt]][,,week] <- slice[[pt]]
  	}
	}

	str <- paste(poptype,"_week%s_%s.txt",sep="")
	if(cfg$writeFlag){
		write.table(tab,file=paste(
			cfg$SimOutFold,"/",
			sprintf(str,zstr(wk),strftime(tPos,"%m%d%y")),
			sep=""),col.names=FALSE,row.names=FALSE)
    
		#write the nc slices
		#definitions for the single nc files
		dims <- list(nc$dim$lon,nc$dim$lat)
		fVars <- list(var.def.ncdf('Count', 'number',dims,1.e30))
		varNames <- list(paste("TX",poptype,sep=""),
				paste("FL",poptype,sep=""))
		for(vInd in seq(1,length(varNames))){
			outFile <- paste(cfg$SimOutFold, "ncs",
											 paste(varNames[[vInd]],
											 			strftime(tPos,"%m%d%y.nc"),sep="_"),sep="/")

			onc <-create.ncdf(outFile,fVars)
			put.var.ncdf(onc,"Count",slice[[vInd]])
			close.ncdf(onc)
		}
	}
	
	return(out)
}

getDate<- function(month, day,yr){
	tPos<-strptime(paste(month,day,toString(yr)),"%b %d %y")
	return( strftime(tPos,"%j"))
}

cleangrowMoths <- function(lpop,lEggs,di){
	addMoth =list()
	if (length(lpop)>0){
		lpop <- cleanPop(lpop)
		for (mi in seq(1, length(lpop))){

			tres <- growMoths(lpop[[mi]],di)
			
			if (length(names(tres[[1]]))>0){
				lpop[[mi]] <- tres[[1]]
			} else {
				lpop[[mi]] <- tres[[1]][[1]]
				addMoth <- lappend(addMoth,tres[[1]][2:length(tres)])
			}
			lEggs <-lappend(lEggs,tres[[2]])
		}
		lpop <- lappend(lpop,addMoth)
		#lpop <- lapply(lpop,function(x) cleanGrid(x, thres=mothThres))
		lpop <- cleanPop(lpop)
		
	}
	
	return(list(lpop,lEggs))


}

overWinter <- function(InputMoth,day){
	test <- vapply(1:length(InputMoth),function(x) 0,0)
	
	for (i in seq(1, length(InputMoth))){
		xi <-map2block(InputMoth[[i]]$grid[1],1,1)
		yi <-map2block(InputMoth[[i]]$grid[2],2,1)
		
		if (apr$CornGDD[[xi,yi,day]] >= cfg$infestThres){
			test[i] <- 1
			
		}
	}
	return(test)
	
}

###############################################################################
#Actual program
###############################################################################

#Create README file
makeREADME(cfg$READMELoc,cfg$SimOutFold,cfg$makeReadmeFlag)

Moth <- list()
mMoth <- list()
youngAdults <- list()
youngMig <- list()
Eggs <- list()
Cohort <- list()
winterPop <- list()
mig <- 1
wk <- 1

#Get the input cohort areas

for (ig in seq(1,dim(intGrids)[1])){
	tag<-ifelse(ig<=cfg$Flnum,"FL","TX")
	winterPop[[ig]] <- makeLife(0,intGrids[ig,],0,tag)
}
# get the start day from the overwinter populations
startDay <- 45 #first guess
repeat{
	Bvec <- overWinter(winterPop,startDay)
	if (length(which(Bvec==1)>0)){
		Cohort <- lappend(Cohort,winterPop)
		winterPop <- list()
		break
	}
	startDay <- startDay+1
}

#startDay <-di+1
for(di in seq(startDay,cfg$endDay)){
	
	#Add input Cohorts when the infestation is possible
	if(length(winterPop) >0){
		
		#Cohort <- lappend(Cohort,winterPop[Bvec])
		#witnerPop  <- winterPop[!winterPop[Bvec]]
	}
	tPos<-strptime(paste(toString(di),toString(cfg$year)),"%j %Y")
	k <- 1
	

	#
	#add the newly emited moths to their "career paths"
	#
	
	youngAdults <- cleanAll(youngAdults,cfg$mothThres)

	if (length(youngAdults)>0){
	for (ya in seq(1, length(youngAdults))){
		tSplit <- willFly(youngAdults[[ya]],di,1)
		
		if (colSums(tSplit[[1]]$grid,na.rm = TRUE)[3] > cfg$mothThres){
			Moth <- lappend(Moth,tSplit[[1]])
		}
		sp <- cleanPop(tSplit[[2]])
		if (length(sp)>0 
		    && colSums(makePopTable(sp),na.rm = TRUE)[3] > cfg$mothThres){
			#mMoth <- lappend(mMoth,tSplit[[2]])
			youngMig <- rbind(youngMig,tSplit[[2]])
		}

	}
	youngAdults=list()	
	}
  
	siz <- dim(youngMig)
	if (!is.null(siz) && length(youngMig)>0){
# 		dind <- ifelse(di%%7==0,8,(di%%7))
		mMoth <- lappend(mMoth,combinelPop(youngMig[,1,drop = FALSE]))
		youngMig <- youngMig[,-1,drop = FALSE]
# 		for (ym in seq(1:siz[1])){
# 			mMoth <- lappend(mMoth,youngMig[[ym,dind]])
# 		}
	}

	#Moth algebra
	tpop <- list()
	tpop <- cleangrowMoths(Moth,Eggs,di)
	Moth <- tpop[[1]]
	Eggs <- tpop[[2]]

	tpop <- list()
	tpop <- cleangrowMoths(mMoth,Eggs,di)
	mMoth <- tpop[[1]]
	Eggs <- tpop[[2]]


	mMoth <- cleanPop(mMoth)
	#mMoth <- combinelPop(mMoth)
	
	
	if (length(mMoth)>0){
	cat("Day", di, strftime(tPos," %m/%d/%y"),"\n")

	#some of the migrant moths may decide to settle down
	#so they will be subtracted from the population and added to the Moth list

	mi<-1
	while (mi <= length(mMoth)){

		tSplit <-willFly(mMoth[[mi]],di,0)
		sums <- lapply(tSplit,function(x)  colSums(x$grid)[3])
		minmoth <- lapply(tSplit,function(x) cfg$mothThres*dim(x$grid)[1])

		if (sums[[1]] > minmoth[[1]]) Moth<-lappend(Moth,tSplit[[1]])
			
		if (sums[[2]] < minmoth[[2]] || minmoth[[2]] == 0) mMoth <- mMoth[-mi,drop = FALSE]
		else{
		   #Got Moths that want to fly
		   xi <-map2block(tSplit[[2]]$grid[1,1],1,1)
		   yi <-map2block(tSplit[[2]]$grid[1,2],2,1)
		   #newWind <-ifelse((windThres<0 && mMoth[[mi]]$origin=="FL" && di<100),2.5,windThres)
		   newWind <- cfg$windThres
		   wi <- apr$TailWind[xi,yi,di-1]
		   condW <- (is.na(wi) || wi<newWind)
		   condSk <- (length(which(cfg$skip==di))!=0)
		   condSucc <- mMoth[[mi]]$succFlight > cfg$succFlightLim
		   if (condW || condSk || condSucc){
		   	mMoth[[mi]] <- tSplit[[2]]
		   	mMoth[[mi]]$succFlight <- 0
				mi <- mi+1
		   } else{
			#Got Moths that can fly
			
			#take the migratory FAW and change the CONTROL and EMITIMES files to beg a HYSPLIT run
			#changeInput(ifelse(mi==1,1,0),tPos,tSplit[[2]])
		
		
			#####################################################
			#Migrate the species
			#####################################################
			shouldPlot <-ifelse((di >=cfg$invPlotFlag && mig%%10==0),1,0)
			if (simEmploy == 1){
				mMoth[[mi]]$grid <- multiHysplit(tSplit[[2]],1,tPos,shouldPlot)
			} else if (simEmploy == 2){
				mMoth[[mi]]$grid <- runHysplit(.1,shouldPlot)
			} else {
				mMoth[[mi]]$grid <- testFakeHysplit(tSplit[[2]]$grid)
			}

			mig <- mig+1

			#distribute the GDDs
			#mMoth[[mi]]$GDD <- rep(mMoth[[mi]]$GDD,dim(mMoth[[mi]]$grid)[1])

			#get rid of the Moths that went out of bounds
			mMoth[[mi]] <- migrateDeath(mMoth[[mi]],di)
			if (dim(mMoth[[mi]]$grid)[1]==0)  mMoth <- mMoth[-mi,drop = FALSE]
			else {
				mMoth[[mi]]$succFlight <- mMoth[[mi]]$succFlight + 1
				mi<-mi+1
			}
		   }
		}
		
		
	}
	}
  	#cat(length(mMoth))
	mMoth <- cleanAll(mMoth,cfg$mothThres)
	Moth <- cleanAll(Moth,cfg$mothThres)
	
	mMoth <- combinelPop(mMoth)
	Moth  <- combinelPop(Moth)

	#Test to see if it needs to output every day
	txtFlag <- (di==sort(c(cfg$outEveryDayStart,cfg$outEveryDayEnd,di))[2])
	if(txtFlag){
		if(length(mMoth)>0) try(mMothOut <- makeOutput(mMoth,mMothOut,Moth,1))
	}

	###########################
	#do the bio model every week
	###########################
	if (di%%7==1 & di<=359){
		#End of week
    
		#combine moths
		
		Eggs <- combineEggs(Eggs)

		youngMig <- list()
		if (di < 110){
			fldie <- c("TX")
			fldie <- c(fldie,vapply(Cohort,function(x)x$origin,""))
			fldie <- c(fldie,vapply(Moth,function(x)x$origin,""))
			fldie <- c(fldie,vapply(mMoth,function(x)x$origin,""))
			fldie <- c(fldie,vapply(Eggs,function(x)x$origin,""))
		
			if(length(which(fldie=="FL"))==0){
				stop("FL died off, abandon ship")
			}
		}
		#get all the outputs
		#txtFlag <- (di==sort(c(outEveryDayStart,outEveryDayEnd,di))[2])
		if (length(Cohort)>0) CohortOut <- makeOutput(Cohort,CohortOut)
		if (length(mMoth)>0){
			cat("End week", wk,"\n")
			mMothOut <- makeOutput(mMoth,mMothOut,Moth)
			wk<-wk+1
		}

		##
		#do biological model with cohorts
		##

		#Eggs
		#combine eggs
		if (length(Eggs)>0){
			Cohort <- lappend(Cohort,Eggs)
		}
		Eggs <- list()
		youngAdults <- list()
		
		#Clean cohort
		tCoh <- growCohort(Cohort,di)

		#Clean cohort
		Cohort <- cleanAll(tCoh[[1]],cfg$cohortThres)
		
		youngAdults <- tCoh[[2]]	
					
	}
	
}
if (cfg$writeFlag){
	dims <-list(nc$dim$lon,
		nc$dim$lat,
		dim.def.ncdf( "Time", "weeks", 1:52, unlim=TRUE ))

	fVars<- list(var.def.ncdf('TXCohort', '#Immature moths',dims,1.e30),
			 var.def.ncdf('FLCohort', '#Immature moths',dims,1.e30),
			 var.def.ncdf('TXMoth', '#Moths',dims,1.e30),
			 var.def.ncdf('FLMoth', '#Moths',dims,1.e30))
	onc <-create.ncdf(paste(cfg$SimOutFold,"Final.nc",sep="/"),fVars)

	put.var.ncdf(onc,"TXCohort",CohortOut[[1]])
	put.var.ncdf(onc,"FLCohort",CohortOut[[2]])
	put.var.ncdf(onc,"TXMoth",mMothOut[[1]])
	put.var.ncdf(onc,"FLMoth",mMothOut[[2]])

	att.put.ncdf(onc,0,"Assumptions",Assump)
	att.put.ncdf(onc,0,"simData",simData)
	close.ncdf(onc)
}

toc <- round(as.double(Sys.time() - tic, units = "hours"),2)
cat("Time elapsed:",toc,"hrs","\n")
if (cfg$makeReadmeFlag){
	mat <- readLines(cfg$READMELoc)
	log <- vapply(mat,function(x) grepl("Duration",x),TRUE)
	ind <- which(log)[1]
	
	mat[ind] <- paste0(mat[ind],toc,' hrs')
	jnk <- writeLines(mat,con=cfg$READMELoc)
}

#org <-sapply(Moth,function(x)x$grid)
#org <-t(org)
#max(sapply(Cohort,function(x)x$grid[2]))
#dd <-sapply(Moth,function(x)x$GDD)
#popNum <-sapply(Moth,function(x)colSums(x$grid)[3])
#Convert to plot
#shell(paste(paste("CD",direc),paste(cfg$HyBase,paste(plt,".exe -a1",sep=""),sep="/"),sep=" && "))
	
#output plot
#shell(paste(direc,paste(plt,".ps",sep=""),sep="/"))
#CD C:/hysplit4/working
#C:/hysplit4/exec/hycs_std.exe
