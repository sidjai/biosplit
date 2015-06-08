#'Run the biosplit model for a year
#'
#'Simulates continental moth migration over the course of a year. 
#'	Requires HYSPLIT to be installed.
#'@param config The configuration object returned by loadConfig 
#'	or from loading the cfg.RData in the config.txt location
#'@import ncdf
#'@import readr
#'
#'@return Time stamp. if called for in cfg, also writes the weekly snapshots of
#'	 populations, in .txt and .nc format. 
#'	 It also outputs a final nc file after everything is completed.
#'@export
runBiosplit <- function(cfg){
	
	tic <- Sys.time()
	realWd <- system.file(package = "biosplit")
	
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
	
	#do some tests on apr to make sure it was done correctly
	#apr$CornGDD[is.na(apr$CornGDD)] <- 40000
	if(any(is.na(apr$CornGDD))){
		stop("aprioriVars messed up | there are NAs in cornGDD")
	}
	if(any(is.na(apr$Corn))){
		stop("aprioriVars messed up | there are NAs in corn")
	}
	if(any(apr$Corn<0)){
		stop("aprioriVars messed up | there are negatives in corn")
	}

	#Get Assumptions from the cfg object
	stAss <- charmatch('totFlag',names(cfg))+1
	endAss <- charmatch('relAmtFL',names(cfg))
	Assump <- vapply(stAss:endAss,function(x) paste(names(cfg[x]),cfg[x],sep=' = '),"")
	
	Assump <-paste(Assump,collapse = '; ')
	
	
	#Get from the simulation criteria as supplied in the setup.cfg file
		#Should have been changed before simulation if you want non-default
		#The options secified in the config file under simulation assumptions will change but nothing else
	

	simEmploy <- switch(cfg$simType, single=2, Fake=3, 1)

	simData <- readLines(paste(cfg$HyWorking, "setup.cfg", sep= '/'), warn=0)
	simData <- as.matrix(simData)

	# Set some data off the Control file
	
	if (simEmploy==1){
		#Multi run so set for all three control files
		controlPaths <- paste(paste(cfg$HyWorking,"CONTROL", sep='/'),1:3,sep='.')
	} else if(simEmploy==2){
		#Single run so just change the intial one
		controlPaths <- paste(cfg$HyWorking, "CONTROL", sep='/')
	}	else controlPaths <- ''
	
	depos <- vapply(controlPaths, function(x)changeControlInitial(x), "hello")
	
	simData <- rbind(simData, "Control Variables")
	
	simData <- rbind(simData,
		paste0("Vertical Motion routine = ", cfg$verticalMotionRoutine,","),
		paste0("Top of Model = ", cfg$topOfModel,","))
	simData <- rbind(simData,"Deposition = ", depos[1])
	
	simData <-paste(simData,collapse = ' ')
	simData <- gsub(",",";",simData)
	
	
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
	numFlights <- c()
	
	mMothOut <- CohortOut <- list(array(0, dim=c(dim(apr$Corn),52)),array(0, dim=c(dim(apr$Corn),52)))
	
	#Get the input cohort areas
	
	winterPop <- overWinter(cfg$FLwinterCutoff,"FL")
	numFLWin <- length(winterPop)
	winterPop <- lappend(winterPop,overWinter(cfg$TXwinterCutoff,"TX"))
	
	#check if all overwinter populations are over corn
	startCorn <- as.numeric(testEnv(winterPop,jd=45)[,"cAmt"])
	viableLoc <- which(startCorn > cfg$CornThres)
	if(max(startCorn==0) || length(viableLoc) < 1){
		stop(paste0(
			"The overwintering locations are over places without corn with the amounts being:",
			paste(startCorn,collapse="|")))
	}
	
	#Check if the overwinter population sum to the start amount
	checkAmt <- sum(makePopTable(winterPop)[,3])
	if(abs(checkAmt-cfg$stAmount) > 10){
		stop(paste0("The overwinter population calculation did not go right, calc Pop:",checkAmt, " specified amount:", cfg$stAmount))
	}
	
	midFLPop <- viableLoc[which((viableLoc - (numFLWin/2))>0)[1]]
	# get the start day from the overwinter populations
	xi <- map2block(winterPop[[midFLPop]]$grid[1],1,1)
	yi <- map2block(winterPop[[midFLPop]]$grid[2],2,1)
	startDay <- which(apr$CornGDD[xi,yi,] > cfg$infestThres)[1]
	Cohort <- winterPop
	winterPop <- NULL
	
	#iterate through the days of the year
		#start simulation
	for(di in seq(startDay,cfg$endDay)){
		
		
		tPos<-strptime(paste(di, cfg$year),"%j %Y")
		k <- 1
		
		
		#
		#add the newly emited moths to their "career paths"
		#
		
		youngAdults <- cleanAll(youngAdults, cfg$mothThres)
		
		if (length(youngAdults)>0){
			for (ya in seq(1, length(youngAdults))){
				tSplit <- willFly(youngAdults[[ya]],di,1)
				
				if (colSums(tSplit[[1]]$grid, na.rm = TRUE)[3] > cfg$mothThres){
					Moth <- lappend(Moth,tSplit[[1]])
				}
				
				sp <- cleanPop(tSplit[[2]])
				if (length(sp)>0 
						&& colSums(makePopTable(sp), na.rm = TRUE)[3] > cfg$mothThres){
					youngMig <- rbind(youngMig,tSplit[[2]])
				}
				
			}
			youngAdults=list()	
		}
		
		siz <- dim(youngMig)
		if (!is.null(siz) && length(youngMig) > 0){
			mMoth <- lappend(mMoth,combinelPop(youngMig[,1 , drop = FALSE]))
			youngMig <- youngMig[, -1, drop = FALSE]
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
		
		
		if (length(mMoth)>0){
			cat("Day", di, strftime(tPos," %m/%d/%y"),"\n")
			
			#some of the migrant moths may decide to settle down
			#so they will be subtracted from the population and added to the Moth list
			
			mi <- length(mMoth)
			while (mi > 0){
				
				tSplit <- willFly(mMoth[[mi]],di,0)
				condLowAmt <- lapply(tSplit,function(x){
					dim(x$grid)[1] == 0 ||
						colSums(x$grid)[3] < cfg$mothThres*dim(x$grid)[1]
				})
				
				condRetired <- (mMoth[[mi]]$flights >= cfg$migCareerLimit)
				
				#What to do with the Local population?
				if (!condLowAmt$stay || condRetired){
					Moth<-lappend(Moth,
												if(condRetired){
													mMoth[[mi]]
												} else { 
													tSplit[[1]]
												})
				}
				
				#What to do with the Migrants?
				if (condLowAmt$migrant || condRetired) mMoth <- mMoth[-mi,drop = FALSE]
				else{
					#Got Moths that want to fly but are they able to fly?
					xi <- map2block(tSplit[[2]]$grid[1,1],1,1)
					yi <- map2block(tSplit[[2]]$grid[1,2],2,1)
					
					condSk <- (length(which(cfg$skip==di))!=0)
					condTired <- (mMoth[[mi]]$flights %% cfg$succFlightLim)==0
					if (any(condSk,
									condTired,
									apr$windStopTO[xi,yi,di],
									apr$precStopTO[xi,yi,di],
									apr$tempStopTO[xi,yi,di],
									na.rm = TRUE)){
						mMoth[[mi]] <- tSplit[[2]]
						mMoth[[mi]]$flights <- mMoth[[mi]]$flights+.1
						
					} else{
						
						#Got Moths that can fly
						
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
						
						#get rid of the Moths that went out of bounds
						mMoth[[mi]] <- migrateDeath(mMoth[[mi]],di)
						if (dim(mMoth[[mi]]$grid)[1]==0)  mMoth <- mMoth[-mi,drop = FALSE]
						else {
							mMoth[[mi]]$flights <- round(mMoth[[mi]]$flights + 1,0)
							
						}
					}
				}
				mi <- (mi - 1)
				
			}
		}
		
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
		#do end of week calculations
		###########################
		if (di%%7==1 & di<=359){
			#End of week
			
			
			Eggs <- combineEggs(Eggs)
			
			youngMig <- list()
			if (di < 130){
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
			
			if (length(Cohort)>0) CohortOut <- makeOutput(Cohort,CohortOut)
			if (length(mMoth)>0){
				cat("End week", wk,"\n")
				mMothOut <- makeOutput(mMoth,mMothOut,Moth)
				wk<-wk+1
			}
			
			##
			#do cohort biological
			##
			
			#Eggs
			#combine eggs
			if (length(Eggs)>0){
				Cohort <- lappend(Cohort,Eggs)
			}
			Eggs <- list()
			youngAdults <- list()
			
			if (length(Cohort)>0){
				#Clean cohort
				tCoh <- growCohort(Cohort,di)
				
				#Clean cohort
				Cohort <- cleanAll(tCoh[[1]],cfg$cohortThres)
				
				youngAdults <- tCoh[[2]]
			}
			
		}
		
	}
	
	#Clean up + final output
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
	jnk <- writeLines(
		paste(numFlights,collapse='\n'),
		con=paste(cfg$SimOutFold,"numFlights.txt",sep='/'))
	toc	
	
}

changeControlInitial <- function(path){
	newCon <- befCon <- readLines(path)
	
	indVert <- charmatch("C:/",befCon)-3
	newCon[indVert] <- cfg$verticalMotionRoutine
	
	#top of the model in two places
	endLevel <- charmatch("cdump",newCon)+2
	newCon[indVert+1] <- cfg$topOfModel
	newCon[endLevel] <- cfg$topOfModel
	
	#Done so write
	writeLines(newCon,path)
	depo <- newCon[(endLevel+5):length(newCon)]
	
	return(paste(depo ,collapse = '|'))
}
makeLife <- function(flagMoth,loc,GDD,origin){
	
	life <- list()
	if (length(dim(loc))<2){
		loc <- t(as.matrix(loc))
	}
	colnames(loc) <- NULL
	rownames(loc) <- NULL
	
	life$grid <- loc
	life$origin <- origin
	
	if (flagMoth) {
		life$daysOld <- GDD
		life$numEggs <- cfg$capEggs
		life$flights <- 0
		
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
	tol <-.29
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

testEnv <- function(lpop,w=-9999,jd=di){
	
	#profName <- 
	#Rprof("C:/Users/Siddarta.Jairam/Documents/iterateProf3.out",memory.profiling = TRUE)
	#Rprof("")
	#summaryRprof("C:/Users/Siddarta.Jairam/Documents/iterateProf3.out")
	
	if (w <0){
		lpop <- deconst(lpop)
	} else {
		lpop <- list(lpop[[w]])
	}
	popType <- switch(names(lpop[[1]])[3],daysOld=1,GDD=0)
	tab <- makePopTable(lpop,verboseNames=1)
	
	xbs <- map2block(as.numeric(tab[,1]),1,1)
	ybs <- map2block(as.numeric(tab[,2]),2,1)
	ind  <- cbind(xbs,ybs)
	cAmt <- apr$Corn[ind]
	cGDD <- round(apr$CornGDD[cbind(ind,jd)]/10,2)
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

getNightDur <- function(lat, lon, day){
	if (requireNamespace("insol", quietly = TRUE)) {
		sunLight <- insol::daylength(lat,lon,day,1)[,3]
	} else {
		#from: http://mathforum.org/library/drmath/view/56478.html
		part = asin(.39795*cos(.2163108 + 2*atan(.9671396*tan(.00860(day-186)))))
		sunLight = 24 - (24/pi)*acos((sin(0.8333*pi/180) + sin(lat*pi/180)*sin(part))
																 /(cos(lat*pi/180)*cos(part)))
	}
	names(sunLight) <- NULL
	if(is.nan(sunLight[1])) stop(paste0("Astro calc messed up royally with NaNs used vals:",paste(avgLat,avgLon,di)))
	if(sunLight[1]<0 || sunLight[1]>24) stop(paste0("Astro calc came up with weird response of:",sunLight))
	return(round((24-sunLight)-1,0))
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
	
	#Control file

	befCon <- read_lines(paste(cfg$HyWorking,paste0("CONTROL.",toString(PID)),sep="/"))
	indMon <- charmatch("C:/",befCon)+1
	newCon <- befCon

	#Change the values that only change when the date changes
	if (dateChangeFlag==1){
		#change date at top
		newCon[1]<-strftime(date,"%y %m %d 00")

		
		
		
		#change month if it needs it
		newMon<- tolower(strftime(date,"%b"))
		if (!grepl(newMon,befCon[indMon])){
			#Change the month
			newCon[indMon] <- paste(cfg$metDataType, ".",newMon,cfg$year-2000,sep="")
		}

		#Change the year every time the date changes because it is easier to code
		newCon[indMon-1] <- paste0(cfg$MetARLFold,"/")
	}

	#change the vales that are always gonna be different
	#time of flight
	
	endTimeInd <- charmatch("cdump",newCon)+4
	
	if(cfg$delNightDurFlag){
		avgLon <- mean(item[,1])
		avgLat <- mean(item[,2])
		flightTime <- getNightDur(avgLat,avgLon,as.numeric(strftime(date,"%j")))
		
		newCon[indMon-5] <- paste(flightTime)
		
		newCon[endTimeInd] <- strftime(date,paste("%y %m %d", flightTime, "00"))
		newCon[endTimeInd+1] <- paste("01", flightTime, "00")
																	 
	} else {
		newCon[indMon-5] <- "12"
		newCon[endTimeInd] <- strftime(date,"%y %m %d 12 00")
		newCon[endTimeInd+1] <- "01 12 00"
	}
	
	#number of locations
	newCon[2] <- newSrc
	befSrc <- as.numeric(befCon[2])
	diff <- befSrc-newSrc
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
		newCon[2+sr] <- paste(ymap[[sr]],xmap[[sr]],"500.0")
	}



	

	#finished changing things
	writeLines(newCon,paste(cfg$HyWorking,paste0("CONTROL.",toString(PID)),sep="/")) 
}

callHysplit <- function(hold,PID){
	
	junk <- tryCatch({
		 shell(paste(paste("CD",cfg$HyWorking),paste(paste(cfg$HyBase,cfg$HyConc,sep="/"),toString(PID)),sep=" && "),
			intern=hold,wait=hold)
		 },
		error = function(cond){
			Sys.sleep(3)
			callHysplit(hold,PID)
		},
		warning = function(cond){
			if (grepl("900",cond)){
				
				stop(paste("run",PID,"ERR: starting location in interpolation area | trying to call an empty level\n Using pop",mi,'\n',cond))
			} else {
				Sys.sleep(3)
				callHysplit(hold,PID)
				#stop(paste("run",PID,"too little computing space, supply more or limit model\n Using pop",mi,'\n',cond)))
			}
		}
	)
	
	
}

runHysplit <- function(cutoff=.01, plotFlag=0, hold = TRUE, call=TRUE,PID=1){

	#change directory, then call hysplit
	if(call) jk <- callHysplit(hold,PID)
	cdump <- paste0("cdump",PID)
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

	numCalls <- 1
	repeat{ 
		textFile<-shell(paste(paste("CD",cfg$HyWorking),
			paste(cfg$HyBase,
				paste("con2asc.exe",cdump, "-m -d")
				,sep="/")
			,sep=" && ")
			,intern=TRUE)
		if (length(textFile)<1){
			if(numCalls>10) {
				stop(paste("run", PID, "did not out into cdump properly (con2asc failed) check message and config"))
			} else {
				numCalls <- numCalls+1
				callHysplit(hold=TRUE,PID)
			}
		} else break
	}
		

	datum <- read.csv(paste(cfg$HyWorking,gsub("^\\s+|\\s+$", "", textFile[1]),sep="/"),header = FALSE)
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
	#suppress the multi hysplit output going to console when using Rgui
	sink("NUL")
	for (gg in seq(1,length(inPop))){
		changeInput(dateChange,date,inPop[[gg]],PID=gg)
		callHysplit(hold=holdvec[gg],PID=gg)	
	}
	prc <- shell("tasklist",intern=TRUE)
	while(length(which(grepl("hycs",prc)))>0){
		prc <- shell("tasklist",intern=TRUE)
	}
	sink()
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
	avgLon <- mean(tgrd[,1])
	avgLat <- mean(tgrd[,2])
	sunLight <- daylength(avgLat,avgLon,di,1)[3]
	if(is.nan(sunLight)) stop(paste0("Astro calc messed up royally with NaNs used vals:",paste(avgLat,avgLon,di)))
	if(sunLight<0 || sunLight>24) stop(paste0("Astro calc came up with weird response of:",sunLight))
	mag <- ((24-round(sunLight,1))-1)*(5/12)
	for (gj in seq(1,dim(tgrd)[1])){
		xrandOffset <- runif(1,-mag,mag)
		yrandOffset <- runif(1,0,mag)
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
	cGDD <- apr$CornGDD[inds]/10
	
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
	if(max(deathTest)){
		numFlights <<- c(numFlights,pop$flights)
	}
	pop$grid[deathTest,3] <- (-9999)
	
	
	#Lay Eggs?
	if (pop$daysOld>cfg$oviDay 
			&& pop$numEggs > cfg$eggsPerInfest){
		#row specific checks
		orind <- cbind(xs,ys,rep.int(day,grdLen))
		grCorn <- apr$CornGDD[orind]/10
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
	if (length(lpop)<1) return(lpop)
	
	tab <- makePopTable(lpop)
	xs <- map2block(tab[,1],1,1)
	ys <- map2block(tab[,2],2,1)
	org <- ifelse(tab[,5],"FL","TX")
	
	#growth
	aDD <- tab[,4]
	for (t in seq(di,di+6)){
		aDD <- aDD + apr$FawGDD[cbind(xs,ys,t)]/10
	}
	
	#tests for death and adulthood
	if (di<cfg$altCornDay) {deadTest <- (apr$Corn[cbind(xs,ys)] < 1)
	}	else {
		deadTest <- (apr$CornGDD[cbind(xs,ys,di)]/10 < cfg$infestThres)
	}
	
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
	grCornGDD <- apr$CornGDD[ind]/10
	
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
		},c(1,1,1,1,1), USE.NAMES=FALSE))
	
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
  		x$flights)
  	},c(1,1,1),USE.NAMES=FALSE))
  
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

overWinter <- function(region,origin){
	if (is.double(region)){
		#latitude thres
		possYs <- which(ymapvec < region)
		possXs <- switch(origin,
										 TX=which(xmapvec < (-90)),
										 FL=which(xmapvec > (-90)))
		locInd <- which(apr$Corn[possXs,possYs] > 1,arr.ind=TRUE)
		yi <- possYs[locInd[,2]]
		xi <- possXs[locInd[,1]]
		xs <- map2block(xi,1,2)
		ys <- map2block(yi,2,2)
		locNum <- dim(locInd)[1]
		numPerLoc <- round((cfg$stAmount * switch(origin,
												TX=(1-cfg$relAmtFL),
												FL=cfg$relAmtFL))/locNum,1)
		
		lpop <- lapply(1:locNum,function(ind){
				makeLife(0,cbind(xs[ind],ys[ind],numPerLoc),0,origin)
		})
	}
	return(lpop)
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
