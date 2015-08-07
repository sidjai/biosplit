#'Run the biosplit model for a year
#'
#'Simulates continental moth migration over the course of a year. 
#'	Requires HYSPLIT to be installed.
#'@param cfg The configuration object returned by loadConfig 
#'	or from loading the cfg.RData in the config.txt location
#'@import ncdf
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
	
	apr <- lapply(varNames, function(x) get.var.ncdf(nc,x))
	names(apr) <- varNames
	close.ncdf(nc)
	xmapvec <- nc$dim$lon$vals
	ymapvec <- nc$dim$lat$vals
	
	bndx <- c(min(xmapvec),max(xmapvec))
	bndy <- c(min(ymapvec),max(ymapvec))
	
	#do some tests on apr to make sure it was done correctly
	
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
	
	Assump <- paste(Assump, collapse = '; ')
	
	
	#Get from the simulation criteria as supplied in the setup.cfg file
		#Should have been changed earlier for non-default HYSPLIT simulations
		#Control file will change during the run, but setup is constant
	

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
	
	depos <- vapply(controlPaths, function(x){
		changeControlInitial(x, cfg$MetARLFold, cfg$verticalMotionRoutine, cfg$topOfModel)
		}, "e")
	
	simData <- rbind(simData, "Control Variables")
	
	simData <- rbind(simData,
		paste0("Vertical Motion routine = ", cfg$verticalMotionRoutine,","),
		paste0("Top of Model = ", cfg$topOfModel,","))
	simData <- rbind(simData,"Deposition = ", depos[1])
	
	simData <-paste(simData,collapse = ' ')
	simData <- gsub(",",";",simData)
	
	
	makeREADME(cfg$READMELoc,
             cfg$SimOutFold,
             cfg$runName,
             cfg$codeChanges,
             Assump,
             simData,
             cfg$makeReadmeFlag)
	
	#Make closures
	map2block <- makeMapConverter(xmapvec, ymapvec, tol = .29)
	
	changeInput <- makeHysplitInputChanger(cfg$HyWorking, apr$Corn, 
																				 cfg$delNightDurFlag,
																				 map2block)
	callHysplit <- makeHysplitCaller(cfg$HyWorking,
																	 paste(cfg$HyBase,cfg$HyConc,sep='/'))
	manHysplit <- makeHysplitRunner(cfg$HyWorking, 
																	paste(cfg$HyBase, cfg$HyPlt, sep = '/'),
																	cfg$rawHyPlotFold,
																	callHysplit,
																	cfg$plotWriteFlag)
	
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
	
	mMothOut <- CohortOut <- list(
		array(0, dim=c(dim(apr$Corn),52)),
		array(0, dim=c(dim(apr$Corn),52)))
	
	#Get the input cohort areas
	
	winterPop <- overWinter(cfg$FLwinterCutoff, "FL",
													map2block, apr$Corn, cfg$stAmount, cfg$relAmtFL, cfg$capEggs)
	numFLWin <- length(winterPop)
	winterPop <- lappend(winterPop,
											 overWinter(cfg$TXwinterCutoff,"TX",
											 					 map2block, apr$Corn, cfg$stAmount, cfg$relAmtFL))
	
	#check if all overwinter populations are over corn
	startCorn <- vapply(winterPop, function(pop){
		getPopContext(pop, map2block, apr$Corn)$Corn
	}, 1)
	viableLoc <- which(startCorn > cfg$CornThres)
	if(max(startCorn==0) || length(viableLoc) < 1){
		stop(paste0(
			"The overwintering locations are over places without corn with the amounts being:",
			paste(startCorn,collapse="|")))
	}
	
	#Check if the overwinter population sum to the start amount
	checkAmt <- sum(makePopTable(winterPop)[,3])
	if(abs(checkAmt-cfg$stAmount) > 10){
		stop(paste0("The overwinter population calculation did not go right,",
			" calc Pop:", checkAmt, 
			" specified amount:", cfg$stAmount))
	}
	
	midFLPop <- viableLoc[which((viableLoc - (numFLWin/2))>0)[1]]
	# get the start day from the overwinter populations
	xi <- map2block(winterPop[[midFLPop]]$grid[1], 1)
	yi <- map2block(winterPop[[midFLPop]]$grid[2], 2)
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
				ctxya <- getPopContext(youngAdults[[ya]], map2block,
					apr$CornGDD[, , di],
					cfg$infestLmt,
					cfg$infestThres,
					cfg$flightPropBeforeSilk,
					cfg$flightPropAfterSilk)
				
				tSplit <- willFly(youngAdults[[ya]], 1 , ctxya)
				
				if (colSums(tSplit[[1]]$grid, na.rm = TRUE)[3] > cfg$mothThres){
					Moth <- lappend(Moth, tSplit[[1]])
				}
				
				sp <- cleanPop(tSplit[[2]])
				if (length(sp)>0 
						&& colSums(makePopTable(sp), na.rm = TRUE)[3] > cfg$mothThres){
					youngMig <- rbind(youngMig, tSplit[[2]])
				}

			}
			youngAdults <- list()	
		}
		
		siz <- dim(youngMig)
		if (!is.null(siz) && length(youngMig) > 0){
			mMoth <- lappend(mMoth, combinelPop(youngMig[,1 , drop = FALSE], map2block))
			youngMig <- youngMig[, -1, drop = FALSE]
		}
		
		#Moth algebra
		tpop <- list()
		tpop <- cleangrowMoths(Moth, Eggs, di, cfg, apr, map2block)
		Moth <- tpop[[1]]
		Eggs <- tpop[[2]]
		
		tpop <- list()
		tpop <- cleangrowMoths(mMoth, Eggs, di, cfg, apr, map2block)
		mMoth <- tpop[[1]]
		Eggs <- tpop[[2]]
		
		
		if (length(mMoth)>0){
			cat("Day", di, strftime(tPos," %m/%d/%y"),"\n")
			
			#some of the migrant moths may decide to settle down
			#so they will be subtracted from the population and added to the Moth list
			
			mi <- length(mMoth)
			while (mi > 0){
				ctx <- getPopContext(mMoth[[mi]], map2block,
					apr$Corn,
					apr$CornGDD[, , di],
					cfg$infestLmt,
					cfg$infestThres,
					cfg$flightPropBeforeSilk,
					cfg$flightPropAfterSilk)
				
				tSplit <- willFly(mMoth[[mi]], 0, ctx)
				condLowAmt <- lapply(tSplit,function(x){
					dim(x$grid)[1] == 0 ||
					colSums(x$grid)[3] < cfg$mothThres * dim(x$grid)[1]
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
					testPos <- cbind(ctx$xs, ctx$ys, di)
					condSk <- (any(cfg$skip == di))
					condTired <- (mMoth[[mi]]$flights %% cfg$succFlightLim) == 0
					condTakeOffHalt <- any(
						apr$windStopTO[testPos],
						apr$precStopTO[testPos],
						apr$tempStopTO[testPos], na.rm = TRUE)
					
					if (condSk || condTired || condTakeOffHalt){
						
						mMoth[[mi]] <- tSplit[[2]]
						mMoth[[mi]]$flights <- mMoth[[mi]]$flights+.1
						
					} else{
						
						#Got Moths that can fly
						
						#####################################################
						#Migrate the species
						#####################################################
						shouldPlot <-ifelse((di >=cfg$invPlotFlag && mig%%10==0),1,0)
						if (simEmploy == 1){
							mMoth[[mi]]$grid <- multiHysplit(cfg$HyWorking, tSplit[[2]], tPos, shouldPlot, 
																							 changeInput, callHysplit, manHysplit)
						} else if (simEmploy == 2){
							mMoth[[mi]]$grid <- manHysplit(tPos, shouldPlot)
						} else {
							mMoth[[mi]]$grid <- testFakeHysplit(tSplit[[2]]$grid, di)
						}
						
						mig <- mig+1
						
						ctx <- getPopContext(mMoth[[mi]], map2block,
							apr$Corn,
							apr$CornGDD[, , di],
							cfg$altCornDay,
							cfg$infestThres)

						#get rid of the Moths that went out of bounds
						mMoth[[mi]] <- migrateDeath(mMoth[[mi]], di, ctx)
						if (dim(mMoth[[mi]]$grid)[1]==0)  mMoth <- mMoth[-mi, drop = FALSE]
						else {
							mMoth[[mi]]$flights <- round(mMoth[[mi]]$flights + 1, 0)
						}
					}
				}
				mi <- (mi - 1)
				
			}
		}
		
		mMoth <- cleanAll(mMoth,cfg$mothThres)
		Moth <- cleanAll(Moth,cfg$mothThres)
		
		mMoth <- combinelPop(mMoth, map2block)
		Moth  <- combinelPop(Moth, map2block)
		
		#Test to see if it needs to output every day
		txtFlag <- (di==sort(c(cfg$outEveryDayStart,cfg$outEveryDayEnd,di))[2])
		if(txtFlag){
			if(length(mMoth)>0) try(
				mMothOut <- makeOutput(mMoth, mMothOut, tPos, map2block,
					cfg$SimOutFold,
					lpop2 = Moth,
					onlyTxt = FALSE,
					shWrite = cfg$writeFlag))
		}
		
		###########################
		#do end of week calculations
		###########################
		if (di%%7==1 && di<=359){
			#End of week
			
			
			Eggs <- combineEggs(Eggs, map2block)
			
			youngMig <- list()
			if (di < 130){
				fldie <- c("TX",
					vapply(Cohort,function(x)x$origin,""),
					vapply(Moth,function(x)x$origin,""),
					vapply(mMoth,function(x)x$origin,""),
					vapply(Eggs,function(x)x$origin,"")
				)
				
				if(!any(fldie == "FL")){
					stop(paste("Day:", di, "FL died off, abandon ship"))
				}
			}
			
			#get all the outputs
			
			if (length(Cohort)>0){
				CohortOut <- makeOutput(Cohort, CohortOut, tPos, map2block, 
					cfg$SimOutFold, shWrite = cfg$writeFlag)
			}

			if (length(mMoth)>0){
				cat("End week", wk, "\n")
				mMothOut <- makeOutput(mMoth,mMothOut, tPos, map2block,
					cfg$SimOutFold,
					lpop2 = Moth,
					shWrite = cfg$writeFlag)
				wk <- wk + 1
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
				tCoh <- growCohort(Cohort, map2block, di, apr, 
					cfg$altCornDay, cfg$infestThres, cfg$cohortGDDThres, cfg$capEggs)
				
				#Clean cohort
				Cohort <- cleanAll(tCoh[[1]],cfg$cohortThres)
				
				youngAdults <- tCoh[[2]]
			}
			
		}
		
	}
	
	#Clean up + final output
	if (cfg$writeFlag){
		dims <-list(
			nc$dim$lon,
			nc$dim$lat,
			dim.def.ncdf( "Time", "weeks", 1:52, unlim=TRUE ))
		
		fVars<- list(
			var.def.ncdf('TXCohort', '#Immature moths',dims,1.e30),
			var.def.ncdf('FLCohort', '#Immature moths',dims,1.e30),
			var.def.ncdf('TXMoth', '#Moths',dims,1.e30),
			var.def.ncdf('FLMoth', '#Moths',dims,1.e30))
		onc <- create.ncdf(paste(cfg$SimOutFold,"Final.nc",sep="/"), fVars)
		
		put.var.ncdf(onc,"TXCohort",CohortOut[[1]])
		put.var.ncdf(onc,"FLCohort",CohortOut[[2]])
		put.var.ncdf(onc,"TXMoth",mMothOut[[1]])
		put.var.ncdf(onc,"FLMoth",mMothOut[[2]])
		
		att.put.ncdf(onc,0,"Assumptions",Assump)
		att.put.ncdf(onc,0,"simData",simData)
		close.ncdf(onc)
	}
	
	toc <- round(as.double(Sys.time() - tic, units = "hours"), 2)
	cat("Time elapsed:", toc, "hrs", "\n")
	if (cfg$makeReadmeFlag){
		mat <- readLines(cfg$READMELoc)
		log <- vapply(mat, function(x){ grepl("Duration", x) }, TRUE)
		ind <- which(log)[1]
		
		mat[ind] <- paste0(mat[ind], toc, ' hrs')
		jnk <- writeLines(mat, con = cfg$READMELoc)
	}
	
	return(toc)
}

changeControlInitial <- function(path, ARLFold, vertMotion, topModel){
	newCon <- befCon <- readLines(path)
	
	indVert <- charmatch("C:/",befCon) - 3
	newCon[indVert] <- vertMotion
	
	#top of the model in two places
	endLevel <- charmatch("cdump",newCon) + 2
	newCon[indVert + 1] <- topModel
	newCon[endLevel] <- topModel
	
	newCon[indVert + 3] <- paste0(ARLFold,'/')
	
	#Done so write
	writeLines(newCon,path)
	depo <- newCon[(endLevel + 5):length(newCon)]
	
	return(paste(depo ,collapse = '|'))
}

makeLife <- function(flagMoth,loc,GDD,origin,capEggs){
	
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
		life$numEggs <- capEggs
		life$flights <- 0
		
	} else life$GDD <- GDD
	
	return(life)
	
}

makeREADME <- function(readmeLoc,
                       rundirec,
                       runName,
                       codeChanges,
                       Assump,
                       simData,
                       masFlag = TRUE){
	
	indvReadme <- rbind(
		runName,
		'\tDescription:',
		paste('\tRun Date:',strftime(Sys.time())),
		'\tRun Duration:',
		paste('\tCode Changes:',codeChanges),
		'\tResult notes:',
		paste('\tAssumptions:',Assump),
		paste('\tSimData:',simData),
		'#############################################')
	rownames(indvReadme) <- NULL
	if (masFlag){
		if ( file.exists(readmeLoc)){
			bef <- readLines(readmeLoc)
			strun <- grep(runName,bef)
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

makeMapConverter <- function(xaxis, yaxis, tol){
	realEnv <- new.env()
	realEnv$xaxis <- xaxis
	realEnv$yaxis <- yaxis
	realEnv$tol <- tol
	
	f <- function(vin, axisNum, direction = TRUE){
		box <- cbind(vin-tol,vin+tol)
		if (axisNum == 1){
			mapAxis <- xaxis
		} else if (axisNum == 2) {
			mapAxis <- yaxis
		} else {
			stop(sprintf("The supplied axis number: %f,
									 is not 1 or 2, if a 3d map is needed, have to change code",
									 axisNum))
		}
		
		if (direction){
			#go from map units to a block number
			val <- vapply(1:length(vin),function(x){
				which((mapAxis>box[x,1]) & mapAxis<max(box[x,2]))[1]
			},1)
			
		} else {
			# go from block number to map units
			val <- round(mapAxis[vin], 3)
			
		}
		return(val)
	}
	environment(f) <- realEnv
	return(f)
}

testEnv <- function(lpop,w=-9999,jd, map2block, cornMap, cGDDmap, infestThres){
	
	#profName <- 
	#Rprof("C:/Users/Siddarta.Jairam/Documents/iterateProf3.out",memory.profiling = TRUE)
	#Rprof("")
	#summaryRprof("C:/Users/Siddarta.Jairam/Documents/iterateProf3.out")
	
	if (w <0){
		lpop <- deconst(lpop)
	} else {
		lpop <- list(lpop[[w]])
	}
	popType <- switch(names(lpop[[1]])[3], daysOld=1, GDD=0)
	tab <- makePopTable(lpop,verboseNames=1)
	
	xbs <- map2block(as.numeric(tab[,1]), 1)
	ybs <- map2block(as.numeric(tab[,2]), 2)
	ind  <- cbind(xbs,ybs)
	cAmt <- cornMap[ind]
	cGDD <- round(cGDDmap[cbind(ind,jd)]/10,2)
	Livability <- howLivable(cGDD, infestThres)
	#Livability <- vapply(cGDD,function(x) howLivable(x),1)
	if(popType){
		eg <- as.matrix(lapply(lpop,function(x) x$numEggs))
		tab <- cbind(tab,xbs,ybs,cAmt,cGDD,Livability,eg)
	} else tab <- cbind(tab,xbs,ybs,cAmt,cGDD,Livability)
	return(tab)
			
}

findOnMap <- function(map,xl,xt,yl,yt,map2block,num=0){
	xbl <- map2block(xl, 1)
	xbt <- map2block(xt, 1)
	ybl <- map2block(yl, 2)
	ybt <- map2block(yt, 2)
	
	slice <- which(map[xbl:xbt,ybt:ybl]>num,arr.ind=TRUE)
	slice[,1]<- map2block(slice[,1]+xbl, 1, FALSE)
	slice[,2]<- map2block(slice[,2]+ybt, 2, FALSE)
	return(slice)
}

getxy <- function(bx, by, map2block, dir=FALSE){
	return(c(map2block(bx,1,dir), map2block(by,2,dir)))
}

howLivable <- function(cGrowth, infestThres){
	val <- ifelse(cGrowth < infestThres,0,
		ifelse(cGrowth<1000,.00075*cGrowth+.15,
		ifelse(cGrowth<1400,.9, 
		ifelse(cGrowth<2400,1.8-.00075*cGrowth,0))))
	return(val)
}

genFlightProp <- function(cGrowth,infestLmt, beforeSilk, afterSilk){
	val <- ifelse(cGrowth == 0, 1,
		ifelse(cGrowth < 1400, beforeSilk,
		ifelse(cGrowth < infestLmt, afterSilk, 1)))
	return(val)
}

getNightDur <- function(lat, lon, day){
	if (requireNamespace("insol", quietly = TRUE)) {
		sunLight <- insol::daylength(lat,lon,day,1)[,3]
	} else {
		#from: http://mathforum.org/library/drmath/view/56478.html
		part <- asin(.39795*cos(.2163108 + 2*atan(.9671396*tan(.00860*(day-186)))))
		sunLight <- 24 - (24/pi)*
			acos((sin(0.8333*pi/180) + sin(lat*pi/180)*sin(part))
				/(cos(lat*pi/180)*cos(part)))
	}
	names(sunLight) <- NULL
	return(round((24-sunLight)-1,0))
}
makeHysplitInputChanger <- function(hyDir, cornMap, delNightDurFlag, map2block){
	realEnv <- new.env()
	realEnv$hyDir <- hyDir
	realEnv$cornMap <- cornMap
	realEnv$delNightDurFlag <- delNightDurFlag
	realEnv$map2block <- map2block
	f <- function(pop, date, PID=0){

		#get the place and amount of the population
		item <- pop$grid
		newSrc <- dim(item)[1]
		#write the emit file 
		newEmit<- c("YYYY MM DD HH    DURATION(hhhh) #RECORDS",
			"YYYY MM DD HH MM DURATION(hhmm) LAT LON HGT(m) RATE(/h) AREA(m2) HEAT(w)")
		base <- paste(strftime(date,"%Y %m %d"),"00")
		
		newEmit[3] <- paste(base,"0001",newSrc)
		
	
		posDes <- paste(round(item[, 2],3), round(item[, 1],3))
		
		xbs <- map2block(item[, 1], 1)
		ybs <- map2block(item[, 2], 2)
		cornAmt <- cornMap[cbind(xbs,ybs)]*10000
		itDes <- paste(base, "00 0100", posDes, "500.0", item[, 3], cornAmt, "0.0")
		newEmit <- c(newEmit, itDes)
		
		#done with emit file
		writeLines(newEmit,
			paste(hyDir, paste0("EMITIMES", PID), sep='/'))
		
		#Control file
	
		befCon <- readr::read_lines(paste(hyDir, paste0("CONTROL.", PID), sep='/'))
		indMon <- charmatch("C:/", befCon)+1
		newCon <- befCon
		
		todayStr <- strftime(date,"%y %m %d 00")
		dateChangeFlag <- is.na(charmatch(newCon[1], todayStr))
	
		#Change the values that only change when the date changes
		if (dateChangeFlag){
			#change date at top
			newCon[1] <- todayStr	
			
			#change month if it needs it
			newMon<- tolower(strftime(date, "%b"))
			if (!grepl(newMon, befCon[indMon])){
				#Change the month
				strVec <- strsplit(befCon[indMon], "[.]")[[1]]
				strVec[2] <- paste0(newMon, strftime(date, "%y"))
				
				newCon[indMon] <- paste(strVec, collapse = '.')
			}
	
		}
	
		#change the vales that are always gonna be different
		#time of flight
		
		endTimeInd <- charmatch("cdump", newCon)+4
		
		if(delNightDurFlag){
			avgLon <- mean(item[, 1])
			avgLat <- mean(item[, 2])
			flightTime <- getNightDur(avgLat, avgLon, as.numeric(strftime(date, "%j")))
			
			newCon[indMon-5] <- paste(flightTime)
			
			newCon[endTimeInd] <- strftime(date, paste("%y %m %d", flightTime, "00"))
			newCon[endTimeInd+1] <- paste("01", flightTime, "00")
			
		} else {
			newCon[indMon-5] <- "12"
			newCon[endTimeInd] <- strftime(date, "%y %m %d 12 00")
			newCon[endTimeInd+1] <- "01 12 00"
		}
		
		#number of locations
		newCon[2] <- newSrc
		befSrc <- as.numeric(befCon[2])
		diff <- befSrc-newSrc
		if (diff<0){
			#insert new places
			for (er in seq(1,-diff)){
				newCon <- append(newCon, "placeholder",2)
			}
			
		} else if (diff>0){
			#Delete lines
			for (er in seq(1, diff)){
				newCon <- newCon[-3]
			}
		}
		
		#now that the places are right, just assign the sources
		for (sr in seq(1,newSrc)){
			newCon[2+sr] <- paste(posDes[sr], "500.0")
		}
	
		#finished control 
		writeLines(newCon, paste(hyDir ,paste0("CONTROL.", PID), sep='/'))
	}
	environment(f) <- realEnv
	return(f)
}

makeHysplitCaller <- function(hyDir, hyExePath){
	realEnv <- new.env()
	realEnv$xaxis <- hyDir
	realEnv$xaxis <- hyExePath
	
	callHy <- function(hold,PID){
		
		junk <- tryCatch({
			 shell(paste(paste("CD", hyDir), paste(hyExePath, PID), sep=" && "),
				intern=hold, wait=hold)
			 },
			error = function(cond){
				Sys.sleep(3)
				callHy(hold,PID)
			},
			warning = function(cond){
				if (grepl("900",cond)){
					
					stop(sprintf("ERR: Hysplit specific error: could be starting location
											 in interpolation area - trying to call an empty level\n
											 Look at MESSAGE.%f \n %s", PID, cond))
				} else {
					Sys.sleep(3)
					callHy(hold,PID)
					#stop(paste("run", PID, "too little computing space,",
					#"supply more or limit model\n Using pop", mi, '\n', cond)))
				}
			}
		)
	}
	realEnv$callHy <- callHy
	environment(callHy) <- realEnv
	return(callHy)
	
	
}

makeHysplitRunner <- function(hyDir, hyPlotExe, rawPlotOutDir,
															callHysplit,
															shPlotWrite = TRUE, cutoff = 0.1){
	realEnv <- new.env()
	realEnv$hyDir <- hyDir
	realEnv$hyPlotExe <- hyPlotExe
	realEnv$rawPlotOutDir <- rawPlotOutDir
	realEnv$callHysplit <- callHysplit
	realEnv$shPlotWrite <- shPlotWrite
	realEnv$cutoff <- cutoff
	
	hyAscExe <- paste0(substr(hyPlotExe, 1,
														rev(gregexpr('/', hyPlotExe)[[1]])[1]),
										 "con2asc.exe")
	realEnv$hyAscExe <- hyAscExe
	f <- function(date, plotFlag=FALSE, hold = TRUE, call=TRUE, PID=1){
	
		#change directory, then call hysplit
		if(call) jk <- callHysplit(hold, PID)
		cdump <- paste0("cdump", PID)
		if (plotFlag == 1){
			#Convert to plot
			sadg <- shell(paste(paste("CD", hyDir),
													paste(hyPlotExe, cdump, "-k0"),
													sep=" && "),
										intern=TRUE)
			
			if (shPlotWrite){
				
				endTok <- strftime(date,"%m%d%y")
				lst <- list.files(rawPlotOutDir, endTok)
				file.rename(paste(hyDir, "concplot.ps", sep='/'),
						paste0(rawPlotOutDir, '/', endTok, zstr(length(lst)), ".ps"))
			} else {
	
				#display plot
				shell(paste(hyDir, "concplot.ps", sep='/'))
			}
		}
	
	
		#make into a ascii
	
		numCalls <- 1
		repeat{ 
			textFile<-shell(paste(paste("CD", hyDir),
														paste(hyAscExe, cdump, "-m -d"),
														sep=" && "),
											intern=TRUE)
				
			if (length(textFile)<1){
				if(numCalls>10) {
					stop(paste("run", PID, "did not out into cdump properly (con2asc failed) check message and config"))
				} else {
					numCalls <- numCalls+1
					callHysplit(hold=TRUE,PID)
				}
			} else break
		}
			
	
		datum <- read.csv(paste(hyDir, gsub("^\\s+|\\s+$", "", textFile[1]),sep='/'),
											header = FALSE)
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
	environment(f) <- realEnv
	return(f)
}

round2number <- function(val,num){
	val <- val - val%%(num)
	return(val)
}

multiHysplit <- function(hyDir, pop, date, shPlotFlag,
                         changeInput, callHysplit, manHysplit){
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
		changeInput(inPop[[gg]], date, PID=gg)
		callHysplit(hold=holdvec[gg],PID=gg)	
	}
	prc <- shell("tasklist",intern=TRUE)
	while(length(which(grepl("hycs",prc)))>0){
		prc <- shell("tasklist",intern=TRUE)
	}
	sink()
	for (gg in seq(1,length(inPop))){
		out <- rbind(out,
								 manHysplit(date, shPlotFlag, hold=holdvec[gg], call=FALSE, PID=gg)
								 )
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

testFakeHysplit <- function(tgrd, jd){
	ogrd <- tgrd
	avgLon <- mean(tgrd[,1])
	avgLat <- mean(tgrd[,2])
	mag <- getNightDur(avgLat,avgLon,jd) * (5/12)
	
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

getPopContext <- function(pop, map2block, ...){
	if(!any(grepl("grid", names(pop)))){
		stop(paste("getPopContext wasn't supplied with a population, pop =", substitute(pop)[2]))
	}
	
	args <- list(...)
	
	argNames <- vapply(paste0(substitute(as.list(...)))[-1], function(x){
		gsub("apr[$]", "", gsub("cfg[$]", "", x))
	}, "e", USE.NAMES = FALSE)
	
	
	out <- list()
	out$xs <- map2block(pop$grid[, 1],1)
	out$ys <- map2block(pop$grid[, 2],2)
	
	pos <- cbind(out$xs, out$ys)
	if (length(args) > 0){
		for(ain in 1:length(args)){
			out[[argNames[ain]]] <- if(is.matrix(args[[ain]])) {
				dm <- dim(args[[ain]])
				if (length(dm)>2){
					pwtime <- cbind(pos, 1:dm[3]) 
					args[[ain]][pwtime]
							 
				} else args[[ain]][pos]
			} else {
				args[[ain]]
			}
		}
	}
	return(out)
}

checkContext <- function(ctx, ...){
	reqVars <- c(...)
	allNames <- names(ctx)
	test <- vapply(reqVars, function(v){
		any(grepl(v, allNames))
		
	}, TRUE)
	missVars <- reqVars[!test]
	if(length(missVars) > 0){
		 stop(paste0(rev(names(sys.frames()))[1], ": did not recieve these variables in ctx\n", 
		 						paste(missVars, collapse = "\n")
		 ))
	}
	invisible(0)
}

willFly <- function(pop, genFlag, ctx){
	#From the population figure out how many fly than make two knew populations (stayFAW and mFAW)
	checkContext(ctx, 
               "CornGDD",
               "infestLmt",
               "infestThres",
               "flightPropBeforeSilk",
               "flightPropAfterSilk")
	
	
	stayFaw <- mFaw <- pop
	
	cGDD <- ctx$CornGDD/10
	
	if (genFlag) {
		expVal <- (pop$grid[,3] * 
			genFlightProp(cGDD,
			ctx$infestLmt,
			ctx$flightPropBeforeSilk,
			ctx$flightPropAfterSilk))
		
	} else expVal <- pop$grid[,3] * (1-howLivable(cGDD, ctx$infestThres))
	
	expVal[(is.na(expVal)| expVal < 0) ] <- 0
	
	numFly <- vector("numeric", length(expVal))
	
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
		s <- 1:6
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

growMoths <- function(pop, ctx){
	checkContext(ctx, 
	             "CornGDD",
               "lifeSpan",
               "infestLmt",
               "infestThres",
               "eggsPerInfest",
               "oviDay",
               "capEggs")
	
	grdLen <- length(ctx$xs)

	#first do growth and death
	pop$daysOld <- pop$daysOld+1
	
	
	#Death
	if (pop$daysOld > ctx$lifeSpan){
		deathTest <- !logical(grdLen)
	} else {
		deathTest <- (is.na(ctx$xs) | is.na(ctx$ys))
	}
	if(max(deathTest)){
		#numFlights <<- c(numFlights,pop$flights)
	}
	pop$grid[deathTest,3] <- (-9999)
	
	
	#Lay Eggs?
	newEggs <- list()
	opop <- pop
	
	laySet <- (pop$grid[,3] > 0 &
						 ctx$CornGDD/10 < ctx$infestLmt &
						 ctx$CornGDD/10 > ctx$infestThres)
	
	if (pop$daysOld > ctx$oviDay &&
			pop$numEggs > ctx$eggsPerInfest &&
			any(laySet)){
		
		remEggs <- rep.int(pop$numEggs, grdLen)
		
		remEggs[laySet] <- pop$numEggs - ctx$eggsPerInfest
		
		nEggPos <- cbind(
			pop$grid[laySet, 1],
			pop$grid[laySet, 2],
			ctx$eggsPerInfest * pop$grid[laySet, 3])
		
		newEggs <- lapply(1:dim(nEggPos)[1], function(x){
			makeLife(0, nEggPos[x, ], 0, pop$origin, ctx$capEggs)
		})
		
	
		
		#seperate the population if a portion did not lay eggs and the others did
		
		niq <- unique(remEggs)
		if( length(niq) > 1){
			opop <- list()
			for (q in seq(1,length(niq))){
				opop[[q]] <- pop
				opop[[q]]$grid <- pop$grid[remEggs==niq[q], ,drop=FALSE]
				opop[[q]]$numEggs <- niq[q]
			}
		} else opop$numEggs <- niq
	
	}
	
	if (length(names(opop))>0) opop <- cleanGrid(opop)

	
	out <- list(opop,newEggs)
	return(out)
}

growCohort <- function(lpop, map2block, di, apr,
                       altCornDay, infestThres, cohortGDDThres, capEggs){
	
	if (length(lpop)<1) return(lpop)
	
	tab <- makePopTable(lpop)
	xs <- map2block(tab[,1],1)
	ys <- map2block(tab[,2],2)
	org <- ifelse(tab[,5],"FL","TX")
	
	#growth
	aDD <- tab[,4]
	for (t in seq(di,di+6)){
		aDD <- aDD + apr$FawGDD[cbind(xs, ys,t)]/10
	}
	
	#tests for death and adulthood
	if (di < altCornDay) {deadTest <- (apr$Corn[cbind(xs,ys)] < 1)
	}	else {
		deadTest <- (apr$CornGDD[cbind(xs,ys,di)] / 10 < infestThres)
	}
	
	adultTest <- (aDD > cohortGDDThres)
	
	removeTest <- (adultTest | deadTest)
	
	#Create adults
	tadult <- lapply(which(adultTest),function(x) makeLife(1,tab[x,1:3],0,org[x], capEggs))
	
	#set GDD for those that remain
	for (gg in which(!removeTest)){
		lpop[[gg]]$GDD <- aDD[gg]
	}
	
	#remove the ones that died or turned into an adult
	lpop <- lpop[!removeTest]
	
	return(list(lpop,tadult))
}

migrateDeath <- function(pop, di, ctx){
	checkContext(ctx, "Corn", "CornGDD", "altCornDay", "infestThres")
	
	grd <- pop$grid
	
	outOfMap  <- (is.na(ctx$xs) | is.na(ctx$ys))
	
	
	if(pop$origin=="FL" && di < ctx$altCornDay){
		bakersfield <- (is.na(ctx$Corn) | ctx$Corn < 0.01)
	} else {
		bakersfield <- howLivable(ctx$CornGDD/10, ctx$infestThres)==0
	}
	
	deathTest <- (outOfMap | bakersfield)
	
	pop$grid[deathTest,3] <- (-9999)
	pop$grid[!deathTest,3] <- round(pop$grid[!deathTest,3],2)
	
	pop <- cleanGrid(pop)
	return(pop)
}

deconst <- function(lpop){

	if (length(lpop)>0){
		numRow <- vapply(lpop, function(x) dim(x$grid)[1],1)
		needsDecon <- which(numRow>1)
		for (n in needsDecon){
			for (r in seq(2,numRow[n])){
				lpop<-lappend(lpop,lpop[[n]])
				lpop[[length(lpop)]]$grid<-lpop[[n]]$grid[r,,drop=FALSE]
			}
			lpop[[n]]$grid<-lpop[[n]]$grid[1,,drop = FALSE]
		}
	}
	
	return(lpop)
}

makeRowVec<-function(x){
	vec <- as.matrix(x)
	if (dim(vec)[1]>1 && dim(vec)[2]==1) vec <- t(vec)
	return(vec)
}

makePopTable <- function(lpop,desOrigin=0,verboseNames=FALSE){
	if ( is.numeric(desOrigin))subpop <- lpop
	else {
		orgs <- vapply(lpop,function(x)x$origin,"")
		vec <- charmatch(orgs,desOrigin)
		subpop <- lpop[vec]
	}
	if ( length(subpop)>0){

	subpop <- deconst(subpop)
	

	mat <- t(vapply(subpop, function(x){
		cbind(x$grid,
			x[[3]],
			switch(x$origin,TX=0,FL=1))
		}, rep(1, 5), USE.NAMES=FALSE))
	
	if(verboseNames){
		colnames(mat) <- c("Lon","Lat","Amt","Age","Origin")
		mat[,5] <- vapply(mat[,5],function(x) ifelse(x!=0,"FL","TX"),"TE")
	}
	
	return(mat)
	}
}

combineEggs <- function(lpop, map2block, thres=1){

	if (length(lpop)>1){
	tab <- makePopTable(lpop)
	xs <- map2block(tab[,1], 1)
	ys <- map2block(tab[,2], 2)
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

combinelPop <- function(lpop, map2block){
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
  		ctx <- getPopContext(lpop[[gg]], map2block)
  		mat <- cbind(ctx$xs,ctx$ys)
  		
  	
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



makeOutput <- function(lpop, out, date, map2block, dirSim, lpop2=0,
                       onlyTxt = FALSE,
                       shWrite = TRUE){
	
	#lpop is for migrants, lpop2 is for stationary moths
	xsMap <- as.list(environment(map2block))$xaxis
	ysMap <- as.list(environment(map2block))$yaxis
	dimSlice <- c(length(xsMap), length(ysMap))
	

	tab <- makePopTable(lpop)
	#if (length(tab)<7) tab<-t(as.matrix(tab))

	if (!is.numeric(lpop2)){
    poptype <- "Moth"
		tab2 <- makePopTable(lpop2)

		#add the second population with an additional flag
		tab <- cbind(tab, t(t(rep(1,dim(tab)[1]))))
		tab2 <- cbind(tab2, t(t(rep(0,dim(tab2)[1]))))
		tab <- rbind(tab, tab2)

	} else poptype <- "Cohort"
	
	
	xs <- map2block(tab[,1],1)
	ys <- map2block(tab[,2],2)
	week <- as.integer(strftime(date,"%U"))
	#reorganize sparse matrix into full matrix
	slice <- list(array(0, dim=dimSlice), array(0, dim=dimSlice))
	
	for (ae in seq(1,dim(tab)[1])){
		type <- tab[ae,5]+1
		slice[[type]][xs[ae],ys[ae]]  <- slice[[type]][xs[ae],ys[ae]] + tab[ae,3]
	}
	if (!onlyTxt){
  	for (pt in seq(1:length(out))){
  		out[[pt]][,,week] <- slice[[pt]]
  	}
	}

	str <- paste(poptype,"_week%s_%s.txt",sep="")
	if(shWrite){
		write.table(tab,file=paste0(dirSim, '/',
															  sprintf(str,zstr(week), strftime(date,"%m%d%y"))),
								col.names=FALSE, row.names=FALSE)
		
		#write the nc slices
		#definitions for the single nc files
		dims <- list(dim.def.ncdf("lon", "deg E", xsMap),
								 dim.def.ncdf("lat", "deg N", ysMap)
								 )
		fVars <- list(var.def.ncdf('Count', 'number', dims,1.e30))
		varNames <- list(paste0("TX", poptype),
										 paste0("FL", poptype))
		for(vInd in seq(1, length(varNames))){
			outFile <- paste(dirSim, "ncs",
											 paste(varNames[[vInd]],
											 			strftime(date, "%m%d%y.nc"), sep="_"), sep='/')

			onc <-create.ncdf(outFile, fVars)
			put.var.ncdf(onc, "Count", slice[[vInd]])
			close.ncdf(onc)
		}
	}
	
	return(out)
}

cleangrowMoths <- function(lpop, lEggs, di, cfg, apr, map2block){
	addMoth <- list()
	if (length(lpop)>0){
		lpop <- cleanPop(lpop)
		for (mi in seq(1, length(lpop))){
			ctx <- getPopContext(lpop[[mi]], map2block,
                           apr$CornGDD[, , di],
                           cfg$infestLmt,
                           cfg$lifeSpan,
                           cfg$infestThres,
                           cfg$eggsPerInfest,
                           cfg$oviDay,
                           cfg$capEggs
													 )
			
			tres <- growMoths(lpop[[mi]], ctx)
			
			if (length(names(tres[[1]]))>0){
				lpop[[mi]] <- tres[[1]]
			} else {
				lpop[[mi]] <- tres[[1]][[1]]
				addMoth <- lappend(addMoth,tres[[1]][2:length(tres)])
			}
			lEggs <-lappend(lEggs, tres[[2]])
		}
		lpop <- lappend(lpop,addMoth)
		#lpop <- lapply(lpop,function(x) cleanGrid(x, thres=mothThres))
		lpop <- cleanPop(lpop)
		
	}
	
	return(list(lpop,lEggs))


}

overWinter <- function(region, origin, map2block, cornMap, stAmt, relAmtFL, capEggs){
	if (is.double(region)){
		#latitude thres
		xsMap <- as.list(environment(map2block))$xaxis
		ysMap <- as.list(environment(map2block))$yaxis
		
		possYs <- which(ysMap < region)
		possXs <- switch(origin,
										 TX=which(xsMap < (-90)),
										 FL=which(xsMap > (-90)))
		locInd <- which(cornMap[possXs,possYs] > 1, arr.ind=TRUE)
		
		xs <- map2block(possXs[locInd[,1]], 1, FALSE)
		ys <- map2block(possYs[locInd[,2]], 2, FALSE)
		locNum <- dim(locInd)[1]
		numPerLoc <- round((stAmt * switch(origin,
												TX = (1-relAmtFL),
												FL = relAmtFL))/locNum,1)
		
		lpop <- lapply(1:locNum, function(ind){
				makeLife(0, cbind(xs[ind], ys[ind], numPerLoc), 0, origin, capEggs)
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
