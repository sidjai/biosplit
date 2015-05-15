#'Post-processing: Extract model output at trap locations for comparison
#'
#'Extracts the model output by location and outputs the 
#'	time series for the entire year for that location
#'
#'@param dirSim The simulation output folder 
#'@param pathTrap The path to the trap location csv file with the 
#'	lat/lon of all trap locations
#'@param useCombined Should the program use the combined Final.nc file 
#'	or the nc slices in the nc folder to use do the snap shot calculation
#'@param shWrite Should the function write the horizontal and 
#'	vertical csv files or just return the horizontal matrix
#'@param shDoSum Go through the individual slices and sum up for the week 
#'	instead of using the combined file
#'@return shWrite = 1: only writes the csv file
#'	      shWrite = 0: A matrix of the horizontal time series 
#'	 				with the identifying information in front
#'@details For the summation analysis type, sometimes there may be less than 7
#' 	files in the ncs folder due to model conditions (cfg$outEveryDayStart) so 
#'	output whether the week has all the files as a logical 52 week vector along 
#'	with a combined nc file which is used like the regular ncfile throughout.
#'	outputs the summation of less than 7 files anyway.
#'@export
ncdf2trapdata <- function(dirSim, 
													pathTrap,
													useCombined = TRUE,
													shDoSum = FALSE,
													shWrite = TRUE){
	
	pathNc <- paste(dirSim, "Final.nc", sep="/")
	pathOut <- paste(dirSim, "Trap", sep="/")
	
	year <- as.numeric(regmatches(dirSim,regexpr("\\d{4}", dirSim)))
	
	nc <- ncdf::open.ncdf(pathNc)
	
	Assump <- ncdf::att.get.ncdf(nc,0,"Assumptions")$value
	simData <- ncdf::att.get.ncdf(nc,0,"simData")$value
	
	varNames <- names(nc$var)
	mod <- lapply(varNames, function(x) ncdf::get.var.ncdf(nc,x))
	names(mod) <- varNames
	
	ncdf::close.ncdf(nc)
	
	if (!useCombined) mod <- rebuildNc(dirSim, dim(mod$TXMoth), year)
	
	if(shDoSum){
		mod <- rebuildNc(dirSim, dim(mod$TXMoth), year, TRUE)
		names(mod)[3] <- 'fullWeek'
	}
	
	#parse the trap input for the x,y grid points
	traps <- read.csv(pathTrap, stringsAsFactors=FALSE)
	lonInd <- grep("on", colnames(traps))
	lons <- traps[, lonInd]
	lons[which(lons>0)] <- (-lons[which(lons>0)])
	xb <- vapply(lons, function(x) trap2block(x,nc$dim$lon$vals),1)
	yb <- vapply(traps[, grep("ati", colnames(traps))],
							 function(y) trap2block(y,nc$dim$lat$vals),1)
	
	
	#intiatialize the out table
	inSize <- dim(traps)
	tab <- matrix(nrow = 2*inSize[1], ncol = inSize[2]+1)
	tSer <- matrix(nrow = 2*inSize[1], ncol = 52)
	
	fi <- 1
	nnSet <- ""
	idenInd <- vapply(c("ounty","tate"),function(x) grep(x, names(traps)), 1 , USE.NAMES = FALSE)
	
	for (el in seq(1, inSize[1])){
		for(co in seq(1, inSize[2])){
			tab[fi,co] <- traps[[co]][[el]]
			tab[fi+1,co] <- traps[[co]][[el]]
		}
		
		tab[fi, inSize[2]+1] <-"FL"
		tab[fi+1, inSize[2]+1] <-"TX"
		
		#do Time series
		
		tSer[fi,] <- mod$FLMoth[xb[el], yb[el], ]
		tSer[fi+1,] <- mod$TXMoth[xb[el], yb[el], ]
		
		#If no moths in area, try nearest neighbor
		#Reasons: Beach area, near national park, dead spot in corn
		totMoth <- sum(tSer[fi,],tSer[fi+1,])
		if (totMoth == 0){
			identifier <- paste(tab[fi , idenInd[1]], tab[fi , idenInd[2]], sep = ', ')
			nnind <- matrix(data = 0, nrow = 1, ncol = 2)
			#load up inds
			dist <- ifelse(grepl("Miami", identifier), 3, 1)
			for(xp in seq(xb[el] - dist, xb[el] + dist)){
				for(yp in seq(yb[el] - dist, yb[el] + dist)){
					nnind <- rbind(nnind, cbind(xp, yp))
				}
			}
			nnind <- nnind[-1,]
			
			nnk <- 1
			while(totMoth==0 && nnk <= dim(nnind)[1]){
				nns <- rbind(mod$FLMoth[nnind[nnk,1], nnind[nnk,2], ],
										 mod$TXMoth[nnind[nnk,1], nnind[nnk,2], ])
				totMoth <- sum(nns)
				nnk <- nnk+1
			}
			if (nnk <= dim(nnind)[1]){
				tSer[fi, ] <- nns[1, ]
				tSer[fi+1, ] <- nns[2, ]
				nnSet <- c(nnSet, identifier)
			}
		}
		
		fi <- fi+2
	}
	nnSet <- nnSet[-1]
	
	tSer <- tSer[ ,1:52]
	colnames(tab) <- c(names(traps), "Origin")
	#write the dates as the column name
	days <- seq(8,365,7)
	colnames(tSer) <- getDayStamp(days,year)
	notFullweekSet <- if(shDoSum){
		getDayStamp(days[!mod$fullWeek],year)
	} else {
		FALSE
	}
	
	outh <- cbind(tab, tSer)
	outh <- addAppendix(outh, Assump, simData, nnSet, notFullweekSet)
	
	#Now do the vertical output
	outv <- matrix(nrow = 1, ncol = inSize[2]+6)
	vertNeed <- seq(1, dim(tab)[2]-1)
	r <- 1
	blank <- vapply(1:(inSize[2]+1),function(x) "","")
	
	while (r<=dim(tab)[1]){
		outv <- rbind(outv, 
									c(tab[r,vertNeed],
										colnames(tSer)[1], 
										days[1],
										1,
										tSer[r,1], 
										tSer[r+1,1], 
										"New station"))
		
		for (ti in 2:52){
			#outv <- rbind(outv,c(blank,colnames(tSer)[ti],tSer[st,ti]))
			outv <- rbind(outv,
										c(tab[r,vertNeed],
											colnames(tSer)[ti],
											days[ti],
											ti,
											tSer[r,ti],
											tSer[r+1,ti],
											""))
		}
		r <- r+2
		
	}
	outv <- outv[2:dim(outv)[1], ]
	colnames(outv) <- c(colnames(tab)[vertNeed],
											"Date", 
											"Day",
											"Week",
											"FL Moths", 
											"TX Moths", 
											"New")
	
	outv <- addAppendix(outv, Assump, simData, nnSet, notFullweekSet)
	
	if(shWrite){
		prepend <- ifelse(shDoSum,'Sum','Snap')
		write.csv(outh, paste0(pathOut, prepend, "H.csv"), row.names=FALSE)
		write.csv(outv, paste0(pathOut, prepend, "V.csv"), row.names=FALSE)
	} else {
		return(outh)
	}
}
	


trap2block <- function(vin, mapvec){
	
	diff <- abs(mapvec - vin)
	return(which.min(diff))
}

getDayStamp <- function(jd, yr, outPat = ' %m/%d/%y'){
	strftime(
		strptime(paste(jd, yr), "%j %Y"), 
		outPat)
}


addAppendix <- function(res,
												assump,
												simData,
												nearestSet,
												lessThanWeekSet = FALSE){
	flagSum <- length(lessThanWeekSet) > 1
	
	width <- dim(res)[2]
	
	apen <- ""
	apen[2] <- paste("#",assump)
	apen[3] <- paste("#",simData)
	apen[4] <- paste("# Trap file:",cfg$trapName)
	apen[5] <- ""
	apen[6] <- paste("# Used Nearest neighbor:",
									 paste(nearestSet, collapse = " | "))
	apen[7] <- ""
	
	if(flagSum){
		apen[8] <- "# Analysis type: Sum"
		apen[9] <- paste("# weeks with less than 7 observations:",
										 paste(lessThanWeekSet,collapse = "|"))
	} else {
		apen[8] <- "# Analysis type: Snapshot"
	}
	
	for(ele in seq(1,length(apen))){
		res <- rbind(res, fillWid(apen[ele], width))
	}
	
	return(res)
}

fillWid <- function(str,wid){
	val <- c(str,vapply(1:(wid-1),function(x) "",""))
}

quickOpenNCDF <- function(p, var = "Count"){
	nc <- ncdf::open.ncdf(p)
	out <- ncdf::get.var.ncdf(nc, var)
	ncdf::close.ncdf(nc)
	
	return(out)
}

rebuildNc <- function(dirSim, outDim, yr, flagSum = FALSE){
	days <- seq(8,365,7)
	dates <- getDayStamp(days, yr, '_%m%d%y.nc')
	
	out <- list(array(0, dim = outDim),
							array(0, dim = outDim),
							array(FALSE, dim = c(length(days))))
	
	bakersgrid <- array(NaN, dim = outDim[c(1,2)])
	
	popName <- c("TXMoth", "FLMoth", 'fullWeek')
	for (type in 1:2){
		slFiles <- paste0(dirSim, '/', popName[type], dates)
		for (k in seq(1,length(dates))){
			if(flagSum){
				#Get the summation of all the captures in that week 
				#gets whether it used the full week and puts it in $fullWeek
				prevWeek <- getDayStamp((days[k]-6):(days[k]), yr,'_%m%d%y.nc')
				
				trapCap <- vapply(paste0(dirSim, '/ncs/', popName[type], prevWeek), function(f){
					if(file.exists(f)){
						return(quickOpenNCDF(f))
					} else {
						return(bakersgrid)
					}
				},bakersgrid)
				out[[type]][ , , k] <- rowSums(trapCap, na.rm = TRUE, dims = 2)
				out[[3]][k] <- (7 == length(which(vapply(1:7, function(x){
					!all(is.na(trapCap[, , x]))
				},TRUE))))
				
			} else {
				#just get the end of the week as a snapshot
				
				if(file.exists(slFiles[k])){
					out[[type]][, ,k] <- quickOpenNCDF(slFiles[k])
				}
			}
			
		}
	}
	names(out) <- popName
	return(out)
}

