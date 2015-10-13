#'Post-processing: Extract model output at trap locations for comparison
#'
#'Extracts the model output by location and outputs the 
#'	time series for the entire year for that location
#'
#'@param dirSim The simulation output folder 
#'@param pathTrap The path to the trap location csv file with the 
#'	lat/lon of all trap locations or the combined records list with all the data
#'@param pathHap The path to the haplotype data in a nice csv output of scrubHap
#'@param useCombined Should the program use the combined Final.nc file 
#'	or the nc slices in the nc folder to use do the snap shot calculation
#'@param shWrite Should the function write the horizontal and 
#'	vertical csv files or just return the horizontal matrix
#'@param shUseSum Go through the individual slices and sum up for the week 
#'	instead of using the combined file
#'@param shAvgSimforHap Should the program add an extra column with the average
#'  mixing ratio for those dates where the haplotype record is spread out 
#'  over more than one week?
#'@param notes A vector or string of any notes that should be included in the
#'	appendix
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
                          pathHap = NULL,
                          useCombined = TRUE,
                          shUseSum = FALSE,
                          shWrite = TRUE,
                          shAvgSimforHap = FALSE,
													notes = ""){
	
	pathNc <- paste(dirSim, "Final.nc", sep="/")
	pathOut <- paste(dirSim, "Trap", sep="/")
	
	year <- file2year(dirSim)
	dat <- openSimNC(dirSim)
	mod <- dat$sim
	
	
	if (!useCombined) mod <- rebuildNc(dirSim, year)
	
	if(shUseSum){
		mod <- rebuildNc(dirSim, year, TRUE)
		names(mod)[3] <- 'fullWeek'
	}
	
	#parse the trap input for the x,y grid points
	if(is.character(pathTrap)){
		trapID <- read.csv(pathTrap, stringsAsFactors=FALSE)
		lonInd <- grep("on", colnames(trapID))
		lons <- trapID[, lonInd]
		lons[which(lons>0)] <- (-lons[which(lons>0)])
		xb <- vapply(lons, function(x) trap2block(x,dat$lon),1)
		yb <- vapply(trapID[, grep("ati", colnames(trapID))],
								 function(y) trap2block(y,dat$lat),1)
		idenInd <- vapply(c("ounty","tate"),function(x) grep(x, names(trapID)), 1 , USE.NAMES = FALSE)
		identifiers <- paste(trapID[ , idenInd[1]], trapID[ , idenInd[2]], sep = ', ')
		
		trapTSer <- NULL
		
	} else {
		trapDat <- pathTrap
		pathTrap <- "data"
		trapID <- t(vapply(trapDat, function(x) { c(x[1:2], recursive = TRUE) }, rep("e",3)))
		trapLat <- as.numeric(trapID[, 2])
		trapLon <- as.numeric(trapID[, 3])
		xb <- vapply(trapLon, function(x) { trap2block(x,dat$lon) }, 1)
		yb <- vapply(trapLat, function(y) { trap2block(y,dat$lat) }, 1)
		
		identifiers <- trapID[, 1]
		
		trapTSer <- t(vapply(trapDat, function(x){ x$capTSer }, rep(0.5,52)))
	}
	
	#Do haplotype data 
	if (length(pathHap)>0){
		hapRatio <- parseHapData(pathHap, trapLat, trapLon)
		
		mixRatio <- calcMixingRatio(mod$FLMoth, mod$TXMoth)
		mixRatio <- t(mapply(function(x,y){ mixRatio[x,y,] }, xb, yb))
	}
	
	#intiatialize the out table
	inSize <- dim(trapID)
	tab <- matrix(nrow = 2*inSize[1], ncol = inSize[2]+1)
	tSer <- matrix(nrow = 2*inSize[1], ncol = 52)
	
	fi <- 1
	nnSet <- ""
	
	for (el in seq(1, inSize[1])){
		for(co in seq(1, inSize[2])){
			tab[fi+1,co] <- tab[fi,co] <- trapID[el, co]
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
			nnind <- matrix(data = 0, nrow = 1, ncol = 2)
			#load up inds
			dist <- ifelse(grepl("Miami", identifiers[el]), 3, 1)
			for(xp in seq(xb[el] - dist, xb[el] + dist)){
				for(yp in seq(yb[el] - dist, yb[el] + dist)){
					if( xp > 0 && xp < length(dat$lon) && yp > 0 && yp < length(dat$lat)){
						nnind <- rbind(nnind, cbind(xp, yp))
					}
				}
			}
			nnind <- nnind[-1, ,drop = FALSE]
			
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
				nnSet <- c(nnSet, identifiers[el])
			}
		}
		
		fi <- fi+2
	}
	nnSet <- nnSet[-1]
	
	tSer <- tSer[ ,1:52]
	colnames(tab) <- c(colnames(trapID), "Origin")
	#write the dates as the column name
	days <- seq(8,365,7)
	colnames(tSer) <- getDayStamp(days,year)
	notFullweekSet <- if(shUseSum){
		getDayStamp(days[!mod$fullWeek],year)
	} else {
		FALSE
	}
	
	
	
	outh <- cbind(tab, tSer)
	outh <- addAppendix(outh, dat$assump, dat$simData, nnSet, 
		pathTrap, notFullweekSet, notes)
	
	#Now do the vertical output
	outv <- matrix(nrow = 1, ncol = inSize[2] + 9)
	vertNeed <- seq(1, dim(tab)[2]-1)
	r <- 1
	blank <- vapply(1:(inSize[2]+1),function(x) "","")
	mixName <- ifelse(shAvgSimforHap,
		"Sim Mix Ratio (smoothed to Hap w avg)",
		"Sim Mix Ratio")
	
	while (r<=dim(tab)[1]){
		locInd <- (r+1)/2
		if (shAvgSimforHap && r > 1) {
			if(any(!is.na(hapRatio[locInd,]))){
				mixRatio[locInd,] <- 
					smoothSimtoHap(mixRatio[locInd,], hapRatio[locInd,], 'avg')
			}
		}
		
		outv <- rbind(outv, 
									c(tab[r,vertNeed],
										colnames(tSer)[1], 
										days[1],
										1,
										tSer[r,1],
										tSer[r+1,1],
										trapTSer[locInd, 1],
										mixRatio[locInd, 1],
										hapRatio[locInd, 1],
										trapDat[[locInd]]$notes))
		
		for (ti in 2:52){
			#outv <- rbind(outv,c(blank,colnames(tSer)[ti],tSer[st,ti]))
			outv <- rbind(outv,
										c(tab[r,vertNeed],
											colnames(tSer)[ti],
											days[ti],
											ti,
											tSer[r,ti],
											tSer[r+1,ti],
											trapTSer[locInd, ti],
											mixRatio[locInd, ti],
											hapRatio[locInd, ti],
											trapDat[[locInd]]$notes))
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
											"Trap Moths",
											mixName,
											"Trap Hap Ratio",
											"Trap Notes")
	
	outv <- addAppendix(outv, dat$assump, dat$simData, nnSet, pathTrap, notFullweekSet, notes)
	
	if(shWrite){
		prepend <- ifelse(shUseSum,'Sum','Snap')
		write.csv(outh, paste0(pathOut, prepend, "H.csv"), row.names=FALSE)
		write.csv(outv, paste0(pathOut, prepend, "V.csv"), row.names=FALSE)
	} else {
		return(outh)
	}
}

smoothSimtoHap <- function(mixRatio, hapTable, method){
	mixAvgTable <- mixRatio

	rstart <- 1
	while(rstart < length(hapTable)){
		r <- rstart + 1
		while(!any(is.na(hapTable[(r-1):r])) &&
				hapTable[r-1] == hapTable[r]){
			r <- r+1
		}
		if(r != rstart + 1){
			seqH <- rstart:(r-1)
			
			mixAvgTable[seqH] <- 
				switch(method,
					avg = mean(mixRatio[seqH], na.rm = TRUE),
					max = max(mixRatio[seqH], na.rm = TRUE)
				)
		}
		rstart <- r
	}
	return(mixAvgTable)
}

#'Post-processing: Summarize the simulation details in a nicely mapped format
#'
#'Either do the time of first Occurance, or the Mixing Ratio for either the 
#'simulation or the difference between the simulation and the trap data.
#'Also can be used with either the snapshot view or the summation for the whole
#'week.
#'
#'@param dirSim The simulation output folder 
#'@param pathTrapGrid The path to the trap grd location
#'@param pathOut The path where the nc file should be saved
#'@param goodProj The projection of the target raster
#'@param shDoMix Should the mixing ratio be calculated or the First occurance?
#'@param shUseSum Should the procedure use the summation of the week or the snapshot?
#'@return A raster of the selected analysis method. If the pathOut is specified,
#'Then the raster is outputed as an netCDF file with proper variable names and units
#'@import raster
#'@export
ncdf2trapgrid <- function(dirSim, 
													pathTrapGrid = '',
													pathOut ='',
													goodProj = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
													shDoMix = FALSE,
													shUseSum = FALSE){
	
	dat <- openSimNC(dirSim)
	mod <- dat$sim
	year <- file2year(dirSim)
	
	if(shUseSum){
		mod <- rebuildNc(dirSim, year, TRUE)
		names(mod)[3] <- 'fullWeek'
	}
	
	boBox <- extent(dat$lon[1], rev(dat$lon)[1], rev(dat$lat)[1], dat$lat[1])
	niceGrid <- raster(boBox,
										 nrows = dim(dat$lat)[1],
										 ncols = dim(dat$lon)[1],
										 crs = goodProj)
	
	
	predMat <- if(shDoMix){
		varName <- "Mixing Ratio"
		unitName <- "unitless"
		calcMixingRatio(mod$FLMoth, mod$TXMoth, timePeriod = "year")
	} else {
		varName <- "First week of Arrival"
		unitName <- "wk"
		calcFirstOcc(mod$FLMoth, mod$TXMoth)
	}
	
	if(shDoMix) predMat[is.nan(predMat)] <- NA
	
	if(nzchar(pathTrapGrid)){
		trapRas <- raster(pathTrapGrid)
		projection(trapRas) <- goodProj
		trapMat <- as.matrix(resample(trapRas, niceGrid))
		
		resMat <- predMat - trapMat
		resRas <- raster(resMat, template = niceGrid)
		varName <- paste(varName, "(Pred-obv)")
		
	} else {
		resRas <- raster(t(predMat), template = niceGrid)
		varName <- paste(varName, "(Pred)")
	}
	
	if(nzchar(pathOut)){
		writeRaster(resRas,
								filename = pathOut,
								format="CDF",
								varname = varName,
								varunit = unitName,
								overwrite=TRUE
		)
		
	}
	return(invisible(resRas))
}


calcFirstOcc <- function(matFL, matTX = array(0, dim = dim(matFL))){
	matFL <- checkInMat(matFL)
	matTX <- checkInMat(matTX)
	
	
	matFirst <- t(vapply(1:dim(matFL)[1], function(xi){
		vapply(1:dim(matFL)[2], function(yi){
			firstObv <- pmin.int(which(matFL[xi,yi,] > 0)[1],
													 which(matTX[xi,yi,] > 0)[1],
													 na.rm = TRUE)
			return(ifelse(is.infinite(firstObv),NA,firstObv))
		},1)
	}, rep(1,dim(matTX)[2])))
	
	return(matFirst)
	
}

calcMixingRatio <- function(matFL, matTX, timePeriod = "week"){
	matFL <- checkInMat(matFL)
	matTX <- checkInMat(matTX)
	
	interMix <- switch(timePeriod, 
									 	 week = list(log10(matFL +1), log10(matTX + 1)),
									   year = list(log10(rowSums(matFL, na.rm = TRUE, dims = 2) +1),
									   						 log10(rowSums(matTX, na.rm = TRUE, dims = 2) +1))
	)
	matMix <- (interMix[[1]] - interMix[[2]]) /(interMix[[1]] + interMix[[2]])
	
	
	return(matMix)
	
}

parseHapData <- function(pathHap, trapLat, trapLon, shAvgYear = FALSE){
	hapDat <- switch(class(pathHap),
		character = as.matrix(read.csv(pathHap)),
		matrix = pathHap[-1,],
		stop(sprintf("ParseHapData requires a path or the matrix of the data. You provided a %c",
			character(pathHap)))
	)
		
	hapDict <- mapply(function(lat, lon){
		which.min(abs(trapLat - lat) + abs(trapLon - lon))
	},as.numeric(hapDat[, 2]), as.numeric(hapDat[, 3]))
	
	startInd <- grep("Start", colnames(hapDat), ignore.case = TRUE)
	endInd <- grep("End", colnames(hapDat), ignore.case = TRUE)
	
	hapRatio <- matrix(data = NA, nrow = length(trapLat), ncol = 52)
	for (tele in 1:dim(hapDat)[1]){
		beg <- jul2Week(hapDat[tele, startInd])
		last <- jul2Week(hapDat[tele, endInd])
		if(is.na(last)) { last <- beg }
		hapRatio[hapDict[tele], beg:last] <- as.numeric(substr(hapDat[tele, dim(hapDat)[2]], 1,4))
	}
	
	if(shAvgYear){
		fullYear <- round(rowMeans(hapRatio, na.rm = TRUE), 2)
		fullYear[is.nan(fullYear)] <- NA
	  return(fullYear)
	} else {
		return(hapRatio)
	}
}



openSimNC <- function(dirSim, asRaster = FALSE){
	pathNc <- paste(dirSim, "Final.nc", sep="/")
	if(!file.exists(pathNc)){
		stop(sprintf("File: %s does not exist, Simulation did not completly finish",
								 pathNc))
	}
	
	if(asRaster){
		out <- raster::raster(pathNc)
	} else {
		nc <- ncdf::open.ncdf(pathNc)
		
		varNames <- names(nc$var)
		sim <- lapply(varNames, function(x) ncdf::get.var.ncdf(nc,x))
		names(sim) <- varNames
		
		
		out <- list(sim = sim,
								assump = ncdf::att.get.ncdf(nc,0,"Assumptions")$value,
								simData = ncdf::att.get.ncdf(nc,0,"simData")$value,
								lat = nc$dim$lat$vals,
								lon = nc$dim$lon$vals
		)
		
		
		ncdf::close.ncdf(nc)
	}
	
	return(out)
	
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

checkInMat <- function(input){
	
	callingFun <- as.list(sys.call(-1))[[1]]
	
	if(grepl("Raster", class(input))){
		input <- raster::as.matrix(input)
	}
	
	if(!is.array(input)){
		stop(sprintf("%s wants an array or a Raster object, you provided a %s",
								 paste0(callingFun),
								 class(input)))
	}
	return(input)
	
}

addAppendix <- function(res,
												assump,
												simData,
												nearestSet,
												pathTrap,
												lessThanWeekSet = FALSE,
												notes = ""){
	
	flagSum <- length(lessThanWeekSet) > 1
	
	width <- dim(res)[2]
	
	apen <- ""
	apen[2] <- paste("#",assump)
	apen[3] <- paste("#",simData)
	apen[4] <- paste("# Trap file:", pathTrap)
	apen[5] <- ""
	apen[6] <- paste("# Used Nearest neighbor:",
									 paste(nearestSet, collapse = " | "))
	apen[7] <- ""
	
	apen[8] <- paste("# Notes:",
									 paste(notes, collapse = " | "))
	if(flagSum){
		apen[9] <- "# Analysis type: Sum"
		apen[10] <- paste("# weeks with less than 7 observations:",
										 paste(lessThanWeekSet,collapse = "|"))
	} else {
		apen[9] <- "# Analysis type: Snapshot"
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

#' Rebuild the sparse matrices with raw data
#'
#' @param dirSim The simulation output folder 
#' @param yr The year of the simulation
#' @param flagSum Should the program sum all the captures for that week in this
#'   file?
#' @param shWrite Should the rebuilt file be written to the directory using
#'   predefined names?
#'
#' @return A large list with the variables as names and 3-d matrix for the data
#' @export
rebuildNc <- function(dirSim, yr, flagSum = FALSE, shWrite = FALSE){
	days <- seq(8,365,7)
	dates <- getDayStamp(days, yr, '_%m%d%y.nc')
	
	#get a sample of output to define bounds
	samplePath <- list.files(paste0(dirSim, "/ncs"), "FLMoth", full.names = TRUE)[1]
	basenc <- ncdf::open.ncdf(samplePath)
	outDim <- c(basenc$var$Count$size, length(days))
	
	out <- list(array(0, dim = outDim),
							array(0, dim = outDim),
							array(FALSE, dim = outDim[3]))
	
	bakersgrid <- array(NaN, dim = outDim[c(1,2)])
	
	popName <- c("TXMoth", "FLMoth", 'fullWeek')
	for (type in 1:2){
		slFiles <- paste0(dirSim, '/ncs/', popName[type], dates)
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
	if(shWrite){
		tempOut <- paste(dirSim,
			paste0("FinalRebuild", ifelse(flagSum, "Sum", "Snap"), ".nc"), 
			sep="/")
		
		
		dims <- list( 
			basenc$dim$lon,
			basenc$dim$lat,
			ncdf::dim.def.ncdf( "Time", "weeks", 1:52, unlim=TRUE ))
		fVars<- list(
			var.def.ncdf('TXMoth', '#Moths',dims,1.e30),
			var.def.ncdf('FLMoth', '#Moths',dims,1.e30))
		
		onc <- create.ncdf(tempOut, fVars)
		
		put.var.ncdf(onc, "TXMoth", out[[1]])
		put.var.ncdf(onc, "FLMoth", out[[2]])
		
		close.ncdf(onc)
		
		return(invisible(out))
	} else {
		return(out)
	}
}

