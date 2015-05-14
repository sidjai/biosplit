#'Post-processing: Extract model output at trap locations for comparison
#'
#'Extracts the model output by location and outputs the 
#'	time series for the entire year for that location
#'
#'@param dirSim The simulation output folder 
#'@param pathTrap The path to the trap location csv file with the 
#'	lat/lon of all trap locations
#'@param useCombined Should the program use the combined Final.nc file 
#'	or the nc slices in the nc folder
#'@param shWrite Should the function wrtie the horizontal and 
#'	vertical csv files or just return the horizontal matrix
#'
#'@return shWrite = 1: only writes the csv file
#'	 shWrite = 0: A matrix of the horizontal time series 
#'	 with the identifying information in front
#'@export
ncdf2trapdata <- function(dirSim, pathTrap, useCombined = TRUE, shWrite = TRUE){
	
	pathNc <- paste(dirSim, "Final.nc", sep="/")
	pathOut <- paste(dirSim, "TrapFinal", sep="/")
	
	year <- as.numeric(regmatches(dirSim,regexpr("\\d{4}", dirSim)))
	
	nc <- ncdf::open.ncdf(pathNc)
	
	Assump <- ncdf::att.get.ncdf(nc,0,"Assumptions")$value
	simData <- ncdf::att.get.ncdf(nc,0,"simData")$value
	
	varNames <- names(nc$var)
	mod <- lapply(varNames, function(x) ncdf::get.var.ncdf(nc,x))
	names(mod) <- varNames
	
	ncdf::close.ncdf(nc)
	
	if (!useCombined) mod <- rebuildNc(dim(mod$TXMoth))
	
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
	colnames(tSer) <- getTimeStamps(year)
	
	outh <- cbind(tab, tSer)
	outh <- addAppendix(outh, Assump, simData, nnSet)
	
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
	
	outv <- addAppendix(outv, Assump, simData, nnSet)
	
	if(shWrite){
		write.csv(outh, paste0(pathOut, "h.csv"), row.names=FALSE)
		write.csv(outv, paste0(pathOut, "v.csv"), row.names=FALSE)
	} else {
		return(outh)
	}
}
	


trap2block <- function(vin, mapvec){
	
	diff <- abs(mapvec - vin)
	return(which.min(diff))
}

getTimeStamps <- function(yr,outPat = ' %m/%d/%y'){
	days <- seq(8,365,7)
	return(vapply(days, function(x)
		strftime(
			strptime(paste(x,yr), "%j %Y")
			," %m/%d/%y"),""))
}

addAppendix <- function(res,assump, simData, nearestSet){
	width <- dim(res)[2]
	res <- rbind(res, fillWid("",width))
	res <- rbind(res, fillWid(paste("#",assump),width))
	res <- rbind(res, fillWid(paste("#",simData),width))
	res <- rbind(res, fillWid(paste("# Trap file:",cfg$trapName),width))
	res <- rbind(res, fillWid("",width))
	res <- rbind(res, fillWid(paste("# Used Nearest neighbor:",
		paste(nearestSet, collapse = " | ")), width))
	
	return(res)
}

fillWid <- function(str,wid){
	val <- c(str,vapply(1:(wid-1),function(x) "",""))
}

rebuildNc <- function(outDim){
	
	dates <- getTimeStamps(year,'Moth_%m%d%y.nc')
	Txfiles <- vapply(dates, function(x)paste0(sliceFiles, "/TX" ,x),"")
	Flfiles <- vapply(dates, function(x)paste0(sliceFiles, "/FL" ,x),"")
	slfiles <- list(Txfiles, Flfiles)
	out <- list(array(0, dim = outDim), array(0, dim = outDim))
	
	for (type in 1:2){
		for (k in seq(1,length(dates))){
			if(file.exists(slfiles[[type]][[k]])){
				sl <- ncdf::open.ncdf(slfiles[[type]][[k]])
				out[[type]][,,k] <- ncdf::get.var.ncdf(sl, "Count")
				ncdf::close.ncdf(sl)
			}
			
		}
	}
	names(out) <- c("TXMoth", "FLMoth")
	return(out)
}

