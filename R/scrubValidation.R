#' Make the haplotype data easier to look at
#'
#' @param pathXlsx The path to the xlsx with the Haplotype data
#' @param pathTrapDict A csv with the translation of all the County names to the latLon location
#' @param pathCsvOut The csv location where the nice data should be outputed
#' @param maskBefore2011 should values before 2011 be used?
#' @param onlyYr an array of the years you want to be kept
#' @param maskNumMoth The minimum number of moths that are required for a proper Haplotype experiment
#'
#' @return The final matrix and a csv file that was written if pathCsvOut was specified
#' @export
scrubHaplo <- function(pathXlsx, pathTrapDict,
											 pathCsvOut = '',
											 maskBefore2011 = TRUE,
											 onlyYr = c(2011, 2012, 2013, 2014),
											 maskNumMoth = 15){
	
	outTab <- cbind("Label", "Latitude", "Longitude", "Year", "Start (julian day)", "End (julian day)","Total number of Moths", "Mix Ratio (h2/h4)")

	dictTrap <- as.matrix(read.csv(pathTrapDict))
	dictTrap[, 1] <- paste(dictTrap[, 1], dictTrap[, 2], sep = ', ')
	
	hapColType <- rep("numeric",13)
	hapColType[2:4] <- "text"
	for (sh in (readxl::excel_sheets(pathXlsx))){
		sheet <- readxl::read_excel(pathXlsx,
																sheet = sh,
																col_types = hapColType)
																						 
		
		DNESet <- is.na(sheet[ ,1, drop = FALSE])
		usefulEle <- sheet <- sheet[!DNESet,]
		
		maskSet <- rep(FALSE,dim(usefulEle)[1])
		
		if(maskBefore2011){
			maskSet <- (maskSet | sheet[,1, drop = FALSE] < 2011)
		}
		allYrs <- c(2011, 2012, 2013, 2014)
		maskYr <- allYrs[!is.element(allYrs, onlyYr)]
		maskSet <- (maskSet | is.element(as.matrix(sheet[,1, drop = FALSE]), maskYr))
		maskSet <- (maskSet | sheet[,9, drop = FALSE] < maskNumMoth)
		
		usefulEle <- as.list(sheet[!maskSet, ])
		
		if(length(usefulEle$Year)>0){
			locName <- paste(usefulEle$county, usefulEle$state, sep = ', ')
			latLon <- t(vapply(locName, function(x){
					val <- dictTrap[grepl(x, dictTrap[,1]),3:4]
					if(length(val) == 0){
						stop(paste(x, "is not in the dictionary"))
					}
					return(val)
				}, rep('e',2), USE.NAMES = FALSE))
			
			startDay <- endDay <- rep(1,dim(latLon)[1])
		inDates <- usefulEle$`collection date`
			
			daysAfter1900Set <- grepl("^[0-9]+$",inDates)
			
			endDay[daysAfter1900Set] <- convDaysAfter1900(inDates[daysAfter1900Set])
			startDay[daysAfter1900Set] <- endDay[daysAfter1900Set] - 1
			
			justMonthTry <-date2Jul(paste0("1 / ",inDates), "%d / %Y %b")
			justMonthSet <- !is.na(justMonthTry)
			startDay[justMonthSet] <- justMonthTry[justMonthSet]
			endDay[justMonthSet] <- justMonthTry[justMonthSet] + 30
			
			splitSet <- grepl("[-]", inDates)
			
			if(length(which(splitSet))>0){
				lout <- parseSplitDate(inDates[splitSet])
				startDay[splitSet] <- lout[[1]]
				endDay[splitSet] <- lout[[2]]
			}
			
			id <- cbind(locName, latLon)
			dat <- t(rbind(usefulEle$Year,startDay ,endDay ,usefulEle$total, usefulEle$`CS4/2`))
			totDat <- cbind(id, dat)
	
			outTab <- rbind(outTab, totDat)
		}
	}
	names(outTab) <- NULL
	
	if(nzchar(pathCsvOut)){
		outTab[,1] <- quoteIt(outTab[,1])
		write.table(outTab, file = pathCsvOut, sep = ',', row.names = FALSE, col.names = FALSE, quote = FALSE)
	}
	return(invisible(outTab))
}

convDaysAfter1900 <- function(days){
	days <- as.numeric(days)
	realYr <- floor(days/365)
	dayOff <-  realYr * 365 + floor(realYr/4)
	jd <- (days - dayOff)
	return(jd)
}
parseSplitDate <- function(sDates){
	
	hyphenLoc <- regexpr("[-]", sDates)
	
	formatPos <- c("%b %d-%M %Y",
								 "%b%d-",
								 "%m/%d-%M ",
								 "%m/%d-%M/%S",
								 "%m/%d - %M/%S",
								 "%b-")
	
	guessForm <- vapply(formatPos, function(x){
		if(grepl("%b-", x)){
			date2Jul(paste(1, sDates), paste("%d", x))
		} else {
			date2Jul(sDates, x)
		}
		
	}, rep(1,length(sDates)))
	
	guessForm <- makeRowVec(guessForm)
	splitSeq <- 1:dim(guessForm)[1]
	
	rightInd <- vapply(splitSeq, function(x){
		which(!is.na(guessForm[x, ]))[1]
	},1)
	
	if( any(is.na(rightInd)) ){
		stop(paste(paste(sDates[is.na(rightInd)], collapse ="\n"),
							 "were indecipherable add new date string"))
	}
	
	
	startDay <- guessForm[cbind(splitSeq, rightInd)]
	
	secondHalf <- substr(sDates, hyphenLoc, nchar(sDates))
	
	jdSecond <- rightInd
	jdSecond[rightInd == 1] <- date2Jul(sDates[rightInd==1], "%b %M-%d %Y")
	jdSecond[rightInd == 2] <- date2Jul(secondHalf[rightInd==2], "-%b%d")
	jdSecond[rightInd == 3] <- date2Jul(sDates[rightInd==3], "%m/%M-%d ")
	jdSecond[rightInd == 4] <- date2Jul(sDates[rightInd==4], "%S/%M-%m/%d")
	jdSecond[rightInd == 5] <- date2Jul(sDates[rightInd==5], "%S/%M - %m/%d")
	jdSecond[rightInd == 6] <- date2Jul(paste(secondHalf[rightInd==6], 1), "-%b %d")
	endDay <- jdSecond
	
	return(list(startDay, endDay))
	
}

#' Scrub the trap data from PestWatch
#'
#' @param pathXlsx The path to the xlsx pestwatch dump
#' @param year The year you want to select
#' @param pathCsvOut The path for the indivdual horizontal report file that you 
#' want printed out
#' @param pestType The insect 3 letter identifier that you want
#' @param minTotCatch The minimum total catch for the year to be counted
#' @param verboseRawTrap Should the raw captures for each valid record be outputed?
#'
#' @return A list of the records that are filtered (in a named list) and the 
#' csv document written if you have pathCsv supplied
#' @export
scrubTrap <- function(pathXlsx, year,
											pathCsvOut = '',
											pestType = "faw",
											minTotCatch = 10,
											verboseRawTrap = FALSE){
	trapColType <- rep("numeric",10)
	trapColType[4:5] <- "text"
	trapColType[6] <- "date"
	trapColType[7] <- "text"
	trapColType[10] <- "text"
	
	sheet <- readxl::read_excel(pathXlsx, sheet = 1, col_types = trapColType)
	
	usefulEle <- basicTrapCutoff(sheet, year, pestType)[ , -10]
	
	
	usefulEle[,6] <- as.integer(strftime(usefulEle[,6], "%j"))
	
	usefulEle <- usefulEle[order(usefulEle[,1], usefulEle[,6]),]
	
	current <- rev(rev(usefulEle$farmid)[-1])
	nextEle <- usefulEle$farmid[-1]
	
	farmEndInd <- which(current != nextEle)
	farmStaInd <- c(as.integer(1), rev(rev(farmEndInd)[-1]))
	
	statDat <- t(mapply(function(beg, last){
		catches <- usefulEle[beg:last, 8]
		totCatch <- sum(catches)
		firstOccInd <- which(catches>0)[1]
		firstOcc <- usefulEle[(beg:last)[firstOccInd], 6]
		return(c(totCatch, firstOcc))
	}, farmStaInd, farmEndInd))
	
	badFarm <- (statDat[,1] < minTotCatch)
	
	statDat <- statDat[!badFarm,]
	farmEndInd <- farmEndInd[!badFarm]
	farmStaInd <- farmStaInd[!badFarm]
	
	
	statID <- paste(usefulEle$countyname[farmStaInd], usefulEle$stateabbreviation[farmStaInd], sep = ", ")
	
	
	records <- list()
	for (rind in 1:length(farmEndInd)){
		rseq <- farmStaInd[rind]:farmEndInd[rind]
		
		temp <- getTrapTimeSeries(usefulEle[rseq, 8],
															usefulEle[rseq, 6],
															usefulEle[rseq, 9])
		notes <- ifelse(length(temp[[2]])>0, paste(temp[[2]], collapse =" ! "), "Good")
		
		
		newRec <- list(ID = statID[rind],
									 latlon = c(usefulEle[farmStaInd[rind],2],
									 					 -usefulEle[farmStaInd[rind],3]),
									 totCatch = statDat[rind, 1],
									 firstOcc = statDat[rind, 2],
									 capTSer = temp[[1]],
									 notes = notes
		)
		if (verboseRawTrap){
			rawMat <- cbind(usefulEle[rseq, 8],
											usefulEle[rseq, 6], 
											usefulEle[rseq, 9])
			colnames(rawMat) <- c("catches", "jds", "periods")
			rawMat <- rawMat[order(usefulEle[rseq, 6]),]
			newRec$rawTrap = rawMat
		}
		
		sameCountySet <- grepl(statID[rind], vapply(records,function(x) {x$ID},"e"))
		if(any(sameCountySet)){
			oldRecInd <- which(sameCountySet)[1]
			if(statDat[rind,1] > records[[oldRecInd]]$totCatch){
				records[[oldRecInd]] <- newRec
			}
			
		} else {
			records <- lappend(records, newRec)
		}
	}
	
	if(nzchar(pathCsvOut)){
		header <- rbind(names(c(records[[1]][1:6], recursive = TRUE)))
		header[1, 6:57] <- getDayStamp(seq(8,365,7), year)
		dat <- vapply(records, function(rec){
			rbind(c(rec[1:6], recursive = TRUE))
		}, header)
		dat <- t(dat[1,,])
		dat[,1] <- quoteIt(dat[,1])
		colnames(dat) <- NULL
		write.table(rbind(header, dat), file = pathCsvOut,
								sep = ',', row.names = FALSE, col.names = FALSE, quote = FALSE)
	}
	return(invisible(records))
}

getTrapTimeSeries <- function(catches, jds, periods){
	
	notes <- c()
	#get rid of articifically low catch periods because of pestWatch limit of 14
	realPeriods <- (jds[-1] - rev(rev(jds)[-1]))
	realPeriods <- c(realPeriods[1], realPeriods)
	if(all(periods == 14 & realPeriods > 14)){
		notes <- c(notes, sprintf("Old catchperiod: %f, New catchperiod: %f", 
															mean(periods), 
															mean(realPeriods)))
		periods <- realPeriods
		
	}
	#normalize to weekly captures
	normMat <- normalizeWk(catches, jds, periods)
	
	tSer <- rep(0, 52)
	niceMat <- sumDups(normMat)
	
	catInd <- niceMat[,1]
	amtPerWeek <- (niceMat[,"catches"] / niceMat[,"periods"]) * 7
	
	tSer[catInd] <- round(amtPerWeek, 1)
	
	if( amtPerWeek[order(catInd)][1] > 0 ){
		notes <- c(notes, "Capture on first week")
	}
	
	
	nonZ <- which(tSer!=0)
	if (any(rev(rev(nonZ)[-1]) + 3 < nonZ[-1])){
		notes <- c(notes, "Gap of a month or more in FAW Catches")
	}
	
	return(list(tSer, notes))
	
}

basicTrapCutoff <- function(sheet, year, pest){
	inYr <- as.integer(strftime(sheet[,6], "%Y"))
	
	badYearSet <- (inYr != year)
	badPestSet <- !grepl(pest, sheet[,7])
	noCountySet <- is.na(sheet[,4])
	tooCanadaSet <- grepl("QC", sheet[,5]) & (sheet[,2] > 44 | sheet[,3] < 78)
	
	return(sheet[!(badYearSet | 
								 badPestSet |
								 noCountySet | 
								 tooCanadaSet), ])
}
sumDups <- function(vin){
	combineCols <- (2:dim(vin)[2])
	dupSet <- duplicated(vin[,1])
	while (any(dupSet)) {
		dupEle <- vin[which(dupSet)[1],1]
		dupInds <- (vin[,1] == dupEle)
		for(co in combineCols){
			vin[which(dupInds)[1],co] <- sum(vin[dupInds, co])
		}
		vin <- vin[-which(dupInds)[-1], ,drop = FALSE]
		
		dupSet <- duplicated(vin[,1])
	}
	return(vin)
	
}

normalizeWk <- function(catches, jds, periods){
	days <- seq(1,364,7)
	
	collectWk <- findInterval(jds, days)
	begWk <- findInterval(jds-periods, days)
	tooEarlySet <- (begWk == 0) | (collectWk ==0)
	
	collectWk <- collectWk[!tooEarlySet]
	jds <- jds[!tooEarlySet]
	begWk <- begWk[!tooEarlySet]
	periods <- periods[!tooEarlySet]
	catches <- catches[!tooEarlySet]
	
	resMat <- cbind(collectWk, catches, periods)
	
	splitSet <- (collectWk != begWk)
	
	for (spl in which(splitSet)){
		trapSeq <- (jds[spl] - periods[spl] +1) : jds[spl]
		bins <- findInterval(trapSeq, days)
		
		newSplit <- unique(bins)
		newPeriods <- vapply(newSplit, function(x){
			sum(bins==x)
			
		},1)
		newCatch <- (catches[spl]/periods[spl]) * newPeriods
		
		newRec <- cbind(newSplit, newCatch, newPeriods)
		
		resMat[spl,] <- newRec[1,,drop = FALSE]
		resMat <- rbind(resMat, newRec[-1,,drop = FALSE])
	}
	
	return(resMat)
}