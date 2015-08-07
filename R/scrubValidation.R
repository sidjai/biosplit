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
	
	startDay <- endDay <- rep(-9999, length(sDates))
	
	hyphenLoc <- regexpr("[-]", sDates)
	firstHalf <- substr(sDates, 1, hyphenLoc -1)
	secondHalf <- substr(sDates, hyphenLoc + 1, nchar(sDates))
	
	cmpYear <- grepl("\\d{5}",sDates)
	sDates[cmpYear] <- substr(sDates[cmpYear],
	                              1, nchar(sDates[cmpYear]) - 4)
	numSlashes <- getNumSymbol(sDates, "[/]")
	numMonths <- getNumSymbol(sDates, "[a-zA-Z]")/3
	
	
	form <- (numSlashes == 0 & numMonths == 2)
	if(any(form)){
		stTok <- ifelse(0 < getNumSymbol(firstHalf[form], "[0-9]"),
		                firstHalf[form],
		                paste0(firstHalf[form], 1))
		endTok <- gsub("\\d{4}","", secondHalf[form])
		endTok <- ifelse(0 < getNumSymbol(endTok, "[0-9]"),
		                 endTok,
			               paste0(endTok, 1))
		
		startDay[form] <- date2Jul(stTok, "%b%d")
		endDay[form] <- date2Jul(endTok, "%b%d")
	}
	
	form <- (numSlashes == 0 & numMonths == 1)
	startDay[form] <- date2Jul(sDates[form], "%b %d-%M")
	endDay[form] <- date2Jul(sDates[form], "%b %M-%d")
	
	form <- (numSlashes == 1)
	startDay[form] <- date2Jul(sDates[form], "%m/%d-%M")
	endDay[form] <- date2Jul(sDates[form], "%m/%M-%d")
	
	form <- (numSlashes >= 2)
	startDay[form] <- date2Jul(sDates[form], "%m/%d - %M/%S")
	endDay[form] <- date2Jul(sDates[form], "%S/%M - %m/%d")
	
	badSet <- (is.na(startDay) | is.na(endDay))
	if(any(badSet)){
		stop(paste("indecipherable dates:", paste(sDates[badSet], collapse = '\n')))
	}
	return(list(startDay, endDay))
	
}
getNumSymbol <- function(sin, format){
	
	vapply(gregexpr(format, sin), function(x){
		length(which(x>0))
	},1)
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
	farmStaInd <- c(as.integer(1), rev(rev(farmEndInd)[-1]) + 1)
	
	statDat <- t(mapply(function(beg, last){
		catches <- usefulEle[beg:last, 8]
		totCatch <- sum(catches)
		firstOccInd <- which(catches>0)[1]
		firstOcc <- usefulEle[(beg:last)[firstOccInd], 6]
		
		if(!all(usefulEle[beg:last, 1] == usefulEle[beg, 1])){
			stop(paste(
				"The subseting for",
				usefulEle[beg, 4],
				"messed up as they have different farmIDs"))
		}
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

#' Summarize the validation data
#'
#' @param trapIn Either the path to the pest watch dump,
#'  the csv of the scrubing result,
#'  or the list of trap records
#' @param year numerical year where the data should be extracted
#' @param hapDat The haplotype data as the path to the csv output of the scrubbing
#'  or the matrix of the csv data.
#' @param addDat additional data that you want to be included that must have the
#'  same row numbers as the original trap data. Either a vector or
#'  a list of vectors of the multiple pieces of data that you want to combine.
#' @param pathCsvOut the path of the csv that will be outputted
#' @param firstMethod How should the first Occurance be calculated? Either
#'  using the "real" method that takes the first time moths were actually captured,
#'  or the "pred" method that gets the first time the trap record could have traps in 
#'  in that grid cell as determined by the length of the catch period.
#'
#' @return outputs a matrix of the csv output or if pathCsvOut is specified, 
#'  works as a byproduct by writing a csvFile
#' @export
summarizeValid <- function(trapIn, year,
	hapDat = NULL,
	addDat = NULL,
	pathCsvOut = "",
	firstMethod = c("real", "pred")[1]) {
	
	days <- seq(1, 365, 7)
	
	trapDat <- switch(class(trapIn),
		character = if(grepl("[.]xlsx", trapIn)){
			scrubTrap(trapIn, year)
			
		} else {
			read.csv(trapIn, stringsAsFactors = FALSE)
		},
		trapIn)
	if(!(grepl("real", firstMethod) || grepl("pred", firstMethod))){
		stop(paste("summarizeValid requires a first Occurance method of either",
			"the first time a capture was reported (real) or",
			"the first time where moths could reside in the location",
			"based on the trap record and period of capture (pred),",
			"you provided", firstMethod)
		)
	}
	
	notes <- switch(class(trapDat),
		list = t(vapply(trapDat, function(rec){ cbind(rec$ID, rec$notes) }, rep("",2))),
		matrix = cbind(trapDat[,1], trapDat[,dim(trapDat)[2]])
	)
	
	trapXYZ <- switch(class(trapDat),
		list = t(vapply(trapDat, function(rec){
			switch(firstMethod,
				real = c(rec$latlon, findInterval(rec$firstOcc, days)),
				pred = c(rec$latlon, which(rec$capTSer > 0)[1])
			)
		},rep(.5, 3))),
		matrix = {
			realInd <- grep("firstOcc", colnames(trapDat))
			firstOcc <- switch(firstMethod,
				real = findInterval(trapDat[,realInd], days),
				pred = {
					dat <- trapDat[, (realInd + 1):(dim(trapDat)[2]-1)]
					vapply(1:dim(dat)[1], function(row){
						which(dat[row,] > 0)[1] 
					}, 1)
				}
			)
			cbind(trapDat[,2], trapDat[,3], firstOcc)
		}
	)
	sumDat <- trapXYZ
	
	#Haplotype data
	if(!is.null(hapDat)){
		hapRatio <- parseHapData(hapDat, 
			trapXYZ[,1],
			trapXYZ[,2],
			shAvgYear = TRUE)
		sumDat <- cbind(trapXYZ, hapRatio)
	}
	
	
	if(!is.null(addDat)){
		if(!is.list(addDat)) addDat <- list(addDat)
		
		for(dInd in 1:length(addDat)){
			numRec <- dim(sumDat)[1]
			numRecIn <- dim(addDat)[1]
			if(numRec != numRecIn){
				stop(paste("addDat number",
					dInd, "had", numRecIn, "records when it needed", numRec))
			}
			
			sumDat <- cbind(sumDat, addDat[[dInd]])
		}
	}
	
	sumDat <- cbind(notes[,1], sumDat, notes[,2])
	colnames(sumDat)[1:4] <- c(
		"ID",
		"Lat",
		"Lon",
		sprintf("First Week of capture (%s)", firstMethod))
	colnames(sumDat)[6] <- "notes"
	
	if(nzchar(pathCsvOut)){
		sumDat[,1] <- quoteIt(sumDat[,1])
		write.table(sumDat, file = pathCsvOut,
			sep = ',', row.names = FALSE, col.names = FALSE, quote = FALSE)
	}
	
	return(sumDat)
	
}

getTrapTimeSeries <- function(catches, jds, periods){
	
	notes <- c()
	#get rid of articifically low catch periods because of pestWatch limit of 14
	realPeriods <- (jds[-1] - rev(rev(jds)[-1]))
	realPeriods <- c(periods[1], realPeriods)
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
