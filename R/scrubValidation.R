#' Make the haplotype data easier to look at
#'
#' @param pathXlsx The path to the xlsx with the Haplotype data
#' @param pathTrapDict A csv with the translation of all the County names to the latLon location
#' @param pathCsvOut The csv location where the nice data should be outputed
#' @param maskBefore2011 should values before 2011 be used?
#' @param maskYr An array of the years after 2010 that you want to mask
#' @param maskNumMoth The minimum number of moths that are required for a proper Haplotype experiment
#'
#' @return The final matrix and a csv file that was written if pathCsvOut was specified
#' @export
scrubHaplo <- function(pathXlsx, pathTrapDict,
											 pathCsvOut ='',
											 maskBefore2011 = TRUE,
											 maskYr = c(),
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
		
		maskSet <- (maskSet | is.element(sheet[,1, drop = FALSE], maskYr))
		maskSet <- (maskSet | sheet[,9, drop = FALSE] < maskNumMoth)
		
		usefulEle <- as.list(sheet[!maskSet, ])
		
		
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
		
		splitSet <- grepl("[-]", inDates)
		
		if(length(which(splitSet))>0){
			hyphenLoc <- regexpr("[-]", inDates[splitSet])
			
			formatPos <- c("%b %d-%M %Y", "%b%d-", "%m/%d-%M ", "%m/%d-%M/%S","%b-")
			
			guessForm <- vapply(formatPos, function(x){
				if(grepl("%b-", x)){
					date2Jul(paste(1, inDates[splitSet]), paste("%d", x))
				} else {
					date2Jul(inDates[splitSet], x)
				}
				
			},which(splitSet))
			
			guessForm <- makeRowVec(guessForm)
			splitSeq <- 1:dim(guessForm)[1]
			
			rightInd <- vapply(splitSeq, function(x){
				which(!is.na(guessForm[x, ]))[1]
			},1)
			
			
			startDay[splitSet] <- guessForm[cbind(splitSeq, rightInd)]
			
			secondHalf <- substr(inDates[splitSet], hyphenLoc, nchar(inDates[splitSet]))
			
			jdSecond <- rightInd
			jdSecond[rightInd == 1] <- date2Jul(inDates[splitSet][rightInd==1], "%b %M-%d %Y")
			jdSecond[rightInd == 2] <- date2Jul(secondHalf[rightInd==2], "-%b%d")
			jdSecond[rightInd == 3] <- date2Jul(inDates[splitSet][rightInd==3], "%m/%M-%d ")
			jdSecond[rightInd == 4] <- date2Jul(inDates[splitSet][rightInd==4], "%S/%M-%m/%d")
			jdSecond[rightInd == 5] <- date2Jul(paste(1,secondHalf[rightInd==5]), "%d -%b")
			
			endDay[splitSet] <- jdSecond
	# 		endDay[splitSet] <- ifelse(rightInd == 1,
	# 			date2Jul(inDates[splitSet], "%b %M-%d %Y"),
	# 			ifelse(rightInd == 2,
	# 						 date2Jul(secondHalf, "-%b%d"),
	# 						 ifelse(rightInd == 3,
	# 						 			  date2Jul(inDates[splitSet], "%m/%M-%d"),
	# 						 			  date2Jul(secondHalf, "-%b")
	# 						 )
	# 			)
	# 		)
		}
		
		id <- cbind(locName, latLon)
		dat <- t(rbind(usefulEle$Year,startDay ,endDay ,usefulEle$total, usefulEle$`CS4/2`))
		totDat <- cbind(id, dat)

		outTab <- rbind(outTab, totDat)
	}
	names(outTab) <- NULL
	
	if(!nzchar(pathCsvOut)){
		write.csv(outTab, file = pathCsvOut)
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

scrubTrap <- function(pathXlsx, year, pestType = "faw", minTotCatch = 10){
	trapColType <- rep("numeric",10)
	trapColType[4:5] <- "text"
	trapColType[6] <- "date"
	trapColType[7] <- "text"
	trapColType[10] <- "text"
	
	sheet <- readxl::read_excel(pathXlsx, sheet = 1, col_types = trapColType)
	
	#basic data cutoff
	
	inYr <- as.integer(strftime(sheet[,6], "%Y"))
	
	badYearSet <- (inYr != year)
	badPestSet <- !grepl(pestType, sheet[,7])
	noCountySet <- is.na(sheet[,4])
	
	usefulEle <- sheet[!(badYearSet | badPestSet | noCountySet), -10]
	usefulEle[,6] <- as.integer(strftime(usefulEle[,6], "%j"))
	
	usefulEle <- usefulEle[order(usefulEle[,1], usefulEle[,6]),]
	
	current <- rev(rev(usefulEle$farmid)[-1])
	nextEle <- usefulEle$farmid[-1]
	
	farmEndInd <- which(current != nextEle)
	farmStaInd <- c(as.integer(1), rev(rev(farmInd)[-1]))
	
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
		
		
		newRec <- list(ID = statID[rind],
									 latlon = c(usefulEle[farmStaInd[rind],2],
									 					 -usefulEle[farmStaInd[rind],3]),
									 totCatch = statDat[rind, 1],
									 firstOcc = statDat[rind, 2],
									 capTSer = temp[[1]],
									 notes = temp[[2]]
		)
									 
									 
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
	
	
	return(invisible(records))
}

getTrapTimeSeries <- function(catches, jds, periods){
	days <- seq(8,365,7)
	notes <- "None"
	
	#get rid of articifically low catch periods because of pestWatch limit of 14
	
	#normalize to weekly captures
	catches <- round(catches * 7 / periods, 1)
	
	numWeeks <- ceiling(periods / 7)
	moreWeekSet <- (numWeeks > 1)
	catches <- c(catches, rep(catches[moreWeekSet], numWeeks[moreWeekSet]))
	jds <- c(jds, rep(jds[moreWeekSet], numWeeks[moreWeekSet]))
	
	catInd <- vapply(jds, function(x){
		which(days > x)[1]
	}, 1)
	
	tSer <- rep(0, 52)
	tSer[catInd] <- catches
	
	return(list(tSer, notes))
	
}
