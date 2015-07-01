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
