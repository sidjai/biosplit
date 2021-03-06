parseFiles <- function(dirIn, exten = "", shReverse = FALSE){
	if(!dir.exists(dirIn)){
		stop(paste("Directory:", dirIn, "does not exist or can't be assessed."))
	}

	rawFiles <- list.files(dirIn,pattern = paste0(exten, '$') ,full.names = TRUE)

	if(length(rawFiles) < 1){
		stop(paste("Directory:", dirIn, "does not have any files with extension:", exten))
	}

	if (shReverse) rawFiles <- rev(rawFiles)
	return(rawFiles)
}

lappend <- function(lst, obj) {
	num<-length(obj)
	if (num>0){
		if (length(names(obj))!=0) obj <- list(obj)
		lst <- c(lst,obj)
	}
	return(lst)
}

getDate<- function(month, day, yr){
	tPos <- strptime(paste(month, day, yr),"%b %d %y")
	return(strftime(tPos, "%j"))
}

date2Jul <- function(strIn, format){
	posTime <- strptime(strIn, format)
	return(as.integer(strftime(posTime, "%j")))
}

jul2Week <- function(jd){
	days <- seq(1,365,7) + 1
	return(findInterval(as.numeric(jd), days))
}

file2year <- function(path){
	as.numeric(regmatches(path,regexpr("\\d{4}", path)))
}
zstr <- function(num, dig=2){
	str <- toString(num)
	while (nchar(str) < dig){
		str <- paste0("0", str)
	}
	return(str)
}

boundNumLine <- function(data, boundLow, boundHigh, inclusive = FALSE){
	if(inclusive){
		data[data >= boundHigh] <- boundHigh
		data[data <= boundLow] <- boundLow
	} else {
		data[data > boundHigh] <- boundHigh
		data[data < boundLow] <- boundLow
	}

	return(data)
}

isUnix <- function(){
	grepl("unix", .Platform$OS.type)
}

didProvideVar <- function(vars, getVar = FALSE, ...){
	lell <- list(...)
	check <- if(length(lell) == 0){
		FALSE
	} else {
		!is.na(match(vars, names(lell)))
	}
	
	if(getVar){
		return(lell[vars[check]])
	} else {
		return(check)
	}
}

existsPlot <- function(){
	check <- is.list(tryCatch(par(new = TRUE),
		warning = function(cond){
			!grepl("no plot", cond)
		}))
	return(check)
}

convDaysYr <- function(days, yr = 2038, timezone = "UTC"){
	secs <- (days * 24 * 60 * 60 + (7 * 60 + 12))
	tPos <- as.POSIXct(secs, origin = "2038-01-01", tz = "UTC")
	return(tPos)
}

allOsSystem <- function(cmd, ...){
	if(!isUnix()){
		cmd <- paste("C:/WINDOWS/system32/cmd.exe", "/c", cmd)
	}
	system(cmd, ...)
}
