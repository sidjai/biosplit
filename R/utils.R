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