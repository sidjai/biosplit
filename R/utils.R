parseFiles <- function(dirIn, exten = "", shReverse = FALSE){
	if(!dir.exists(dirIn)){
		stop(paste("Directory:", dirIn, "does not exist or can't be assessed."))
	}
	
	rawFiles <- list.files(dirIn,pattern = paste0(ext, '$') ,full.names = TRUE)
	
	if(length(rawFiles) < 1){
		stop(paste("Directory:", dirIn, "does not have any files with extension:", exten))
	}
	
	if (shReverse) rawFiles <- rev(rawFiles)
	return(rawFiles)
}

zstr <- function(num,dig=2){
	str <- toString(num)
	while (nchar(str) < dig){
		str <- paste("0", str, sep="")
	}
	return(str)
}
