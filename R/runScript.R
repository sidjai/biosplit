#' A functionfactory for running scripts in this package
#' 
#' An OS agnostic way of calling a script from the command line given the
#' location of the program file. The scripts are either in the scripts folder or
#'  the individual languges. The py extension refers to the Jython scripts that 
#'  are used with MeteoInfo which needs to eb installed beforehand. The BAS 
#'  extension is used with Golden Software's Surfer's automation tool, scripter.
#'  
#'@param driver The file location of the root folder of the desired function.
#'@param ext The extension that the function is to be used with (py, BAS or R)
#'@export
makeRunFun <- function(driver, ext){
	langFlag <- switch(ext, R=1,BAS=2,py=3,-9999)
	if (langFlag == -9999){
		stop(paste("pkg doesn't have any", ext, "files"))
	}
	
	base <- system.file(package = "biosplit")
	
	scrDir <- paste(
		base,
		switch(ext, R="exec", BAS="surfer", py="jython"),
		sep = '/')
	
	driver <- if(langFlag == 1){
		#R scripts
		 quoteIt(paste(driver, "bin", "Rscript.exe", sep='/'))
	} else if(langFlag ==2){
		paste(quoteIt(paste(driver, "Scripter", "Scripter.exe", sep="/")), "-x")
	} else {
		if(isUnix()){
			sprintf("cd %s && ./%s -b", driver, "meteoinfo.sh")
		} else {
			quoteIt(paste(driver, "meteoinfo.bat", sep="/"))
		}
	}
	
	return(function(scrName, args="",
									verbose = 1){
		
		if (!grepl(paste0('[.]', ext), scrName)){
			stop(paste("Code:", scrName, "| is not a '", ext, "' file"))
		}
		
		if (!file.exists(paste(scrDir, scrName, sep="/"))){
			stop(paste("Code:", scrName, "| Does not exist"))
		}
		
		if(isUnix()){
			command <- sprintf("%s %s/%s %s",
				driver,
				scrDir,
				scrName,
				paste(quoteIt(args), collapse = " ")
			)
		} else {
			command <- paste("cd", scrDir)
			command[2] <- paste(driver, scrName, paste(quoteIt(args), collapse = " "))
		}
		
		input <- paste(command, collapse=" && ")
		
		if(verbose > 0){
			cat("\n","#######", paste("calling", scrName),"\n")
			
			if(verbose > 1) cat(input)
		}
		
		jk <- system(input, intern = verbose > 1)
		if(length(jk) == 1 && jk == -1L){
			stop(paste("Shell command:", input, "did not work"))
		}
		
		if(verbose > 1) cat('\n',jk,'\n')
	})
}
quoteIt <- function(instr){
	paste0("\"", instr, "\"")
}
