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
	
	scrDir <- paste(base, 
								 switch(ext, R="scripts", BAS="surfer", py="jython")
								 ,sep='/')
	if (!is.na(match(ext, 'R'))){
		scrDir <- "C:/Users/Siddarta.Jairam/Documents/MothMigrationModel/scripts"
	}
	
	driver <- if(langFlag == 1){
		#R scripts
		 quoteSt(paste(driver,"bin","Rscript.exe",sep="/"))
	} else if(langFlag ==2){
		paste(quoteSt(paste(driver,"Scripter","Scripter.exe",sep="/")), "-x")
	} else {
		quoteSt(paste(meteoLoc,"meteoinfo.bat",sep="/"))
	}
	
	return(function(scrName, args="",
									verbose = FALSE){
		
		if (!grepl(paste0('[.]', ext), scrName)){
			stop(paste("Code:", scrName, "| is not of a supported language"))
		}
		
		if (!file.exists(paste(scrDir, scrName, sep="/"))){
			stop(paste("Code:", scrName, "| Does not exist"))
		}
		
		command <- paste("CD", scrDir)
		
		command[2] <- paste(driver, scrName, paste(quoteSt(args, ext), collapse = " "))
		
		input <- paste(command,collapse=" && ")
		
		cat("\n","#######", paste("calling", scrName),"\n")
		if(verbose) cat(input)
		
		jk <- shell(input,mustWork=TRUE, intern = verbose)
		if(verbose) cat('\n',jk,'\n')
		
	})
}
quoteSt <- function(instr, tok){
	paste0("\"", instr, "\"")
}
