#'Load configuration parameters for the run, biological and simulation needs
#'
#'Parses the config file and returns the cfg with all the parameters
#'as an object, a .RData file and compiled text documents
#'Also creates all the directories as specified in the config files
#'
#'@param path The path to the config file if different than the package
#'	directory
#'@return a list of all the elements in the configuration file with their values
#'	Also sets up the directory trees for the data
#'	Saves the R object as a RData file and a second text file "cfg.txt" 
#'	for other languages
#'
#'@export
loadConfig <- function(path=""){
	path <- checkCfgPath(path)
	
	outTok <- gsub("\\w*.txt","",path)
	
	raw <- readLines(path)
	nice <- grep("[=]",raw,value=TRUE)
	nice <- gsub("^\\s+|\\s+$", "", nice)
	nice <- gsub("[\\]", "/", nice)
	nice <- nice[!grepl("[#]",nice)]
	
	both <- strsplit(nice," = ")
	firstHalf <- vapply(both,function(x) x[1],"e")
	secondHalf <- vapply(both,function(x) x[2],"e")
	
	#Initial overwinter populations
	
	winterSet <- grepl("Overwinter",firstHalf)
	FLset <- grepl("FL",firstHalf[winterSet])
	xygrid <- t(vapply(which(winterSet),function(x) as.numeric(strsplit(secondHalf[[x]],",")[[1]]),rep(1,2)))
	
	firstHalf <- firstHalf[!winterSet]
	secondHalf <- secondHalf[!winterSet]
	
	#ARL indv changes
	dirNum <- grep("Air|Soil|Wind",firstHalf)
	yearStr <- secondHalf[grep("year",firstHalf)]
	addFold <- c("Parsed","ParseAndExtract","Projected")
	addRight  <- vapply(dirNum,function(g){
		vapply(addFold,function(x) paste(secondHalf[g],yearStr,x,sep="/")
					 ,"e")
		},addFold,USE.NAMES=FALSE)
	addLeft <- gsub("Dir","Fold",firstHalf[dirNum])
	addComm <- vapply(1:length(addLeft),function(x){
		paste0("cfg$",addLeft[x]," <- ", "addRight[,",x,"]")
		},"e")
	
	#turn the strings into literal strings
	vecSet <- grepl("c\\(", secondHalf)
	digSet <- grepl("\\d", secondHalf)
	alpSet <- grepl("[:alpha:]", secondHalf)
	comdSet <- grepl("list\\(\\)", secondHalf)
	
	digAssSet <- grepl("\\d", firstHalf) & grepl("$", firstHalf)

	numSet <- (digSet & !alpSet) | vecSet | comdSet
	secondHalf[!numSet] <- paste0("\"",secondHalf[!numSet],"\"")
# 	firstHalf[nameListSet <- firstHalf[grep("\\d", listAssSet)]]
# 	regexpr('\\d+',test)[1]
						
	commands <- paste0("cfg$",firstHalf," <- ",secondHalf)
	commands <- c(commands,addComm)
	cfg <- list()
	eval(parse(text=commands))
	
	
	#Additional manipulations
	
	dirSet <- grepl("Dir",firstHalf) & !grepl("Direc|docu",firstHalf)
	
	baseFiles <- paste(secondHalf[dirSet],secondHalf[grep("year",firstHalf)],sep="/")
	
	baseLeft <- gsub("Dir","Fold",firstHalf[dirSet])
	
	#simulation out
	cfg$SimOutFold <- paste(cfg$SimOutDir, cfg$year, cfg$runName, sep="/")
	cfg$READMELoc <- paste(cfg$SimOutDir, cfg$year, "README.txt", sep="/")
	cfg$CropFold[1] <- paste(cfg$CropDir, cfg$year, "RawNASS", sep="/")
	cfg$CropFold[2] <- paste(cfg$CropDir, cfg$year, "Projected", sep="/")
	cfg$MetARLFold <- paste(cfg$MetARLDir, cfg$year,cfg$metDataType, sep="/")
	cfg$ncSliceFold <- paste(cfg$SimOutFold, "ncs", sep="/")
	cfg$rawHyPlotFold <- paste(cfg$SimOutFold, "HysplitPlots/", sep="/")
	cfg$AprioriLoc <- paste(cfg$AirTempDir, cfg$year, "aprioriVars.nc",sep="/")
	cfg$TrapLoc <- paste(cfg$docuDir, cfg$trapName, sep="/")
	names(cfg$TrapLoc) <- names(cfg$trapName)
	
	cfg$MetMappingLoc <- paste(cfg$MetARLDir, cfg$MetMappingLoc, sep="/")
	
	# make folders
	foldSet <- grep("Fold",names(cfg))
	
	for (y in foldSet){
		for(x in seq(1,length(cfg[[y]]))){
			dir.create(cfg[[y]][[x]],showWarnings = FALSE, recursive = TRUE)
		}
	}
	back <- paste(cfg,collapse=" \n")
	back <- strsplit(back," \n")[[1]]
	names(back) <- paste(names(cfg),"=")
	
	write.table(back,file=paste0(outTok,"cfg.txt"))
	save(cfg,file=paste0(outTok,"cfg.RData"))
	
	return(cfg)
}

#'Change the configuration file
#'
#'After the configuration path, there should be pairs of configuration 
#'elements that should be changed. The function loads up the file as lines and changes
#'those values then rewrites the entire file.
#'
#'@param path The path to the config file if different than the package directory
#'@param ...odd config element name as a string e.g. "runName", "year".
#'@param ...even the value specified. e.g. "runWorld", "2012".
#'
#'@examples
#'cfg <- loadConfig()
#'changeConfig(,'year',2014)
#'changeConfig(,'year',cfg$year,'runName',cfg$runName)
#'
#'\dontrun{
#'changeConfig(,'asdjkf',5)
#'#Error asdjkf are not valid variables in Config
#'}
#'@export
changeConfig <- function(path="",...){
	path <- checkCfgPath(path)
	
	args <- list(...)
	varNames <- c(args[seq(1,length(args),2)],recursive=TRUE)
	newVals <- args[seq(2,length(args),2)]
	
	befCon <- readLines(path)
	inds <- charmatch(varNames,befCon)
	invalidNames <- varNames[is.na(inds),drop=FALSE]
	if (length(invalidNames)>0){
		stop(paste(paste(invalidNames,collapse=','), "are not valid variables in Config"))
	}
	befCon[inds] <- paste(varNames,newVals,sep=' = ')
	writeLines(befCon,path)
}

#sees if the path exists and can be assessed
#looks at the package directory if no path is specified
checkCfgPath <- function(pth){
	if(!nzchar(pth)){
		pth <- paste(system.file(package="biosplit"), "config.txt", sep = '/')
	}
	testName <- c('does not exist',
								'cannot be written',
								'cannot be read')
	test <- (0 == vapply(c(0,2,4), function(x){
		file.access(pth, x)
	},0))
	if(!all(test)){
		stop(paste("file:", pth,'\n',
							 paste(testName[which(!test)], collapse = ' and \n')))
	} else {
		return(pth)
	}
}

	