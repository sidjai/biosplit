#'Load configuration parameters for the run, biological and simulation needs
#'
#'Parses the config file and returns the cfg with all the parameters
#'as an object, a .RData file and compiled text documents
#'
#'@param path The path to the config file if different than the package directory
#'
#'@export
loadConfig <- function(path="config.txt"){
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
	vecSet <- grepl("c\\(",secondHalf)
	digSet <- grepl("\\d",secondHalf)
	alpSet <- grepl("[:alpha:]",secondHalf)
	
	numSet <- (digSet & !alpSet) | vecSet
	secondHalf[!numSet] <- paste0("\"",secondHalf[!numSet],"\"")
	
	commands <- paste0("cfg$",firstHalf," <- ",secondHalf)
	commands <- c(commands,addComm)
	cfg <- list()
	eval(parse(text=commands))
	
	
	#Additional manipulations
	
	dirSet <- grepl("Dir",firstHalf) & !grepl("Direc|docu",firstHalf)
	
	baseFiles <- paste(secondHalf[dirSet],secondHalf[grep("year",firstHalf)],sep="/")
	
	baseLeft <- gsub("Dir","Fold",firstHalf[dirSet])
	
	#simulation out
	cfg$SimOutFold <- paste(cfg$SimOutDir,cfg$year,cfg$runName,sep="/")
	cfg$READMELoc <- paste(cfg$SimOutDir,cfg$year,"README.txt",sep="/")
	cfg$CropFold[1] <- paste(cfg$CropDir,cfg$year,"RawNASS",sep="/")
	cfg$CropFold[2] <- paste(cfg$CropDir,cfg$year,"Projected",sep="/")
	cfg$MetARLFold <- paste(cfg$MetARLDir,cfg$year,cfg$metDataType,sep="/")
	cfg$ncSliceFold <- paste(cfg$SimOutFold,"ncs",sep="/")
	cfg$rawHyPlotFold <- paste(cfg$SimOutFold,"HysplitPlots/", sep="/")
	cfg$AprioriLoc <- paste(cfg$AirTempDir,cfg$year,"aprioriVars.nc",sep="/")
	cfg$TrapLoc <- paste(cfg$docuDir,cfg$trapName,sep="/")
	
	
	
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
