#! C:/Program Files/R/R-3.1.1/bin/x64/Rscript.exe

rm(list=ls(all=TRUE))
cfgFile <- "config.txt"

raw <- readLines(cfgFile)
nice <- grep("[=]",raw,value=TRUE)
nice <- gsub("^\\s+|\\s+$", "", nice)
nice <- nice[!grepl("[#]",nice)]

split <- strsplit(nice," = ")
firstHalf <- vapply(split,function(x) x[1],"e")
secondHalf <- vapply(split,function(x) x[2],"e")

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
	paste0("cfg$",addLeft[x]," <- ", "addRight[,",toString(x),"]")
	},"e")

#turn the strings into literal strings
digSet  <- grepl("\\d",secondHalf)
alpSet  <- grepl("[:alpha:]",secondHalf)

numSet <- (digSet & !alpSet)
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
cfg$SimOutFold <- paste(cfg$SimOutDir,toString(cfg$year),cfg$runName,sep="/")
cfg$READMELoc <- paste(cfg$SimOutDir,toString(cfg$year),"README.txt",sep="/")
cfg$CropFold[1] <- paste(cfg$CropDir,toString(cfg$year),"RawNASS",sep="/")
cfg$CropFold[2] <- paste(cfg$CropDir,toString(cfg$year),"Projected",sep="/")
cfg$MetARLFold <- paste(cfg$MetARLDir,toString(cfg$year),cfg$metSimType,sep="/")
cfg$AprioriLoc <- paste(cfg$AirTempDir,toString(cfg$year),"aprioriVars.nc",sep="/")
cfg$TrapLoc <- paste(cfg$docuDir,cfg$trapName,sep="/")

save(cfg,file="cfg.Rout")