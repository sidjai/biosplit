#! C:/Program Files/R/R-3.1.1/bin/x64/Rscript.exe
rm(list=ls(all=TRUE))
realWd <- gsub("/r_code","",ifelse(grepl("System",getwd()),dirname(sys.frame(1)$ofile),getwd()))
cfgFile <- paste0(realWd,"/config.txt")

raw <- readLines(cfgFile)
nice <- grep("[=]",raw,value=TRUE)
nice <- gsub("^\\s+|\\s+$", "", nice)
nice <- nice[!grepl("[#]",nice)]

split <- strsplit(nice," = ")
firstHalf <- vapply(split,function(x) x[1],"e")
secondHalf <- vapply(split,function(x) x[2],"e")

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
	paste0("cfg$",addLeft[x]," <- ", "addRight[,",toString(x),"]")
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
cfg$SimOutFold <- paste(cfg$SimOutDir,toString(cfg$year),cfg$runName,sep="/")
cfg$READMELoc <- paste(cfg$SimOutDir,toString(cfg$year),"README.txt",sep="/")
cfg$CropFold[1] <- paste(cfg$CropDir,toString(cfg$year),"RawNASS",sep="/")
cfg$CropFold[2] <- paste(cfg$CropDir,toString(cfg$year),"Projected",sep="/")
cfg$MetARLFold <- paste(cfg$MetARLDir,toString(cfg$year),cfg$metDataType,sep="/")
cfg$ncSliceFold <- paste(cfg$SimOutFold,"ncs",sep="/")
cfg$rawHyPlotFold <- paste(cfg$SimOutFold,"HysplitPlots/", sep="/")
cfg$AprioriLoc <- paste(cfg$AirTempDir,toString(cfg$year),"aprioriVars.nc",sep="/")
cfg$TrapLoc <- paste(cfg$docuDir,cfg$trapName,sep="/")

# add amounts to input grids
cfg$Flnum <- length(which(FLset))
FLamt <- cfg$stAmount *((cfg$relAmtFL)/cfg$Flnum)
TXamt <- cfg$stAmount *((1-cfg$relAmtFL)/length(which(!FLset)))

grdAmt <- vapply(FLset,function(x){
	if(x){FLamt}
	else {TXamt}
},1)

intGrids <- cbind(xygrid,grdAmt,deparse.level = 0)


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

write.table(back,file=paste(realWd,"cfg.txt",sep="/"))
save(cfg,intGrids,file=paste(realWd,"cfg.Rout",sep="/"))