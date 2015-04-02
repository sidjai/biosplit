rm(list=ls(all=TRUE))

#meteoLoc <- "C:/Program Files (x86)/MeteoInfo"
meteoLoc <- "C:/Users/Siddarta.Jairam/Desktop/sid/MeteoInfo1.2"
goldLoc <- "C:/Program Files/Golden Software/Surfer 12"
RLoc <- "C:/Program Files/R/R-3.1.1"

#realWd <- gsub("/r_code","",ifelse(grepl("ystem",getwd()),dirname(sys.frame(1)$ofile),getwd()))
realWd <- "C:/Users/Siddarta.Jairam/Documents/MothMigrationModel"

runScript <- function(file,args=""){
	langFlag <- switch(strsplit(file,"[.]")[[1]][2], R=1,BAS=2,py=3,-9999)
	if (langFlag==-9999){
		stop(paste("Code:",file, "| is not of a supported language"))
	}
		
	if (langFlag==1){
		#Rscripts
		scrDir <- paste(realWd,"r_code",sep="/")
		command <- paste("CD",scrDir)
		command[2] <- paste(
			paste0("\"",paste(RLoc,"bin","Rscript.exe",sep="/"),"\"")
			,file,args)
		
	} else if (langFlag==2){
		#Visual basic from Scripter
		scrDir <- paste(realWd,"surf_code",sep="/")
		command <- paste("CD",scrDir)
		command[2] <- paste(
			paste0("\"",paste(goldLoc,"Scripter","Scripter.exe",sep="/"),"\"")
			,"-x",file,args)
		#cat("scripterArgs.txt",paste(args,collapse="\n"))
		
	} else if (langFlag==3){
		#IronPython or Jython from MeteoInfo
		scrDir <- paste(realWd,"Python_code",sep="/")
		command <- paste("CD",scrDir)
		command[2] <- paste(
			paste0("\"",paste(meteoLoc,"meteoinfo.bat",sep="/"),"\"")
			,file,args)
		
	}
	if (!file.exists(paste(scrDir,file,sep="/"))){
		stop(paste("Code:",file, "| Does not exist"))
	}
		
	
	input <- paste(command,collapse=" && ")
	
	cat("\n","#######", paste("calling", file),"\n")
	cat(input)
	shell(input,mustWork=TRUE)
}

#Load configuration for this set

runScript("loadConfig.R")
load(paste(realWd,"cfg.Rout",sep="/"))

stop("Run stuff now")

#####Crop#####################################
#Crop data grab section
####


#Download the files directly from NASS cropscape
runScript("NASS2TIFs.R")


#convert the downloaded Tiff 30m blocks to 40 km blocks and get rid of projection
runScript("rawCrop2nicenc.R")


######ARL####################################
#ARL data grab section
####


#Go through the ARL data 
#Require MeteoInfo
runScript("ARL2GRD.py",paste(realWd,"cfg.txt",sep="/"))

#Change the extent of the data

runScript("rawMet2nicenc.R")




######Clean###################################
#Clean up and deriving section
####

#Transform this raw data into the derived variables used by the program
runScript("aprioriVars.R")

####Run####################################
# Run the model and iterate upon HYSPLIT
####


#Call the model
runScript("iterateHysplit.R")

#runScript("loadConfig.R")
#load(paste(realWd,"cfg.Rout",sep="/"))
####Post-Process####################################
# Make maps and JUMP ready tables for the model output
####

#Make the tables
runScript("ncdf2trapdata.R")


#Make the maps
#require scripter
runScript("Mothtxt2ClassPost.BAS",cfg$SimOutFold)

#require scripter
runScript("Mothtxt2contour.BAS",cfg$SimOutFold)


