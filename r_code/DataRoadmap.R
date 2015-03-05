rm(list=ls(all=TRUE))

meteoLoc <- "C:/Program Files (x86)/MeteoInfo"
goldLoc <- "C:/Program Files/Golden Software/Surfer 12"
realWd <- gsub("/r_code","",ifelse(grepl("System",getwd()),dirname(sys.frame(1)$ofile),getwd()))

runScript <- function(file,args=""){
	langFlag <- switch(strsplit(file,"[.]")[[1]][2], R=1,BAS=2,py=3,-9999)
	if (langFlag==-9999){
		stop(paste("Code:",file, "| is not of a supported language"))
	}
		
	if (langFlag==1){
		#Rscripts
		scrDir <- paste(getwd(),"r_code",sep="/")
		command <- paste("CD",scrDir)
		command[2] <- paste("Rscript",file,args)
		
	} else if (langFlag==2){
		#Visual basic from Scripter
		scrDir <- paste(getwd(),"surf_code",sep="/")
		command <- paste("CD",scrDir)
		command[2] <- paste(
			paste0("\"",paste(goldLoc,"Scripter","Scripter.exe",sep="/"),"\"")
			,"-x",file,args)
		#cat("scripterArgs.txt",paste(args,collapse="\n"))
		
	} else if (langFlag==3){
		#IronPython or Jython from MeteoInfo
		scrDir <- paste(getwd(),"Python_code",sep="/")
		command <- paste("CD",scrDir)
		command[2] <- paste(
			paste0("\"",paste(MeteoLoc,"MIScript.exe",sep="/"),"\"")
			,file,args)
		
	}
	if (!file.exists(paste(scrDir,file,sep="/"))){
		stop(paste("Code:",file, "| Does not exist"))
	}
		
	
	input <- paste(command,collapse=" && ")
	
	cat("\n","#######", paste("calling", file),"\n")
	#cat(input)
	shell(input)
}

#Load configuration for this set

runScript("loadConfig.R")
load(paste(realWd,"cfg.Rout",sep="/"))


#####Crop#####################################
#Crop data grab section
####


#Download the files directly from NASS cropscape
runScript("NASS2TIFs.R")

#convert the downloaded Tiff 30m blocks to 40 km blocks in .grd format
#Require Scripter
runScript("TIFFR2GRD.BAS")


######ARL####################################
#ARL data grab section
####


#Go through the ARL data 
#Require MeteoInfo
runScript("ARS2GRD.py")

#Change the extent of the data
#Require Scripter (port now this is ridonk)
runScript("ARSparsed2extgrid.BAS")




######Clean###################################
#Clean up and deriving section
####

#get the raw measured ncdf data for Corn and ARL
runScript("XYZ402Map.R")

#Transform this raw data into the derived variables used by the program
runScript("aprioriVars.R")

####Run####################################
# Run the model and iterate upon HYSPLIT
####


#Call the model
runScript("iterateHysplit.R")

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


