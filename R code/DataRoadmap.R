rm(list=ls(all=TRUE))

runScript <- function(file,args=""){
	langFlag <- switch(strsplit(file,"[.]")[[1]][2], R=1,BAS=2,py=3,-9999)
	if (langFlag==-9999){
		stop(paste("Code:",file, "| is not of a supported language"))
	}
		
	if (langFlag==1){
		#Rscripts
		scrDir <- getwd()
		command <- paste("CD",scrDir)
		command[2] <- paste("Rscript",file,args)
		
	} else if (langFlag==2){
		#Visual basic from Scripter
		scrDir <- getwd()
		command <- paste("CD",scrDir)
		command[2] <- paste("Rscript",file,args)
		
	} else if (langFlag==3){
		#IronPython or Jython from MeteoInfo
		scrDir <- getwd()
		command <- paste("CD",scrDir)
		command[2] <- paste("Rscript",file,args)
		
	}
	if (!file.exists(paste(scrDir,file,sep="/"))){
		stop(paste("Code:",file, "| Does not exist"))
	}
		
	command <- paste("CD",getwd())
	command[2] <- paste("Rscript",file,args)
	input <- paste(command,collapse=" && ")
	
	cat("\n","#######", paste("calling", file),"\n")
	shell(input)
}

#Load configuration for this set

runScript("loadConfig.R")
load("cfg.Rout")


#####Crop#####################################
#Crop data grab section
####


#Download the files directly from NASS cropscape

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
runScript("Mothtxt2ClassPost.BAS")

#require scripter
runScript("Mothtxt2contour.BAS")


