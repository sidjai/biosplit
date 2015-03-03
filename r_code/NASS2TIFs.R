#! C:/Program Files/R/R-3.1.1/bin/x64/Rscript.exe

options(show.error.locations=TRUE)
require(httr)

#direc<-"C:/Users/Siddarta.Jairam/Downloads/Crop r/"
direc <- "C:/Users/Siddarta.Jairam/Documents/Crop Data/"

year <- 2011

xmin <- -1005735
#xmax <- 1794255
xmax <- 2274265
ymin <- 284055
ymax <- 2924055

dir.create(direc,showWarnings = FALSE)
direc <- paste0(direc,year,"/")
dir.create(direc,showWarnings = FALSE)
direc <- paste0(direc,'rawNASS',"/")
dir.create(direc,showWarnings = FALSE)





parseURL<- function(x){
	raw<-content(x,as = 'text')
	parse<-strsplit(raw,'returnURL>')
	text<-gsub('</','',parse[[1]][2])
	return(text)

}

startCDL <- 'http://nassgeodata.gmu.edu:8080/axis2/services/CDLService/GetCDLFile?year='
startCDL <- paste(startCDL,year, '&bbox=',xmin, sep="")

startExt <- 'http://nassgeodata.gmu.edu:8080/axis2/services/CDLService/ExtractCDLByValues?file='

spc <- 40000
kk<-1
#(ymin+(3*spc)
for (y in seq(ymin,(ymax-spc),by=spc)){
	print(toString(y/ymax))
	outName <-paste0(direc,year,'_Corn_',kk,'.tif')
	if (!file.exists(outName)){
		textCDL <- paste(startCDL,y,xmax,toString(y+spc),sep=",")
		cdlURL <- parseURL(GET(textCDL))
	
		textExt <- paste0(startExt,cdlURL,'&values=1')
		tifURL <- parseURL(GET(textExt))
		download.file(tifURL, destfile=outName, mode='wb')
	}
	
	
	
	kk<-kk+1
}