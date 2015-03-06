#! C:/Program Files/R/R-3.1.1/bin/x64/Rscript.exe
realWd <- gsub("/r_code","",ifelse(grepl("ystem",getwd()),dirname(sys.frame(1)$ofile),getwd()))
load(paste(realWd,"cfg.Rout",sep="/"))

require(httr)


parseURL<- function(x){
	raw<-content(x,as = 'text')
	parse<-strsplit(raw,'returnURL>')
	text<-gsub('</','',parse[[1]][2])
	return(text)

}

startCDL <- 'http://nassgeodata.gmu.edu:8080/axis2/services/CDLService/GetCDLFile?year='
startCDL <- paste0(startCDL,cfg$year, '&bbox=',cfg$xmin)

startExt <- 'http://nassgeodata.gmu.edu:8080/axis2/services/CDLService/ExtractCDLByValues?file='

kk<-1
#(ymin+(3*spc)
for (y in seq(cfg$ymin,(cfg$ymax-cfg$spc),by=cfg$spc)){
	cat(y/cfg$ymax,"\n")
	outName <-paste0(cfg$CropFold[1],"/",cfg$year,'_Corn_',kk,'.tif')
	if (!file.exists(outName)){
		textCDL <- paste(startCDL,y,cfg$xmax,toString(y+cfg$spc),sep=",")
		cdlURL <- parseURL(GET(textCDL))
	
		textExt <- paste0(startExt,cdlURL,'&values=1')
		tifURL <- parseURL(GET(textExt))
		download.file(tifURL, destfile=outName, mode='wb')
	}
	kk<-kk+1
}