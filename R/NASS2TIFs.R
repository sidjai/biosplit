#! C:/Program Files/R/R-3.1.1/bin/x64/Rscript.exe
rm(list=ls(all=TRUE))
realWd <- gsub("/r_code","",ifelse(grepl("ystem",getwd()),dirname(sys.frame(1)$ofile),getwd()))
load(paste(realWd,"cfg.Rout",sep="/"))

require(httr)


parseURL<- function(x){
	raw<-content(x,as = 'text')
	parse<-strsplit(raw,'returnURL>')
	text<-gsub('</','',parse[[1]][2])
	return(text)

}
zstr <- function(num,dig=2){
	str <- toString(num)
	while (nchar(str)<dig){
		str <- paste("0",str,sep="")
	}
	return(str)
}
startCDL <- 'http://nassgeodata.gmu.edu:8080/axis2/services/CDLService/GetCDLFile?year='
startCDL <- paste0(startCDL,cfg$year, '&bbox=',cfg$xmin)

startExt <- 'http://nassgeodata.gmu.edu:8080/axis2/services/CDLService/ExtractCDLByValues?file='

ys <- seq(cfg$ymin,(cfg$ymax),by=cfg$spc)
lasty <- length(ys)
bots <- rev(rev(ys)[-1])

tops <- ys[-1]

#(ymin+(3*spc)
for (ind in seq(1,lasty)){
	cat(ind/lasty,"\n")
	outName <-paste0(cfg$CropFold[1],"/",cfg$year,'_Corn_',zstr(ind),'.tif')
	if (!file.exists(outName)){
		textCDL <- paste(startCDL,toString(bots[ind]),cfg$xmax,toString(tops[ind]),sep=",")
		cdlURL <- parseURL(GET(textCDL))
	
		textExt <- paste0(startExt,cdlURL,'&values=1')
		tifURL <- parseURL(GET(textExt))
		download.file(tifURL, destfile=outName, mode='wb')
	}
}