updateCntList <- function(lst,name){
	lst[[name]] <- length(lst)+1
	return(lst)
}
dispVar <- function(var){
	cat(paste(deparse(substitute(var)),var,sep=' = '),'\n')
}
mosaicImage <- function(pathMat,
												labelx=paste0('x',1:dim(pathMat)[2]),
												labely=paste0('x',1:dim(pathMat)[2]),
												offset=.1,marl=.1,marb=.1){
	xlen <- dim(pathMat)[2]
	ylen <- dim(pathMat)[1]
	#Empty plot
	par(mar=c(0,0,0,0))
	plot(1,xlim=c(0,xlen+offset+marl),ylim=c(0,ylen+offset+marb),
			 type="n",bty="n",axes=FALSE)
	
	#Get bounding box for every grid cell
	
	xmins <- (1:xlen)-1+marl
	xmaxs <- (1:xlen)+offset+marl
	ymins <- (ylen-(1:ylen))+marb
	ymaxs <- (ylen-(1:ylen))+1+marb+offset
	
	
	for(yi in 1:ylen){
		text(x=0,y=ylen-yi+.55,labels=labely[yi])
		for(xi in 1:xlen){
			path <- pathMat[yi,xi]
			
			if (length(path)>0 && file.exists(path)){
				#on non windows need to resize
				rasterImage(readJPEG(path,native=TRUE),
										xleft=xmins[xi],ybottom=ymins[yi],
										xright=xmaxs[xi],ytop=ymaxs[yi],
										interpolate = FALSE)
			}
			
			
			if (yi==ylen){
				text(x=xi-.35,y=0,labels=labelx[xi])
			}
		}
		
	}
}

getNiceTheme <- function(size=20,font="serif"){
	require(ggplot2)
	niceTh <- theme_bw(size,base_family = font)
	niceTh <- niceTh + theme(axis.title.y = element_text(vjust=0.7))
	niceTh <- niceTh + theme(panel.border = element_rect(size=1.5,colour='black'))
	niceTh <- niceTh + theme(axis.ticks = element_line(size = 1.25))
	niceTh <- niceTh + theme(panel.grid.minor = element_line(colour='white'))
	return(niceTh)
}

grabFunc <- function(path,funcName,returnFun=1,shouldCat=0,atStart=1){
	code <- parse(path)
	pattern <- funcName
	if (atStart) pattern <- paste0('^',funcName)
	comd <- grep(pattern,code,value=TRUE)
	
	if (shouldCat) cat(gsub(', ifelse',', \n\t\t\t ifelse',comd))
	
	if(returnFun){
		return(eval(parse(text=comd)))
	} else {
		return(comd)
	}
}
