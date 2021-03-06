updateCntList <- function(lst,name){
	lst[[name]] <- length(lst)+1
	return(lst)
}
file2year <- function(path){
	as.numeric(regmatches(path,regexpr("\\d{4}", path)))
}
dispVar <- function(var){
	cat(paste(deparse(substitute(var)),var,sep=' = '),'\n')
}
mosaicImage <- function(pathMat,
												labelx = '',
												labely = '',
												offsetx = .1, offsety = .1,
												marl = .1, marb = .1,
												leftAlignYaxis = FALSE,
                        onlyScale = FALSE,
												pathOut = ""){
	
	if(nzchar(pathOut)) jpeg(pathOut)
	pathMat <- as.matrix(pathMat)
	
	if(is.null(labelx)){
		marb <- marb - 0.1
		labelx <- ''
	} else {
		labelx <- cleanLabel(gsub("/", "\n", labelx))
		if(!nzchar(labelx[1])){
			if(is.null(colnames(pathMat))){
				labelx <- paste0('x',1:dim(pathMat)[2])
			} else {
				labelx <- colnames(pathMat)
			}
		}
	}
	
	if(is.null(labely)){
		marl <- marl - 0.1
		labely <- ''
	} else {
		labely <- cleanLabel(gsub("/", "\n", labely))
		if(!nzchar(labely[1])){
			if(is.null(rownames(pathMat))){
				labely <- paste0('y',1:dim(pathMat)[1])
			} else {
				labely <- rownames(pathMat)
			}
		}
	}
	
	
	xlen <- dim(pathMat)[2]
	ylen <- dim(pathMat)[1]
	
	testJpg <- jpeg::readJPEG(pathMat[1,1])
	xscale <- dim(testJpg)[2] / dim(testJpg)[1]
	yscale <- 1
	
	#Get bounding box for every grid cell
	xoffs <- ((1:xlen - 1) * -offsetx) + marl
	yoffs <- ((1:ylen - 1) * -offsety) + marb
	
	xmins <- xscale * (1:xlen - 1) + xoffs
	xmaxs <- xscale * (1:xlen) + xoffs
	ymins <- rev(yscale * (1:ylen - 1) + yoffs)
	ymaxs <- rev(yscale * (1:ylen) + yoffs)
	
	absMaxx <- rev(xmaxs)[1]
	absMaxy <- ymaxs[1]
	
	if(onlyScale){
		return(absMaxy / absMaxx)
	} else {
		
		#Empty plot
		par(mar=c(0,0,0,0))
		plot(1,
			xlim=c(0, absMaxx),
			ylim=c(0, absMaxy),
			type="n", bty="n", axes=FALSE
		)
		
		for(yi in 1:ylen){
			#Y axis labels
			if( leftAlignYaxis){
				text(x = 0, y = mean(c(ymins[yi], ymaxs[yi])), labels = labely[yi], pos = 4)
			} else {
				text(x = 0, y = mean(c(ymins[yi], ymaxs[yi])), labels = labely[yi])
			}
			
			for(xi in 1:xlen){
				path <- pathMat[yi,xi]
				
				if (length(path)>0 && file.exists(path)){
					#on non windows need to resize
					rasterImage(jpeg::readJPEG(path,native=TRUE),
											xleft=xmins[xi],ybottom=ymins[yi],
											xright=xmaxs[xi],ytop=ymaxs[yi],
											interpolate = FALSE)
				}
				
				
				if (yi==ylen){
					#X axis labels
					text(x = mean(c(xmins[xi], xmaxs[xi])), y = 0.01, labels=labelx[xi])
				}
			}
			
		}
	}
	if(nzchar(pathOut)) invisible(dev.off())
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

findDiff <- function(vecvars, labelCol, compareInd=1:length(vecvars)){
	first <- compareInd[1]
	cond <- strsplit(vecvars,';')
	diff <- matrix('Same values', nrow = 1, ncol = length(vecvars))
	colnames(diff) <- labelCol
	locCnt <- 1 
	for (var in seq(1,length(cond[[1]]))){
		allCond <- vapply(1:length(vecvars),function(x){
			if(is.element(x,compareInd)){
				cond[[x]][var]
			} else "blank"},'one')
		test <- allCond[compareInd] %in% cond[[first]][var]
		
		
		if(!min(test)){
			#There is a difference between the runs so output it
			if(grepl("=",allCond[[first]])){
				parse <- strsplit(allCond,' = ')
				diff <- rbind(diff,vapply(1:length(vecvars),function(x){
					if(is.element(x,compareInd)){
						parse[[x]][2]
					} else "blank"},'one'))
				nrname <- parse[[first]][1]
			} else {
				diff <- rbind(diff,allCond)
				nrname <- paste0("Starting Location",locCnt)
				locCnt <- locCnt+1
			}
			
			#add the rowname from the thing that is different
			
			rownames(diff)[dim(diff)[1]] <- nrname
			
			#diff <- paste0(diff,paste(allCond,collapse=' != '),'\n')
		}
	}
	
	if (dim(diff)[1]>1){ 
		diff <- diff[-1,,drop=FALSE]
	}
	return(diff)
}

cleanAllLabels <- function(mtin){
	for (d in 1:length(dim(mtin))){
		dimnames(mtin)[[d]] <- cleanLabel(dimnames(mtin)[[d]])
	}
	return(mtin)
}

cleanLabel <- function(sin){
	numSlashes <- length(which(gregexpr("[/]", sin)[[1]]>0))
	if(numSlashes>2){
		sin <- file2year(sin)
	} else {
		sin <- gsub('/', ' ', sin)
		sin <- gsub(".grd", '', sin)
		sin <- gsub(".kml", '', sin)
		sin <- gsub("run", '', sin)
	}
	
	return(sin)
}
