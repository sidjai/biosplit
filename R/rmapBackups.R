#' Make a diagonstic map for a layer
#'
#' A one layer map on top of a base map 
#' @param arr Anything that can be loaded in by 'raster' package
#' @param type The type of map that you want print
#' @param shNewMap Should the program grab a new map or use the exisiting one
#' @param pathSave The path that you want the saved jpeg to be in. Default is
#'   that the function does not save but just plot
#' @param ... additional parameters as explained in details.
#' 
#' @details The additional variables that can be passed are to the
#'   \code{\link[graphics]{contour}} from the graphics package through the
#'   raster package or \code{\link[graphics]{points}} for class post maps.
#'   Convience parameters for the levels are given as follows:
#' \describe{
#'   \item{levelThres}{Minimum amount shown in the plot}
#'   
#'   \item{levelLimit}{The max level shown on the map}
#'   \item{nLevels = 5}{The number of levels plotted on the map}
#'   \item{levelType = c("linear, log")[1]}{The type of spacing for the levels}
#' }
#'
#' @return A plot with 2 layers, the base map and the array
#' @export
makeDiagnosticMap <- function(
	arr,
	type = c("contour", "post")[1],
	shNewMap = TRUE,
	pathSave = "",
	...
	){

	ras <- parseLayerInput(arr)
	
	if(shNewMap){
		pl <- intwBaseMap(bbox = raster::extent(ras)[])
	} else {
		if(!existsPlot()){
			stop(paste("If you don't want to make a new map 'shNewMap = FALSE',",
				"then there has to be a map already present. There isn't one."))
		}
	}
	
	baseOpts <- intBaseOpt()
	myOpts <- combineOpts(baseOpts, list(...))

	

	pl <- do.call(
		addLayer,
		c(list(type = type, layer = ras), myOpts)
	)

	if(nzchar(pathSave)){
		dev.copy(jpeg, pathSave,
			units = "in",
			width = (par("pin")[1] + sum(par("mai")[c(2,4)])),
			height = (par("pin")[2] + sum(par("mai")[c(1,3)])),
			res = 100,
			quality = 100)
		dev.off()
	}
	return(invisible(0))
}

#' @import maps
intwBaseMap <- function(bbox){
	map("state", xlim = bbox[1:2], ylim = bbox[3:4], mar = c(4.1, 4.1, 0, 0))
	map.axes()
}

intBaseOpt <- function(){
	opt <- list(
		nLevels = 5
		)
}

intLevels <- function(
	levelThres,
	levelLimit,
	nLevels = 5,
	levelType = c("linear", "log")[1]
	){
	
	if(levelType == "log" && levelThres == 0) levelThres = 1

	return(switch(levelType,
		linear = seq(levelThres, levelLimit, length.out = nLevels),
		log = exp(seq(log(levelThres), log(levelLimit), length.out = nLevels)))
	)

}

combineOpts <- function(baseOpts, addOpts){
	pDups <- match(names(addOpts), names(baseOpts))
	if(length(pDups) > 0 && !all(is.na(pDups))){
		pDups[is.na(pDups)] <- NULL
		
		baseOpts[pDups] <- addOpts
		addOpts[!is.null(pDups)] <- NULL
	}
	return(c(baseOpts, addOpts))
}

addLayer <- function(type, layer, ...){
	check <- didProvideVar(vars = c("levelThres", "levelLimit"), ...)

	myLevels <- intLevels(
		levelThres = (if(check[1]) levelThres else cellStats(layer, stat='min')),
		levelLimit = (if(check[2]) levelLimit else cellStats(layer, stat='max')),
		...
	)

	if(type == "contour"){
		raster::contour(
			layer,
			levels = myLevels,
			add = TRUE,
			...)

	} else {
		posts <- postMaperize(layer, levels = myLevels)
		points(
			posts$pts,
			bg = points$color,
			pch = points$shape,
			add = TRUE)
	}

}

postMaperize <- function(
	layer,
	levels,
	classes = matrix(NA, nrow = 5, ncol = 3)
	){
	
	ptsSet <- (layer > thres)
	ptsMat <- which(ptsSet, arr.ind = TRUE)

	#Translate the x y coordinates

	colnames(classes) <- c("Begin Interval", "Symbol", "Color")
	badSet <- vapply(colnames(classes), function(x){
		is.na(classes[, x])
	}, TRUE)

	classes[, "Begin Interval"] <- levels

	classes[badSet[, "Symbol"], "Symbol"] <- c(1, 0, 2, 5, 11)[badSet[, "Symbol"]]

	classes[badSet[, "Color"], "Color"] <- rep("black", sum(badSet[, "Symbol"]))


	colMat <- getFromClass(ptsMat[, 3], classes[, "Color"])
	shapeMat <- getFromClass(ptsMat[, 3], classes[, "Shape"])

	return(list(
		pts = ptsMat,
		color = colMat,
		shape = shapeMat
	))
}

getFromClass <- function(pts, cvec){
	return(cvec[findInterval(pts, cvec)])
}

parseLayerInput <- function(mapin){
	fir <- tryCatch(
		raster::raster(mapin),
		finally = function(cond){
			error("Input cannot be read by raster with this error",
			cond, "/n")
		}

	)
}
