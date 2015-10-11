#' Make a diagonstic map for a layer
#'
#' A one layer map on top of a base map
#' @param arr Anything that can be loaded in by 'raster' package
#' @param type The type of map that you want print
#' @param shNewMap Should the program grab a new map or use the exisiting one
#' @param shPath2Title Should the file provided be turned into a title for the plot? Gives a warning if a path wasn't supplied.
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
#'   \item{levelType = c("linear", "log")[1]}{The type of spacing for the levels}
#'   \item{classColor = NA}{The color specification for the graphs. It can be
#'   given in any of the three ways described in the "Color Specification' section}
#' }
#'@section Color specification options:
#'
#' \describe{
#'   \item{NA}{ Paint it all black }
#'   \item{"BW"}{ gray scale classes from white to black }
#'   \item{function}{ Any function that gives a character vector of color
#'   	specifications with a single required input of the number of colors
#'   	wanted. This means any number of packages can be used to specify color
#'   	with at most a simple wrapper function.}
#' }
#'
#' @return A plot with 2 layers, the base map and the array, or just the array
#'   if 'shNewMap = FALSE'
#' @export
makeDiagnosticMap <- function(
	arr,
	type = c("contour", "filledContour", "post")[1],
	shNewMap = TRUE,
	shPath2Title = FALSE,
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
	
	if(shPath2Title){
		
		if(is.character(arr) && file.exists(arr)){
			plotTitle <- gsub("_", " ", basename(arr))
			extStart <- regexpr("\\.[^\\.]*$", plotTitle)
			title(substr(plotTitle, 1, (extStart - 1)))
			
		} else {
			warning("Can't make title since input is not a file")
		}
		
	}



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
	map("state", xlim = bbox[1:2], ylim = bbox[3:4], mar = c(4.1, 4.1, 1.1, 0))
	map.axes()
}

intBaseOpt <- function(){
	opt <- list(
		numLevels = 5
		)
}

intLevels <- function(
	levelThres,
	levelLimit,
	numLevels = 5,
	levelType = c("linear", "log")[1]
	){

	if(levelType == "log" && levelThres == 0) levelThres = 1
	numBBs <- numLevels + 1

	levels <- switch(levelType,
		linear = seq(levelThres, levelLimit, length.out = numBBs ),
		log = exp(seq(log(levelThres), log(levelLimit), length.out = numBBs)))

	return(levels)
}

intLabels <- function(lvls, labScientific = TRUE, labDigits = 2){
	labs <- signif(lvls, labDigits)
	labs <- format(labs, scientific = labScientific)
	return(labs)
}

makeLegend <- function(labels, classes){
	entries <- paste(rev(rev(labels)[-1]), labels[-1], sep = " - ")
	colnames(classes) <- gsub("bg", "pt.bg", colnames(classes))

	do.call(legend,
		c(list(x = "bottomright", legend = entries, bty = "n", cex = 0.6),
			classes[, -1]))
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
	levelVars <- c("levelThres", "levelLimit", "numLevels", "levelType")
	levelParams <- didProvideVar(vars = levelVars, getVar = TRUE, ...)
	
	if(!"levelThres" %in% names(levelParams)){
		levelParams$levelThres <- cellStats(layer, stat='min')
	}
	if(!"levelLimit" %in% names(levelParams)){
		levelParams$levelLimit <- cellStats(layer, stat='max') + 1
	}

	myLevels <- do.call(intLevels, levelParams)

	switch(type,
		contour = {
		raster::contour(
			layer,
			levels = myLevels,
			add = TRUE,
			...)

	}, filledContour = {
		raster::filledContour(
			layer,
			levels = myLevels,
			add = TRUE,
			...)

	}, post = {
		ccol <- didProvideVar(vars = "classColor", getVar = TRUE, ...)
		ccol <- if(length(ccol) > 0) ccol[[1]] else "BW"
		
		myClasses <- intClasses(myLevels, classColor = ccol)
		colnames(myClasses)[3] <- "bg"
		posts <- postMaperize(layer, levels = myLevels, classes = myClasses)
		points(
			posts$pts,
			bg = posts$color,
			pch = posts$shape)

		makeLegend(intLabels(myLevels), myClasses)
	})

}

intClasses <- function(
	lvls,
	baseSymbols = 21:25,
	classColor = NA
	){
	
	numClasses <- length(lvls) - 1

	classes  <- as.data.frame(matrix(NA, nrow = numClasses, ncol = 3))
	colnames(classes) <- c("lvls", "pch", "col")
	classes$lvls <- rev(rev(lvls)[-1])
	
	
	classes$pch <- rep(baseSymbols, length.out = numClasses)
	
	classes$col <- 
		if(class(classColor) == "function"){
			classColor(numClasses)
		} else if(is.na(classColor)){
			rep("black", numClasses)
		} else if(classColor == "BW"){
			rev(gray.colors(numClasses))
		}
	
	return(classes)
}

postMaperize <- function(
	layer,
	levels,
	classes
	){

	ptsMat <- raster::rasterToPoints(layer, fun = function(x){
		x > levels[1] & x < levels[length(levels)]
	})

	indClass <- findInterval(ptsMat[, 3], levels)

	colMat <- classes$bg[indClass]
	shapeMat <- classes$pch[indClass]

	return(list(
		pts = ptsMat,
		color = colMat,
		shape = shapeMat
	))
}

parseLayerInput <- function(mapin){
	fir <- tryCatch(
		raster::raster(mapin),
		finally = function(cond){
			stop(paste("Input cannot be read by raster with this error",
			cond, sep = "/n"))
		}

	)
}
