#' Make a diagonstic map for a layer
#'
#' A one layer map on top of a base map
#' @param arr Anything that can be loaded in by 'raster' package
#' @param type The type of map that you want print
#' @param shNewMap Should the program grab a new map or use the exisiting one
#' @param shPath2Title Should the file provided be turned into a title for the 
#'   plot? Gives a warning if a path wasn't supplied. If given a string it will
#'   just use it as a label instead
#' @param shReturnInfo Should the information on the plotting (classes, levels
#'   and labels) be returned as a list?
#' @param shLegend Should a legend be ploted?
#' @param legendTitle The title for the legend
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
#'   \item{exLevels}{a numeric vector specifing the levels that were already
#'   formulated i.e they existed before}
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
	shReturnInfo = FALSE,
	shLegend = TRUE,
	legendTitle = "",
	pathSave = "",
	...
	){

	ncParams <- didProvideVar(vars = c("ncVar", "ncLayer"), getVar = TRUE, ...)
	#turn into proper names for ncdf package through raster
	names(ncParams) <- gsub("ncVar", "varname", 
		gsub("ncLayer", "band", 
			names(ncParams)))
	ras <- do.call(parseLayerInput, c(list(mapin = arr), ncParams))

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
	
	if(is.character(shPath2Title)){
		text(ras@extent@xmin, (ras@extent@ymin + .05/2 * sum(ras@extent[3:4])),
			labels = shPath2Title,
			pos = 4)
		shPath2Title <- FALSE
	}

	if(shPath2Title){

		if(is.character(arr) && file.exists(arr)){
			plotTitle <- gsub("_", " ", basename(arr))
			extStart <- regexpr("\\.[^\\.]*$", plotTitle)
			plotTitle <- substr(plotTitle, 1, (extStart - 1))
			if(length(ncParams) > 0){
				tNames <- gsub("varname", "var",
					gsub("band", "wk",
						names(ncParams)))
				
					text(ras@extent@xmin, (ras@extent@ymin + .05/2 * sum(ras@extent[3:4])),
						labels = paste(tNames, ncParams, sep = " = ", collapse = ", "),
						pos = 4)
			}

			title(plotTitle)

		} else {
			
			warning("Can't make title since input is not a file")
		}

	}



	info <- do.call(
		addLayer,
		c(list(type = type, layer = ras), myOpts)
	)

	if(shLegend) makeLegend(info$labels, info$classes, title = legendTitle)

	if(nzchar(pathSave)){
		dev.copy(jpeg, pathSave,
			units = "in",
			width = (par("pin")[1] + sum(par("mai")[c(2,4)])),
			height = (par("pin")[2] + sum(par("mai")[c(1,3)])),
			res = 100,
			quality = 100)
		dev.off()
	}

	if(shReturnInfo) return(info)
	else return(invisible(0))
}

#' @import maps
intwBaseMap <- function(bbox){
	usBox <- raster::extent(c(-125, -70, 25, 50))
	givBox <- raster::extent(bbox)
	intBox <- raster::intersect(usBox, givBox)
	if(is.null(intBox)){
		map("world", xlim = bbox[1:2], ylim = bbox[3:4], mar = c(4.1, 4.1, 1.1, 0))
		map.axes()
	} else {
		map("state", xlim = bbox[1:2], ylim = bbox[3:4], mar = c(4.1, 4.1, 1.1, 0))
		map.axes()
	}
}

intBaseOpt <- function(){
	opt <- list(
		nLevels = 5,
		lwd = 2,
		vfont = c("sans serif", "bold")
		)
}

intLevels <- function(
	levelThres,
	levelLimit,
	nLevels = 5,
	levelType = c("linear", "log")[1]
	){

	if(levelType == "log" && levelThres == 0) levelThres = 1
	numBBs <- nLevels + 1

	levels <- switch(levelType,
		linear = seq(levelThres, levelLimit, length.out = numBBs ),
		log = exp(seq(log(levelThres), log(levelLimit), length.out = numBBs)))

	return(levels)
}

intLabels <- function(lvls, labScientific = TRUE, labDigits = 2){
	labs <- signif(lvls, labDigits)
	labs <- format(labs, scientific = labScientific)
	labs <- gsub("\\+", "", gsub("\\+0", "", labs))
	return(labs)
}

makeLegend <- function(labels, classes, title = ""){
	entries <- paste(rev(rev(labels)[-1]), labels[-1], sep = " - ")
	colnames(classes) <- gsub("bg", "pt.bg", colnames(classes))
	
	opts <- c(list(x = "bottomright", legend = entries, bty = "n", cex = 0.6),
		classes[, -1])
	if(nzchar(title)) opts <- c(opts, list(title = title))

	do.call(legend, opts)
		
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
	levelVars <- c("levelThres", "levelLimit", "nLevels", "levelType")
	levelParams <- didProvideVar(vars = levelVars, getVar = TRUE, ...)

	if(!"levelThres" %in% names(levelParams)){
		levelParams$levelThres <- cellStats(layer, stat='min')
	}
	if(!"levelLimit" %in% names(levelParams)){
		levelParams$levelLimit <- cellStats(layer, stat='max') + 1
	}

	if(didProvideVar(vars = "exLevels", ...)){
		myLevels <- list(...)$exLevels
	} else {
		myLevels <- do.call(intLevels, levelParams)
	}
	ccol <- didProvideVar(vars = "classColor", getVar = TRUE, ...)
	ccol <- if(length(ccol) > 0) ccol[[1]] else "BW"

	myLabels <- do.call(intLabels, 
		c(list(lvls = myLevels), 
			didProvideVar(vars = c("labScientific", "labDigit"), getVar = TRUE, ...)))

	switch(type,
		contour = {

		myClasses <- intClasses(myLevels,
			symName = "lty",
			baseSymbols = 1,
			classColor = ccol)
		raster::contour(
			layer,
			levels = myLevels,
			labels = myLabels,
			col = myClasses$col,
			add = TRUE,
			...)

	}, filledContour = {
		myClasses <- intClasses(myLevels,
			symName = "lty",
			baseSymbols = 1,
			classColor = ccol)
		raster::filledContour(
			layer,
			levels = myLevels,
			col = myClasses$col,
			plot.axes = {intwBaseMap(bbox = raster::extent(layer)[])},
			frame.plot = FALSE)

	}, post = {
		myClasses <- intClasses(myLevels, colName = "bg", classColor = ccol)
		posts <- postMaperize(layer, levels = myLevels, classes = myClasses)
		points(
			posts$pts,
			bg = posts$color,
			pch = posts$shape)


	})

	return(list(
		classes = myClasses,
		levels = myLevels,
		labels = myLabels))

}

intClasses <- function(
	lvls,
	baseSymbols = 21:25,
	classColor = NA,
	symName = "pch",
	colName = "col"
	){

	numClasses <- length(lvls) - 1

	classes  <- as.data.frame(matrix(NA, nrow = numClasses, ncol = 3))
	colnames(classes) <- c("lvls", symName, colName)
	classes$lvls <- rev(rev(lvls)[-1])


	classes[, symName] <- rep(baseSymbols, length.out = numClasses)

	classes[, colName] <-
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

parseLayerInput <- function(mapin, ...){
	if(class(mapin) == "RasterLayer"){ out <- mapin
	} else {
		out <- tryCatch(
			raster::raster(mapin, ...),
			finally = function(cond){
				stop(paste("Input cannot be read by raster with this error",
				cond, sep = "/n"))
			}

		)

	}

	if(!raster::hasValues(out)){
		stop(paste("The input layer is empty ... see look",
			head(raster::rasterToPoints(out))))
	}

	return(out)
}
