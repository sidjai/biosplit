#' Make a diagonstic map for a layer
#'
#' A one layer map with the designated layer.
#' @param arr Anything that can be loaded in by 'raster' package
#' @param type The type of map that you want printe
#' @param pathSave The path that you want the saved jpeg to be in. Default is that the function does not save but just plot
#'
#' @return A plot with 2 layers, the base map and the array
#' @export
makeDiagnosticMap <- function(
	arr,
	type = c("contour", "post")[1],
	pathSave = ""
	){

	ras <- parseLayerInput(arr)

	pl <- intwBaseMap(bbox = raster::extent(ras)[])
	myOpt <- intBaseOpt()


	pl <- do.call(
		addLayer,
		c(list(type = type, layer = ras), myOpt)
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

addLayer <- function(type, layer, ...){

	if(type == "contour"){
		raster::contour(
			layer,
			add = TRUE,
			...)

	} else {
		posts <- postMaperize(layer, classes = opt$classes)
		points(
			posts$pts,
			bg = points$color,
			pch = points$shape,
			add = TRUE)
	}

}

postMaperize <- function(layer, classes = matrix(NA, nrow = 5, ncol = 3)){
	ptsSet <- (layer > thres)
	ptsMat <- which(ptsSet, arr.ind = TRUE)

	#Translate the x y coordinates

	colnames(classes) <- c("Begin Interval", "Symbol", "Color")
	badSet <- vapply(colnames(classes), function(x){
		is.na(classes[, x])
	}, TRUE)

	bot <- min(layers)
	top <- max(layers)
	classes[badSet[, "Begin Interval"], "Begin Interval"] <- {
		seq(bot, top, by = top/5)[badSet[, "Begin Interval"]]}

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
