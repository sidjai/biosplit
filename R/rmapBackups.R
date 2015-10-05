intwBaseMap <- function(bbox = c()){
  plot <- usmap(bbox)
  return(plot)

}

intBaseOpt <- function(){
  opt <- list()
}

addLayer <- function(type, layer, opt){

  if(type = "contour"){
    raster::contour(
      layer,
      levels = opt$levels,
      add = TRUE)
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
    seq(bot, top, by = top/5)[badSet[, "Begin Interval"]}

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

saveMapJpg <- function(map, path){
  JPEG(map, path)
}

parseLayerInput <- function(mapin){
  fir <- raster(mapin)
  if(~is.raster(fir)){
    #do something damn it
  }
}

makeDiagnosticMap <- function(
  arr,
  type = c("contour", "post")[1],
  pathSave = ""
  ){

  ras <- parseLayerInput(arr)

  pl <- intwBaseMap()
  baseOpt <- intBaseOpt()


  pl <- addLayer(type, ras, opt)

  if(nzchar(save)){
    saveMapJpg(pl, save)
  }
  return(pl)
}
