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
    raster::postMap(
      layer,
      opt$thing)
  }

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
