library(diagram)

realWd <- system.file(package = "biosplit")
# Make diagram
outFile <- paste(realWd,'docs','DataDiagram.jpg',sep='/')

arrowMat <- matrix(nrow = 10, ncol = 10, byrow = TRUE, data = 0)
arrowMat[2,1] <- "MeteoInfo"
arrowMat[3,2] <- "SaveasScripter"
arrowMat[4,3] <- "Rasterize"
arrowMat[9,4] <- "ncdf"

arrowMat[6,5] <- "httr: GET"
arrowMat[7,6] <- "Horizontal slices"
arrowMat[8,7] <- "rgdal"
arrowMat[9,8] <- "sum(extract(smallbox))"

arrowMat[10,9] <- "load ncdf"

position <- rbind(c(1,2.5),c(2,2.5),c(3,2.5),c(4,2.5),
									c(1,1.5),c(2,1.5),c(3,1.5),c(4,1.5),
									c(5,2),c(6,2)
									)
position[,1] <- position[,1]/max(position[,1])
position[,2] <- position[,2]/3
type <- c("round","rect","diamond","rect",
					"round","rect","diamond","rect",
					"rect","rect")
textbox <- c("ARLftp","ARL2GRD.py",".grd","rawMet2nicenc.R",
					"CropScape","NASS2TIFs.R",".tif","rawCrop2nicenc.R",
					"aproriVars.R","iterateHYSPLIT.R")
jpeg(outFile,width=1113,height=1200,quality=100)
plotmat(arrowMat, pos = position, name = textbox,
				box.type = type,latex=TRUE,
				dtext=.8, box.size=.05,cex=1,box.cex=1,box.prop=.5,my=-.2,mx=-.08,shadow.size=0)

dev.off()
system(outFile)
