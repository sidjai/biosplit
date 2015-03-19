
load("cfg.Rout")

rawlatlon <- as.matrix(read.table(paste(cfg$MetARLDir,"g212latlon.txt",sep="/")))
translate <- cbind(-rawlatlon[,4],rawlatlon[,3],1:dim(rawlatlon)[1])
xlen <- length(which(rawlatlon[,2]==1))
back <- seq(dim(rawlatlon)[1],0,-xlen)
out <- c(0,0,0)

for (rv in seq(2,length(back))){
	out <- rbind(out, translate[(back[rv]+1):(back[rv-1]),])
}

out <- out[-1,]
out <- cbind(out[,c(1,2)],1:dim(out)[1])
write.table(out,paste(cfg$MetARLDir,"edasGrid2.txt",sep="/"),quote=FALSE,row.names=FALSE,col.names=FALSE)
