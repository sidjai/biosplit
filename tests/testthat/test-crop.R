context("Crop data processing")

inRas <- raster::raster(ncols = 10, nrows = 10, xmn = 0, xmx =10, ymn = 0, ymx = 10, vals = 0)
inRas[1,1] <- 1
addXY <- rbind(c(8.5, 9),
							 c(5, 5),
							 c(3.5, 3))
test_that("Crop Add does adds background", {
	testRas <- addCropArea(inRas, addXY, 1)
	expect_equal(sum(testRas[,]),4)
	
})
