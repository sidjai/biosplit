context("Key modeling functions")
lat <- 23:45
lon <- 30
test_that("Light calculations work with insol", {
	nightLen <- getNightDur(lat, lon, 200)
	expect_equal(is.na(nightLen), rep(FALSE, length(lat)))
	expect_equal(nightLen < 0, rep(FALSE, length(lat)))
	expect_equal(nightLen >24, rep(FALSE, length(lat)))
})

test_that("cleanGrid cleans when it needs to", {
	testGrids <- list(
		list(grid = cbind(1,1,NA)),
		list(grid = rbind(c(1,1,-9999), c(1, 1, 4))),
		list(grid = rbind(c(1,1,0.5), c(1, 1, 4), c(1, 1, 3))),
		list(grid = rbind(c(1,1, 2), c(1, 1, 4), c(1, 1, 3)))
	)
	
	resList <- lapply(testGrids, function(x){
		cleanGrid(x)$grid[,3]
	})
	
	expect_equal(length(resList[[1]]), 0)
	expect_equal(resList[[2]], 4)
	expect_equal(resList[[3]], c(4.5, 3))
	expect_equal(resList[[4]], c(2, 4, 3))
	
	
})
