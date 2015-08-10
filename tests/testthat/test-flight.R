context("Moth flight functions")

map2block <- makeMapConverter(seq(.1, .5, by=.1),
	seq(.1, .5, by=.1),
	tol = .05)

ctx <- list(
	xs = 1,
	ys = 1,
	CornGDD = 90,
	infestThres = 1,
	infestLmt = 2000,
	flightPropBeforeSilk = 0.1,
	flightPropAfterSilk = 0.9
)

moth <- makeLife(1,cbind(.2,.2, 100), 4, 1, 5)

test_that("Right amount of Gen moths stay",{
	
	negctx <- lapply(1:4, function(x)(ctx))
	negctx[[1]]$CornGDD <- 0 #Barren wasteland so all should fly
	negctx[[2]]$CornGDD <- 11 #Corn is not silking so 90% should stay
	negctx[[3]]$CornGDD <- 15000 #Silking so have 10% stay
	negctx[[4]]$CornGDD <- 999999999 #Too ripe so everyone should go
	
	#Get rid of randomness just for the test
	amount <- c(NA, 90, 10, NA)
	mapply(function(mockctx, val){
		tStay <- with_mock(rpois = function(num, val){ rep(val, num) },
			willFly(moth, TRUE, mockctx)$stay
		)
		expect_equal(tStay$grid[3], val)
	}, negctx, amount)
})

test_that("Gen moths are spread right", {
	tSpread <- willFly(moth, TRUE, ctx)$mig
	expect_equal(length(tSpread), 7)
	
	expect_equal(vapply(tSpread, function(x){ x$grid[3] > 0 }, TRUE),
		rep(TRUE, 7))
})
