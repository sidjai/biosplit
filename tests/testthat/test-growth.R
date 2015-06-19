context("Growth functions for the model")

map2block <- makeMapConverter(seq(.1, .5, by=.1),
															seq(.1, .5, by=.1),
															tol = .05)

ctx <- list()

ctx$xs <- ctx$ys <- 1
ctx$CornGDD <- 90
ctx$infestLmt <- 10
ctx$lifeSpan <- 10
ctx$infestThres <- 1
ctx$eggsPerInfest <- 5
ctx$oviDay <- 1
ctx$capEggs <- 5

moth <- makeLife(1,cbind(.2,.2, 1), 4, 1, 5)

test_that("Moths die when they should", {
	
	
	tMoth <- growMoths(moth,ctx)[[1]]
	expect_more_than(tMoth$grid[1,3], 0)
	
	tMoth$daysOld <- 10+1
	tMoth <- growMoths(tMoth, ctx)[[1]]
	expect_equal(tMoth$grid[1,3], -9999)
	
})

test_that("Egg laying", {
	
	out <- growMoths(moth, ctx)[[2]]
	tMoth <- out[[1]]
	tEggs <- out[[2]]
	
	expect_equal(length(tEggs),1)
	expect_equal(tEggs$grid[1,-3], moth$grid[1,-3])
	expect_equal(tEggs$grid[1, 3], moth$grid[1, 3] * ctx$eggsPerInfest)
	
	expect_equal(tMoth$numEggs, moth$numEggs - ctx$eggsPerInfest)
	
})

test_that("No egg laying", {
	
	#change ctx to a bad case
	negctx <- lapply(1:5, function(x)(ctx))
	negctx[[1]]$CornGDD <- 110 #Corn is too ripe
	negctx[[2]]$CornGDD <- 9 #Corn is not even leafing
	negctx[[3]]$oviDay <- 500 #too young to lay
	negctx[[4]]$eggsPerInfest <- 10 #Not enough eggs to lay
	
	lapply(negctx, function(x){
		tEggs <- growMoths(moth, x)[[2]]
		expect_equal(length(tEggs),0)
	})
	
	
})
