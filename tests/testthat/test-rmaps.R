context("Base R map Utilities")

test_that("options can be combined when one is empty",{
	bOpts <- list(thing = 1, other = 3)
	oOpts <- list()
	
	result <- combineOpts(bOpts, oOpts)
	
	expect_equal(length(result), 2)
	expect_equal(result$thing, 1)
	
	result <- combineOpts(oOpts, bOpts)
	expect_equal(length(result), 2)
	expect_equal(result$thing, 1)
	
})

test_that("options can be combined when both aren't empty", {
	bOpts <- list(thing = 1, other = 3)
	oOpts <- list(thing = 4)
	result <- combineOpts(bOpts, oOpts)
	
	expect_equal(length(result), 2)
	expect_equal(result$thing, 4)
	
	oOpts <- list(th = 4)
	result <- combineOpts(bOpts, oOpts)
	
	expect_equal(length(result), 3)
	expect_equal(result$thing, 1)
})
