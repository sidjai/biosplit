context("Test Hap Data scrubing")

badCases <- c("7/31-8/29 2014")

test_that("Resolve start > end", {
	lout <- parseSplitDate(badCases)
	
	mapply(function(st, en){
		expect_less_than(st, en)
	}, lout[[1]], lout[[2]])
	
})
