context("Test Hap Data scrubing")

badCases <- c("7/31-8/29 2014")

firstMonth <- c("Jan-Feb 2011",
                "1/1-2/1 2011",
                "1/1-2/1/11",
                "Jan1-Feb1 2011", 
                "1/1-31 2011",
                "1/1-312011",
                "Jan1-Feb 2011",
                "Jan-Feb")
test_that("Resolve start > end", {
	lout <- parseSplitDate(badCases)
	
	mapply(function(st, en){
		expect_less_than(st, en)
	}, lout[[1]], lout[[2]])
	
})

test_that("Parse first month", {
	
	lapply(firstMonth,function(x){
		lout <- parseSplitDate(x)
		expect_equal(lout[[1]], 1)
		expect_more_than(lout[[2]],30)
		})
})
