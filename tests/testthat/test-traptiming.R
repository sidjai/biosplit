context("Test Trap data scrubing")

thing <- cbind(c(1,2,1,2,3,4,5),rep(10,7), rep(2,7))

jds <- c(16, 23, 55)
periods <- c(3, 3, 20)
catches <- c(10, 10, 10)

test_that("Time series gets rid of duplicates", {
	res <- sumDups(thing)
	expect_equal(sum(thing[,2]), sum(res[,2]))
	expect_equal(dim(thing)[1] -2 , dim(res)[1])
	expect_equal(4, res[1, 3 ])
	
	
})


test_that("Time series spliting up into weekly captures",{
	normMat <- normalizeWk(catches, jds, periods)
	expect_equal(sum(normMat[,2]), sum(catches))
	expect_equal(sum(normMat[,3]), sum(periods))




})