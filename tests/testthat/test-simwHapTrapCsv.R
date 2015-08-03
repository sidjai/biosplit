context("Test simulation post-processing")

test_that("Smoothing to multi-week Haplotype", {
	exm <- smoothSimtoHap(1:4, rep(1,4), 'avg')
	expect_equal(exm, rep(2.5, 4))
	
	exm <- smoothSimtoHap(1:6, c(1.5,1.2, rep(1,4)), 'avg')
	expect_equal(exm, c(1, 2, rep(mean(3:6), 4)))
	
	exm <- smoothSimtoHap(1:6, c(1.5, NA, rep(1,4)), 'avg')
	expect_equal(exm, c(1, 2, rep(mean(3:6), 4)))
	
	exm <- smoothSimtoHap(1:10, c(rep(1,4), NA, 1.3, rep(1,4)), 'avg')
	expect_equal(exm, c(rep(mean(1:4),4), 5, 6, rep(mean(7:10), 4)))
})
