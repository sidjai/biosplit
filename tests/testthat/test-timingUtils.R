context("TimingUtils")

test_that("Weeks are counted correctly", {
	days <- seq(8,365,7)
	week <- jul2Week(days)
	expect_equal(week, 1:52)
})