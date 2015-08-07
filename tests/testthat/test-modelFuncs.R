context("Key modeling functions")
lat <- 23:45
lon <- 30
test_that("Light calculations work with insol", {
	nightLen <- getNightDur(lat, lon, 200)
	expect_equal(is.na(nightLen), rep(FALSE, length(lat)))
	expect_equal(nightLen < 0, rep(FALSE, length(lat)))
	expect_equal(nightLen >24, rep(FALSE, length(lat)))
})
