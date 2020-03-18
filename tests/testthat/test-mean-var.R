x <- c(-1, 4, 5, -1, -2, 7)
.dst <- dst_emp(x)

test_that("mean and variance works with step function cdf's", {
	expect_identical(mean(x), mean(.dst))
	expect_identical(mean(x^2) - mean(x)^2, var(.dst))
})


x <- c(-1, 4, 5, -1, NA, -2, 7)
.dst <- dst_emp(x)

test_that("mean and variance works with step function cdf's -- with NA", {
	expect_identical(
		mean(x^2, na.rm = TRUE) - mean(x, na.rm = TRUE)^2,
		var(.dst)
	)
})
