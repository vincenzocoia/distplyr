x <- c(-1, 4, 5, -1, -2, 7)
.dst <- stepdst(x)

test_that("mean and variance works with stepdst", {
	expect_equal(mean(x), get_mean(.dst))
	v <- mean(x^2) - mean(x)^2
	expect_equal(v, get_variance(.dst))
	expect_equal(sqrt(v), get_sd(.dst))
})


x <- c(-1, 4, 5, NA, -2, 7)
.dst <- stepdst(x)

test_that("mean and variance works with stepdst -- with NA and no duplicates", {
	expect_identical(
		mean(x^2, na.rm = TRUE) - mean(x, na.rm = TRUE)^2,
		get_variance(.dst)
	)
})
