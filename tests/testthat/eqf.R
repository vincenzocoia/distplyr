x <- rnorm(10)
y <- rnorm(5)
p <- runif(5)

qdist <- eqf(x)
pdist <- ecdf(x)


test_that("eqf is vectorized", {
	expect_identical(
		sapply(p, qdist),
		qdist(p)
	)
})


test_that("eqf provides inverse of ecdf at original values", {
	expect_identical(x, qdist(pdist(x)))
})

test_that("special cases work as expected", {
	w_na <- eqf(x)
	wout_na <- eqf(x)
	expect_identical(w_na(p), wout_na(p)
	)
	expect_identical(c(0, 1), pdist(qdist(0:1)))
})
