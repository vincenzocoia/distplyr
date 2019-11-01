x <- rnorm(10)
y <- rnorm(5)
p <- runif(5)

.qdst <- eqf(x)
.pdst <- ecdf(x)


test_that("eqf is vectorized", {
	expect_identical(
		sapply(p, .qdst),
		.qdst(p)
	)
})


test_that("eqf provides inverse of ecdf at original values", {
	expect_identical(x, .qdst(.pdst(x)))
})

test_that("NAs are ignored", {
	w_na <- eqf(x)
	wout_na <- eqf(x)
	expect_identical(w_na(p), wout_na(p))
})


test_that("boundaries work as expected", {
	expect_identical(c(0, 1), .pdst(.qdst(0:1)))
})
