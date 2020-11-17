x <- c(4, 5, 6, 7, 80)
y <- c(TRUE, FALSE)

test_that("stats::sd is called on non-distributions", {
	expect_equal(sd(x), stats::sd(x))
	expect_equal(sd(y), stats::sd(y))
})
