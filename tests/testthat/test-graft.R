d1 <- dst_unif(0, 5)
d2 <- dst_unif(0, 10)
d3 <- stepdst(1:5)
g1 <- graft_right(d1, d2, sep_y = 3)
g2 <- graft_right(d3, d1, sep_y = 3)
g3 <- graft_right(g2, d3, sep_y = 3) # Put original tail back
g4 <- graft_right(d1, g2, sep_y = 3)
g5 <- graft_right(d1, d3, sep_y = 3.5)

test_that("evaluation of representations works", {
	expect_equal(eval_cdf(g1, -1:11),
				 c(0, 0:2 / 5, seq(3 / 5, 1, length.out = 8), 1))
	expect_equal(eval_quantile(g1, seq(0, 1, length.out = 11)),
				 c(seq(0, 2.5, length.out = 6), seq(3, 10, length.out = 5)))
	expect_equal(eval_density(g1, -1:11),
				 c(0, rep(1 / 5, 4), rep(0.4 / 7, 7), 0))
	expect_null(eval_pmf(g1, -1:11))
})

test_that("variable determination works", {
	expect_identical(variable(g1), "continuous")
	expect_identical(variable(g2), "mixed")
	expect_identical(variable(g3), "discrete")
	expect_identical(variable(g4), "continuous")
	expect_null(get_pmf(g1))
	expect_null(get_pmf(g2))
	expect_null(get_density(g2))
	expect_null(get_density(g3))
	expect_null(get_pmf(g4))
	expect_identical(
		discontinuities(g2),
		data.frame(location = 1:3, size = 0.2)
	)
})

test_that("Re-grafting gets original distribution", {
	expect_identical(discontinuities(g3), discontinuities(d3))
	x <- seq(from = -2, to = 6, length.out = 100)
	expect_equal(eval_cdf(g4, at = x), eval_cdf(d1, at = x))
})

test_that("Sudden flattening of cdf does not 'trick' quantile function", {
	# plot(g5, "cdf", n = 1001) # Notice the sudden flattening at 3.5
	expect_identical(eval_quantile(g5, at = 0.7), 3.5)
})
