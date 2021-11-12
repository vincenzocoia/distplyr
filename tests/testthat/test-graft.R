dat <- local({
	base <- distionary::dst_empirical(-2:2)
	norm <- distionary::dst_norm(0, 1)
	list(
		base = base,
		dri = graft_right(base, norm, breakpoint = 0, include = TRUE),
		dre = graft_right(base, norm, breakpoint = 0, include = FALSE),
		dli = graft_left(base, norm, breakpoint = 0, include = TRUE),
		dle = graft_left(base, norm, breakpoint = 0, include = FALSE)
	)
})

test_that("check cdf at breakpoint", {
	expect_equal(distionary::prob_left(dat$dle, of = 0, inclusive = TRUE), 0.6)
	expect_equal(distionary::prob_left(dat$dle, of = 0, inclusive = FALSE), 0.6)
	expect_equal(distionary::prob_left(dat$dli, of = 0, inclusive = TRUE), 0.6)
	expect_equal(distionary::prob_left(dat$dli, of = 0, inclusive = FALSE), 0.4)
	expect_equal(distionary::prob_left(dat$dre, of = 0, inclusive = TRUE), 0.4)
	expect_equal(distionary::prob_left(dat$dre, of = 0, inclusive = FALSE), 0.4)
	expect_equal(distionary::prob_left(dat$dri, of = 0, inclusive = TRUE), 0.6)
	expect_equal(distionary::prob_left(dat$dri, of = 0, inclusive = FALSE), 0.4)
})

test_that("check cdf in base", {
	x <- c(-2.5, -1.5, -0.5)
	expect_equal(
		distionary::eval_cdf(dat$dre, at = x),
		distionary::eval_cdf(dat$base, at = x)
	)
	expect_equal(
		distionary::eval_cdf(dat$dri, at = x),
		distionary::eval_cdf(dat$base, at = x)
	)
	expect_equal(
		distionary::eval_cdf(dat$dle, at = -x),
		distionary::eval_cdf(dat$base, at = -x)
	)
	expect_equal(
		distionary::eval_cdf(dat$dli, at = -x),
		distionary::eval_cdf(dat$base, at = -x)
	)
})

test_that("quantiles at breakpoint", {
	expect_equal(distionary::eval_quantile(dat$dri, at = 0.5), 0)
	expect_gt(distionary::eval_quantile(dat$dre, at = 0.5), 0)
	expect_equal(distionary::eval_quantile(dat$dli, at = 0.5), 0)
	expect_lt(distionary::eval_quantile(dat$dle, at = 0.5), 0)
})



# d1 <- dst_unif(0, 5)
# d2 <- dst_unif(0, 10)
# d3 <- dst_empirical(1:5)
# g1 <- graft_right(d1, d2, sep_y = 3)
# g2 <- graft_right(d3, d1, sep_y = 3)
# g3 <- graft_right(g2, d3, sep_y = 3) # Put original tail back
# g4 <- graft_right(d1, g2, sep_y = 3)
# g5 <- graft_right(d1, d3, sep_y = 3.5)
#
# test_that("evaluation of representations works", {
#   expect_equal(
#     eval_cdf(g1, -1:11),
#     c(0, 0:2 / 5, seq(3 / 5, 1, length.out = 8), 1)
#   )
#   expect_equal(
#     eval_quantile(g1, seq(0, 1, length.out = 11)),
#     c(seq(0, 2.5, length.out = 6), seq(3, 10, length.out = 5))
#   )
#   expect_equal(
#     eval_density(g1, -1:11),
#     c(0, rep(1 / 5, 4), rep(0.4 / 7, 7), 0)
#   )
#   expect_identical(eval_pmf(g1, -1:11), rep(0, 13))
# })
#
# test_that("variable determination works", {
#   expect_identical(variable(g1), "continuous")
#   expect_identical(variable(g2), "mixed")
#   expect_identical(variable(g3), "discrete")
#   expect_identical(variable(g4), "continuous")
#   expect_identical(
#     eval_pmf(g1, at = c(1, 1.5, 4)),
#     c(0, 0, 0)
#   )
#   expect_identical(
#     eval_pmf(g2, at = c(1, 1.5, 4)),
#     c(0.2, 0, 0)
#   )
#   expect_null(eval_density(g2, at = c(1, 1.5, 4)))
#   expect_null(get_density(g3))
#   expect_equal(
#     eval_pmf(g5, at = c(1, 1.5, 4)),
#     c(0, 0, (5 - 3.5) / 5 / 2)
#   )
#   expect_identical(
#     unclass(discontinuities(g2)),
#     unclass(data.frame(location = 1:3, size = 0.2))
#   )
# })
#
# test_that("Re-grafting gets original distribution", {
#   expect_identical(discontinuities(g3), discontinuities(d3))
#   x <- seq(from = -2, to = 6, length.out = 100)
#   expect_equal(eval_cdf(g4, at = x), eval_cdf(d1, at = x))
# })
#
# test_that("Sudden flattening of cdf does not 'trick' quantile function", {
#   # plot(g5, "cdf", n = 1001) # Notice the sudden flattening at 3.5
#   expect_identical(eval_quantile(g5, at = 0.7), 3.5)
# })

test_that(paste("A graft distribution is only ever a mixture of two distributions.",
				"Eventually, conditioning on a mixture distribution may be written",
				"as a mixture of conditionals, in which case a graft distribution",
				"may be a mixture of more than two distributions."), {
	d <- dst_norm(0, 1)
	g <- d %>%
		graft_right(d, breakpoint = 2) %>%
		graft_left(d, breakpoint = -2)
	expect_length(g$components$distributions, 2L)
})

rm('dat')
