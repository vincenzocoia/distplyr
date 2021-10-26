

test_that("check cdf at breakpoint", {
	expect_equal(prob_left(dle, of = 0, inclusive = TRUE), 0.6)
	expect_equal(prob_left(dle, of = 0, inclusive = FALSE), 0.6)
	expect_equal(prob_left(dli, of = 0, inclusive = TRUE), 0.6)
	expect_equal(prob_left(dli, of = 0, inclusive = FALSE), 0.4)
	expect_equal(prob_left(dre, of = 0, inclusive = TRUE), 0.4)
	expect_equal(prob_left(dre, of = 0, inclusive = FALSE), 0.4)
	expect_equal(prob_left(dri, of = 0, inclusive = TRUE), 0.6)
	expect_equal(prob_left(dri, of = 0, inclusive = FALSE), 0.4)
})

test_that("check cdf in base", {
	x <- c(-2.5, -1.5, -0.5)
	expect_equal(eval_cdf(dre, at = x), eval_cdf(base, at = x))
	expect_equal(eval_cdf(dri, at = x), eval_cdf(base, at = x))
	expect_equal(eval_cdf(dle, at = -x), eval_cdf(base, at = -x))
	expect_equal(eval_cdf(dli, at = -x), eval_cdf(base, at = -x))
})

test_that("quantiles at breakpoint", {
	expect_equal(eval_quantile(dri, at = 0.3), 0)
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
	g <- graft_left(graft_right(d, d, breakpoint = 2), d, breakpoint = -2)
	expect_length(g$components$distributions, 2L)
})


# d1 <- dst_empirical(1:10)
# d2 <- dst_unif(5, 10)
# d3 <- dst_unif(0, 5)
#
#
# g1 <- graft_right(d1, d2, breakpoint = 5, include = FALSE)
# g2 <- graft_right(d1, d2, breakpoint = 5, include = TRUE)
#
#
#
# test_that("graft_right on ", {
# 	expect_equal(
# 		eval_cdf(g1, at = 5),
# 		0.4
# 	)
# 	expect_equal(
# 		eval_cdf(g2, at = 5),
# 		0.5
# 	)
# })
#
# plot(g1, 'cdf')
# plot(g2, 'cdf', add = TRUE, col= 'red')
# abline(v = 5, col = 'blue', lty = 'dashed')
# legend(1, 0.9,
# 	   legend=c("g1 (not include)", "g2 (include)", 'breakpoint'),
# 	   col=c("black", "red", 'blue'), lty=c(1,1,2), cex=0.8)


# # slice off the whole d3
# g3 <- graft_left(d3, d1, breakpoint = 5, include = FALSE)
#
# # only retain the breakpoint in d3
# g4 <- graft_left(d3, d1, breakpoint = 5, include = TRUE)
#
# test_that("graft_left", {
# 	expect_equal(
# 		eval_cdf(g3, at = c(1:5)),
# 		eval_cdf(slice_right(d3, breakpoint = 5, include = TRUE), at = c(1:5))
# 	)
# 	expect_equal(
# 		eval_pmf(g4, at =5),
# 		0
# 	)
# })
#
#
# plot(g3, 'cdf')
# plot(g4, 'cdf', add = TRUE, col= 'red')
# abline(v = 5, col = 'blue', lty = 'dashed')
# legend(1, 0.9,
# 	   legend=c("g3 (not include)", "g4 (include)", 'breakpoint'),
# 	   col=c("black", "red", 'blue'), lty=c(1,1,2), cex=0.8)
