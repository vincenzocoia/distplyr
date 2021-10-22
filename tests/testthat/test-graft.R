base <- dst_empirical(-2:2)
dri <- base %>%
	graft_right(dst_norm(0, 1), breakpoint = 0, include = TRUE)
dre <- base %>%
	graft_right(dst_norm(0, 1), breakpoint = 0, include = FALSE)
dli <- base %>%
	graft_left(dst_norm(0, 1), breakpoint = 0, include = TRUE)
dle <- base %>%
	graft_left(dst_norm(0, 1), breakpoint = 0, include = FALSE)
xri <- eval_quantile(dri, at = runif(10000))
xre <- eval_quantile(dre, at = runif(10000))
xli <- eval_quantile(dli, at = runif(10000))
xle <- eval_quantile(dle, at = runif(10000))
eri <- dst_empirical(xri)
ere <- dst_empirical(xre)
eli <- dst_empirical(xli)
ele <- dst_empirical(xle)
enframe_cdf(dri, eri, at = seq(-3, 3, length.out = 1000)) %>%
	pivot_longer(cols = !.arg, names_to = "distribution", values_to = "cdf") %>%
	ggplot(aes(.arg, cdf)) +
	geom_line(aes(group = distribution, colour = distribution), alpha = 0.5) +
	theme_minimal()
enframe_cdf(dre, ere, at = seq(-3, 3, length.out = 1000)) %>%
	pivot_longer(cols = !.arg, names_to = "distribution", values_to = "cdf") %>%
	ggplot(aes(.arg, cdf)) +
	geom_line(aes(group = distribution, colour = distribution), alpha = 0.5) +
	theme_minimal()
enframe_cdf(dli, eli, at = seq(-3, 3, length.out = 1000)) %>%
	pivot_longer(cols = !.arg, names_to = "distribution", values_to = "cdf") %>%
	ggplot(aes(.arg, cdf)) +
	geom_line(aes(group = distribution, colour = distribution), alpha = 0.5) +
	theme_minimal()
enframe_cdf(dle, ele, at = seq(-3, 3, length.out = 1000)) %>%
	pivot_longer(cols = !.arg, names_to = "distribution", values_to = "cdf") %>%
	ggplot(aes(.arg, cdf)) +
	geom_line(aes(group = distribution, colour = distribution), alpha = 0.5) +
	theme_minimal()

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
