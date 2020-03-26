d1 <- dst_unif(0, 5)
d2 <- dst_unif(0, 10)
d  <- graft_right(d1, d2, sep_y = 3)

test_that("connecting works", {
	expect_equal(eval_cdf(d, -1:11),
				 c(0, 0:2/5, seq(3/5, 1, length.out = 8), 1))
	expect_equal(eval_quantile(d, seq(0, 1, length.out = 11)),
				 c(seq(0, 2.5, length.out = 6), seq(3, 10, length.out = 5)))
	expect_equal(eval_probfn(d, -1:11),
				 c(0, rep(1/5, 4), rep(0.4/7, 7), 0))
})
