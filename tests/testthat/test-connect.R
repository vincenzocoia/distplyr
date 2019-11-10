test_that("connecting works", {
	d1 <- dst_unif(0, 5)
	d2 <- dst_unif(0, 10)
	d  <- right_connect(d1, d2, sep_x = 3)
	expect_equal(fun_cumu(d, -1:11),
				 c(0, 0:2/5, seq(3/5, 1, length.out = 8), 1))
	expect_equal(fun_quant(d, seq(0, 1, length.out = 11)),
				 c(seq(0, 2.5, length.out = 6), seq(3, 10, length.out = 5)))
	expect_equal(fun_prob(d, -1:11),
				 c(0, rep(1/5, 4), rep(0.4/7, 7), 0))
})
