test_that("exponential transform works", {
	d1 <- dst_unif()
	d2 <- rv_transform(d1, g = exp, ginv = log, gprime = exp)
	expect_equal(fun_cumu(d2, c(0, 1, 2, exp(1), 3)),
				 c(0, 0, log(2), 1, 1))
	expect_equal(fun_quant(d2, 0:5/5), exp(0:5/5))
	expect_equal(fun_quant(d2, c(-1, 2)), c(NaN, NaN))
	expect_equal(fun_prob(d2, c(-1, 1, 2, exp(1), 3)),
				 c(0, 1/1, 1/2, 1/exp(1), 0))
})
