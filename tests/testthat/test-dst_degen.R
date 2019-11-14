test_that("Distribution quantities are appropriate", {
	.dst <- dst_degen(0.5)
	expect_equal(fun_prob(.dst, -10:10), rep(0, 21))
	expect_equal(fun_prob(.dst, 0.5), 1)
	expect_equal(fun_cumu(.dst, -10:10), c(rep(0, 11), rep(1, 10)))
	expect_equal(fun_cumu(.dst, 0.5), 1)
	expect_equal(fun_rand(.dst, 10), rep(0.5, 10))
	expect_equal(fun_quant(.dst, 1:9/10), rep(0.5, 9))
})
