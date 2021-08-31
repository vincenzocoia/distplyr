test_that("Distribution quantities are appropriate", {
  d <- dst_degenerate(0.5)
  expect_equal(eval_pmf(d, -10:10), rep(0, 21))
  expect_equal(eval_pmf(d, 0.5), 1)
  expect_equal(eval_cdf(d, -10:10), c(rep(0, 11), rep(1, 10)))
  expect_equal(eval_cdf(d, 0.5), 1)
  expect_equal(realise(d, 10), rep(0.5, 10)) # had to change from the use of parameters
  expect_equal(eval_quantile(d, 1:9 / 10), rep(0.5, 9))
})