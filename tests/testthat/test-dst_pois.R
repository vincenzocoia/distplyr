test_that("dst_pois basic functions work", {
  lambda <- 2
  .dst <- dst_pois(lambda = lambda)
  expect_identical(mean(.dst), lambda)
  expect_identical(variance(.dst), lambda)
  expect_identical(distplyr::sd(.dst), sqrt(lambda))
  expect_identical(skewness(.dst), lambda^(-0.5))
  expect_identical(kurtosis_exc(.dst), lambda^(-1))
  expect_identical(range(.dst), c(0, Inf))
})

test_that("dst_pois pmf and cdf functions works", {
  lambda <- 2
  .dst <- dst_pois(lambda = lambda)
  cdf_one <- stats::dpois(0, lambda) + stats::dpois(1, lambda)
  cdf_two <- cdf_one + stats::dpois(2, lambda)
  cdf_nine <- cdf_two + stats::dpois(3, lambda) + stats::dpois(4, lambda) +
    stats::dpois(5, lambda) + stats::dpois(6, lambda) +
    stats::dpois(7, lambda) + stats::dpois(8, lambda) + stats::dpois(9, lambda)
  expect_equal(eval_cdf(.dst, c(1, 2, 9)), c(cdf_one, cdf_two, cdf_nine))

  cdf_survival_one <- ppois(1, lambda, lower.tail = FALSE)
  cdf_survival_two <- ppois(2, lambda, lower.tail = FALSE)
  cdf_survival_nine <- ppois(9, lambda, lower.tail = FALSE)

  expect_equal(
    eval_survival(.dst, c(1, 2, 9)),
    c(cdf_survival_one, cdf_survival_two, cdf_survival_nine)
  )

  pmf_one <- ((exp(-lambda)) * (lambda^1)) / 1
  pmf_two <- ((exp(-lambda)) * (lambda^2)) / factorial(2)
  pmf_nine <- ((exp(-lambda)) * (lambda^9)) / factorial(9)

  expect_equal(eval_pmf(.dst, c(1, 2, 9)), c(pmf_one, pmf_two, pmf_nine))
})
