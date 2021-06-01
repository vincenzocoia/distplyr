test_that("dst_pois basic functions work", {
  lambda <- 2
  .dst <- dst_pois(lambda = lambda)
  expect_identical(mean(.dst), lambda)
  expect_identical(variance(.dst), lambda)
  expect_identical(distplyr::sd(.dst), sqrt(lambda))
})

test_that("dst_pois cdf functions works", {
  lambda <- 2
  .dst <- dst_pois(lambda = lambda)
  cdf_one <- stats::dpois(0, 2) + stats::dpois(1, 2)
  cdf_two <- cdf_one + stats::dpois(2, 2)
  cdf_nine <- cdf_two + stats::dpois(3, 2) + stats::dpois(4, 2) +
    stats::dpois(5, 2) + stats::dpois(6, 2) +
    stats::dpois(7, 2) + stats::dpois(8, 2) + stats::dpois(9, 2)
  expect_equal(eval_cdf(.dst, c(1, 2, 9)), c(cdf_one, cdf_two, cdf_nine))

  cdf_survival_one <- ppois(1, 2, lower.tail = FALSE)
  cdf_survival_two <- ppois(2, 2, lower.tail = FALSE)
  cdf_survival_nine <- ppois(9, 2, lower.tail = FALSE)

  expect_equal(
    eval_survival(.dst, c(1, 2, 9)),
    c(cdf_survival_one, cdf_survival_two, cdf_survival_nine)
  )

  expect_equal()
})