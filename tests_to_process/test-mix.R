d1 <- dst_norm(0, 1)
d2 <- dst_gpd(location = 0, scale = 1, shape = 1)
d3 <- dst_empirical(1:5)
d4 <- dst_empirical(3:5)
m0 <- mix(d1, d2, weights = c(1, 0))
m1 <- mix(d1, d2, weights = c(0.4, 0.6))
m2 <- mix(d1, d3, weights = c(0.4, 0.6))
m3 <- mix(d3, d4, weights = c(0.4, 0.6))

test_that("variables are as expected", {
  expect_identical(m0, d1)
  expect_identical(variable(m1), "continuous")
  expect_identical(variable(m2), "mixed")
  expect_identical(variable(m3), "discrete")
})

test_that("computations are correct", {
  expect_identical(
    eval_density(m1, at = -1),
    dnorm(-1) * 0.4
  )
  expect_identical(
    eval_pmf(m1, at = c(-1, 1)),
    c(0, 0)
  )
  expect_identical(
    eval_pmf(m2, at = c(0.5, 1, 1.5)),
    c(0, 0.2 * 0.6, 0)
  )
  expect_identical(
    eval_pmf(m3, at = c(1, 2.5, 5)),
    c(0.2 * 0.4, 0, 0.2 * 0.4 + 0.6 / 3)
  )
  expect_identical(
    eval_cdf(m1, at = -1),
    pnorm(-1) * 0.4
  )
})
