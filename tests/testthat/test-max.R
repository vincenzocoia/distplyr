test_that("cdf of max distribution makes sense.", {
  d_norm <- dst_norm(0, 1)
  d_pois <- dst_pois(1)
  x <- 1:10 / 5
  #' Expect all greater than
  #'
  #' For matching vector inputs, checks component-wise
  #' whether the object is greater than its expected
  #' counterpart.
  #' @param object,expected The two vectors to compare.
  #' @return Invisible; one call to `expect_gt()` per
  #' vector entry.
  expect_all_gt <- function(object, expected) {
    for (i in seq_along(object)) {
      expect_gt(object[i], expected[i])
    }
  }
  d_max <- maximise(d_norm, d_pois)
  expect_equal(eval_cdf(d_max, at = -0.01), 0)
  expect_all_gt(eval_cdf(d_norm, at = x), eval_cdf(d_max, at = x))
  expect_all_gt(eval_cdf(d_pois, at = x), eval_cdf(d_max, at = x))
  d_max2 <- maximise(d_pois, d_norm)
  expect_equal(eval_cdf(d_max, at = x), eval_cdf(d_max2, at = x))
  d_max3 <- maximise(d_norm, d_norm, d_pois)
  d_max3_nested <- maximise(d_max, d_norm)
  expect_equal(eval_cdf(d_max3, at = x), eval_cdf(d_max3_nested, at = x))
  expect_all_gt(eval_cdf(d_norm, at = x), eval_cdf(d_max3, at = x))
  expect_all_gt(eval_cdf(d_pois, at = x), eval_cdf(d_max3, at = x))
  d_max4 <- maximise(d_norm, d_pois, -1 - d_pois)
  expect_equal(eval_cdf(d_max, at = x), eval_cdf(d_max4, at = x))
})

test_that("pmf of max distribution makes sense.", {
  d_norm <- dst_norm(0, 1)
  d_pois <- dst_pois(1)
  d_max <- maximise(!!!rep(list(d_pois), 10))
  x <- 0:10
  expect_equal(
    eval_pmf(d_max, at = x),
    eval_cdf(d_max, at = x) - eval_cdf(d_max, at = x - 1)
  )
  d_max2 <- maximise(d_norm, d_pois)
  expect_equal(
    eval_pmf(d_max2, at = x, strict = FALSE),
    eval_cdf(d_max2, at = x) - eval_cdf(d_max2, at = x - 1e-6),
    tolerance = 1e-6
  )
})

test_that("density of max distribution makes sense.", {
  d_norm <- dst_norm(0, 1)
  d_exp <- dst_exp(1)
  d_max <- maximise(d_norm, d_exp)
  x <- 1:10 / 2
  eps <- 1e-6
  expect_equal(
    eval_density(d_max, at = x),
    (eval_cdf(d_max, at = x) - eval_cdf(d_max, at = x - eps)) / eps,
    tolerance = eps
  )
  expect_equal(eval_density(d_max, at = -(3:1)), rep(0, 3))
})

test_that("range of max works.", {
  d_exp <- dst_exp(1)
  d_norm <- dst_norm(0, 1)
  r1 <- range(maximise(d_norm, d_exp))
  expect_equal(r1, c(0, Inf))
  r2 <- range(maximise(d_norm, d_exp, -1 - d_exp))
  expect_equal(r2, c(0, Inf))
  r3 <- range(maximise(d_norm, d_norm))
  expect_equal(r3, c(-Inf, Inf))
})
