test_that("range for normal dst works", {
  inf <- c(-Inf, Inf)
  expect_equal(range(dst_norm(0, 1)), inf)
  expect_equal(range(dst_norm(10000, 454)), inf)
  expect_equal(range(dst_norm(-565, 1321321312321)), inf)
  expect_equal(range(dst_norm(-1, 0.5)), inf)
})


test_that("range for uniform dst works", {
  expect_equal(range(dst_unif(0, 1)), c(0, 1))
  expect_equal(range(dst_unif(-165465, 654654645)), c(-165465, 654654645))
  expect_equal(range(dst_unif(0, 0.5)), c(0, 0.5))
  expect_equal(range(dst_unif(-0.5, 1.46546)), c(-0.5, 1.46546))
})

test_that("range for gpd dst works", {
  expect_equal(range(dst_gpd(0, 1, 46)), c(0, Inf))
  expect_equal(range(dst_gpd(-165465, 654654645, 1)), c(-165465, Inf))
  expect_equal(range(dst_gpd(0.2, 3, 1098098)), c(0.2, Inf))
  expect_equal(range(dst_gpd(-2.26, 3, 4)), c(-2.26, Inf))
  expect_equal(range(dst_gpd(0, 1, -46)), c(0, 1 / 46))
  expect_equal(range(dst_gpd(-2, 45.21, -35.23)), c(-2, -0.71671871))
  expect_equal(range(dst_gpd(0.265, 8, -4.54)), c(0.265, 2.02711453744))
  expect_equal(
    range(dst_gpd(-4.56, 5.48, -654)),
    c(-4.56, (-4.56 - (5.48 / -654)))
  )
})


test_that("range for degenerate dst works", {
  expect_equal(range(dst_degenerate(0)), c(0, 0))
  expect_equal(range(dst_degenerate(-165465)), c(-165465, -165465))
  expect_equal(range(dst_degenerate(0.2)), c(0.2, 0.2))
  expect_equal(range(dst_degenerate(-2.26)), c(-2.26, -2.26))
})

test_that("range for finite dst works", {
  expect_equal(range(dst_finite(1:5, probs = rep(0.2, 5))), c(1, 5))
  expect_equal(
    range(dst_finite(c(-1, 2, 654, 66, 5),
      probs = c(0.1, 0.3, 0.5, 0.05, 0.05)
    )),
    c(-1, 654)
  )
  expect_equal(
    range(dst_finite(c(-1, -2, -5.565, -1.1, 0),
      probs = rep(0.2, 5)
    )),
    c(-5.565, 0)
  )
})

test_that("range for empirical dst works", {
  expect_equal(range(dst_empirical(hp, data = mtcars)), c(52, 335))
  expect_equal(range(dst_empirical(speed, data = cars)), c(4, 25))
  expect_equal(range(dst_empirical(dist, data = cars)), c(2, 120))
})
