test_that("next_discrete norm works", {
  expect_identical(NaN, next_discrete(dst_norm(1, 2), 1))
  expect_identical(NaN, next_discrete(dst_norm(12, 1), -2))
  expect_identical(NaN, next_discrete(dst_norm(65, 1), 00))
})

test_that("next_discrete gpd works", {
  expect_identical(NaN, next_discrete(dst_gpd(1, 2, -4), 1))
  expect_identical(NaN, next_discrete(dst_gpd(1, 1, 2), 1546))
  expect_identical(NaN, next_discrete(dst_gpd(561, 1, 8), -565))
})

test_that("next_discrete unif works", {
  expect_identical(NaN, next_discrete(dst_unif(1, 2), 1))
  expect_identical(NaN, next_discrete(dst_unif(-15, 0), -6))
  expect_identical(NaN, next_discrete(dst_unif(561, 18000), -99))
})

test_that("next_discrete pois works", {
  expect_identical(
    c(2),
    next_discrete(dst_pois(1), 1)
  )
  expect_identical(
    c(0),
    next_discrete(dst_pois(5456), -2)
  )
  expect_identical(
    c(56),
    next_discrete(dst_pois(561), 55.1545645)
  )
  expect_identical(
    c(1),
    next_discrete(dst_pois(2), 0.465)
  )
  expect_identical(
    c(0),
    next_discrete(dst_pois(55), -1.465)
  )
})

test_that("next_discrete degenerate works", {
  expect_identical(NaN, next_discrete(dst_degenerate(1), 1))
  expect_identical(c(-15), next_discrete(dst_degenerate(-15), -16))
  expect_identical(NaN, next_discrete(dst_degenerate(56), 80))
  expect_identical(c(56), next_discrete(dst_degenerate(56), 16))
})


test_that("next_discrete lnorm works", {
  expect_identical(NaN, next_discrete(dst_lnorm(1, 2), 1))
  expect_identical(NaN, next_discrete(dst_lnorm(-15, 2), -16))
  expect_identical(NaN, next_discrete(dst_lnorm(2, 3), 80))
  expect_identical(NaN, next_discrete(dst_lnorm(2, 2), 16))
})

car <- data.frame(
  hp = c(
    110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123,
    180, 180, 180, 205, 215, 230, 66, 52, 65, 97, 150,
    150, 245, 175, 66, 91, 113, 264, 175, 335, 109
  )
)

test_that("next_discrete finite works", {
  expect_equal(
    c(52),
    next_discrete(dst_empirical(hp, data = car), 1)
  )
  expect_equal(
    c(97),
    next_discrete(dst_empirical(hp, data = car), 95)
  )
  expect_equal(
    NaN,
    next_discrete(dst_empirical(hp, data = car), 335)
  )
  expect_equal(
    c(97),
    next_discrete(dst_empirical(hp, data = car), 95.4654)
  )
  expect_equal(
    c(1),
    next_discrete(dst_finite(1:5, rep(0.2, 5)), 0)
  )
  expect_equal(
    NaN,
    next_discrete(dst_finite(1:5, rep(0.2, 5)), 5)
  )
  expect_equal(
    c(1),
    next_discrete(dst_finite(1:5, rep(0.2, 5)), 0.3235)
  )
  expect_equal(
    NaN,
    next_discrete(dst_finite(1:5, rep(0.2, 5)), 65.265)
  )
})

rm("car")
