test_that("next_discrete norm works", {
  expect_equal(NA_real_, next_discrete(dst_norm(1, 2), 1))
  expect_equal(NA_real_, next_discrete(dst_norm(12, 1), -2))
  expect_equal(NA_real_, next_discrete(dst_norm(65, 1), 0))
})

test_that("next_discrete gpd works", {
  expect_equal(NA_real_, next_discrete(dst_gpd(1, 2, -4), 1))
  expect_equal(NA_real_, next_discrete(dst_gpd(1, 1, 2), 1546))
  expect_equal(NA_real_, next_discrete(dst_gpd(561, 1, 8), -565))
})

test_that("next_discrete unif works", {
  expect_equal(NA_real_, next_discrete(dst_unif(1, 2), 1))
  expect_equal(NA_real_, next_discrete(dst_unif(-15, 0), -6))
  expect_equal(NA_real_, next_discrete(dst_unif(561, 18000), -99))
})

test_that("next_discrete pois works", {
  expect_equal(
    2,
    next_discrete(dst_pois(1), 1)
  )
  expect_equal(
    0,
    next_discrete(dst_pois(5456), -2)
  )
  expect_equal(
    1:10 + 55,
    next_discrete(dst_pois(561), 55.1545645, n = 10)
  )
  expect_equal(
    1,
    next_discrete(dst_pois(2), 0.465)
  )
  expect_equal(
    0,
    next_discrete(dst_pois(55), -1.465)
  )
})

test_that("next_discrete degenerate works", {
  expect_equal(NA_real_, next_discrete(dst_degenerate(1), 1))
  expect_equal(-15, next_discrete(dst_degenerate(-15), -16))
  expect_equal(NA_real_, next_discrete(dst_degenerate(56), 80))
  expect_equal(56, next_discrete(dst_degenerate(56), 16))
})


test_that("next_discrete lnorm works", {
  expect_equal(NA_real_, next_discrete(dst_lnorm(1, 2), 1))
  expect_equal(NA_real_, next_discrete(dst_lnorm(-15, 2), -16))
  expect_equal(NA_real_, next_discrete(dst_lnorm(2, 3), 80))
  expect_equal(NA_real_, next_discrete(dst_lnorm(2, 2), 16))
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
    52,
    next_discrete(dst_empirical(hp, data = car), 1)
  )
  expect_equal(
    97,
    next_discrete(dst_empirical(hp, data = car), 95)
  )
  expect_equal(
    NA_real_,
    next_discrete(dst_empirical(hp, data = car), 335)
  )
  expect_equal(
    97,
    next_discrete(dst_empirical(hp, data = car), 95.4654)
  )
  expect_equal(
    1,
    next_discrete(dst_finite(1:5, rep(0.2, 5)), 0)
  )
  expect_equal(
    NA_real_,
    next_discrete(dst_finite(1:5, rep(0.2, 5)), 5)
  )
  expect_equal(
    1,
    next_discrete(dst_finite(1:5, rep(0.2, 5)), 0.3235)
  )
  expect_equal(
    NA_real_,
    next_discrete(dst_finite(1:5, rep(0.2, 5)), 65.265)
  )
})

rm("car")
