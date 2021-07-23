test_that("Transform norm works", {
  expect_identical(dst_norm(2, 4) + 5, dst_norm(7, 4))
  expect_identical(dst_norm(0, 54) + 0, dst_norm(0, 54))

  expect_identical(dst_norm(2, 4) - 5, dst_norm(-3, 4))
  expect_identical(dst_norm(0, 1) - 0, dst_norm(0, 1))

  expect_identical(dst_norm(1, 2) * 2, dst_norm(1, 4))
  expect_identical(dst_norm(0, 1) * 0, dst_degenerate(0))

  expect_identical(dst_norm(1, 2) / 2, dst_norm(1, 1))
  expect_identical(dst_norm(1, 2) / 4, dst_norm(1, 0.5))
})

test_that("Transform unif works", {
  expect_identical(dst_unif(2, 4) + 5, dst_unif(7, 9))
  expect_identical(dst_unif(0, 54) + 0, dst_unif(0, 54))

  expect_identical(dst_unif(2, 4) - 5, dst_unif(-3, -1))
  expect_identical(dst_unif(2, 4) - 4, dst_unif(-2, 0))

  expect_identical(dst_unif(2, 4) * 5, dst_unif(2 * 5, 20))
  expect_identical(dst_unif(2, 4) * 0.75, dst_unif(2 * 0.75, 4 * 0.75))

  expect_identical(dst_unif(1, 4) / 2, dst_unif(1 / 2, 2))
  expect_identical(dst_unif(2, 80) / 5, dst_unif(2 / 5, 16))
})

test_that("Transform degenerate works", {
  expect_identical(dst_degenerate(2) + 5, dst_degenerate(7))
  expect_identical(dst_degenerate(0) + 0, dst_degenerate(0))

  expect_identical(dst_degenerate(-2) - 5, dst_degenerate(-7))
  expect_identical(dst_degenerate(2) - 2, dst_degenerate(0))

  expect_identical(dst_degenerate(-2) * 5, dst_degenerate(-2 * 5))
  expect_identical(dst_degenerate(-2) * 65, dst_degenerate(-2 * 65))
  expect_identical(dst_degenerate(4) * 5, dst_degenerate(20))

  expect_identical(dst_degenerate(-2) / 5, dst_degenerate(-2 / 5))
  expect_identical(dst_degenerate(4) / 5, dst_degenerate(4 / 5))
})

test_that("Transform finite works", {
  expect_identical(dst_norm(2, 4) + 5, dst_norm(7, 4))
  expect_identical(dst_norm(0, 54) + 0, dst_norm(0, 54))

  expect_identical(dst_norm(2, 4) - 5, dst_norm(-3, 4))
  expect_identical(dst_norm(2, 4) - 1, dst_norm(1, 4))

  expect_identical(dst_norm(2, 4) * 5, dst_norm(2, 20))
  expect_identical(dst_norm(-3, 4) * 0.2, dst_norm(-3, 0.8))

  expect_identical(dst_norm(2, 4) / 5, dst_norm(2, 4 / 5))
  expect_identical(dst_norm(2, 4) / 1, dst_norm(2, 4))
})

test_that("Transform gpd works", {
  expect_identical(dst_gpd(2, 4, 6) + 5, dst_gpd(7, 4, 6))
  expect_identical(dst_gpd(0, 54, 5) + 0, dst_gpd(0, 54, 5))

  expect_identical(dst_gpd(2, 4, 9) - 5, dst_gpd(-3, 4, 9))
  expect_identical(dst_gpd(0, 1, -54) - 0, dst_gpd(0, 1, -54))

  expect_identical(dst_gpd(1, 2, 54) * 2, dst_gpd(1 * 2, 4, 54))
  expect_identical(dst_gpd(0, 1, 12) * 8, dst_gpd(0, 8, 12))

  expect_identical(dst_gpd(1, 2, 4) / 2, dst_gpd(1 / 2, 1, 4))
  expect_identical(dst_gpd(1, 2, 9) / 4, dst_gpd(1 / 4, 0.5, 9))
})
