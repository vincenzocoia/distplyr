test_that("Base discretise functionality works", {
  d0 <- dst_norm(0, 1)
  breaks <- -2:2
  n_breaks <- length(breaks)
  d1 <- discretise(d0, breakpoints = breaks, midpoints = "median")
  expect_true(is_finite_dst(d1))
  expect_equal(nrow(d1$probabilities), n_breaks + 1)
  expect_true(all(d1$probabilities$location[1:n_breaks] <= breaks))
  expect_true(all(d1$probabilities$location[1 + 1:n_breaks] >= breaks))
  expect_equal(d1, discretise(d0, append(breaks, NA)))
})

test_that("Venturing outside of the probability distribution works.", {
  d0 <- dst_exp(1)
  expect_equal(discretise(d0, -2:4), discretise(d0, 1:4))
  expect_equal(discretise(d0, -10:-4), dst_degenerate(median(d0)))
  expect_equal(discretise(d0, numeric()), dst_degenerate(median(d0)))
  expect_equal(discretise(d0, numeric()), discretise(d0, NA))
  expect_equal(discretise(d0, numeric(), midpoints = "mean"), dst_degenerate(1))
})


test_that("Open and Closed works.", {
  d0 <- dst_pois(1)
  d11 <- discretise(d0, c(0, 4, 10), closed = "right")
  d12 <- discretise(d0, c(0, 4, 10), closed = "left")
  expect_equal(nrow(d11$probabilities), 4)
  expect_equal(nrow(d12$probabilities), 3)
})
