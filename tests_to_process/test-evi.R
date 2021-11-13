a <- dst_unif(0, 1)
b <- dst_gpd(0, 1, 1)
c <- dst_gpd(1 / 2, 1, -2) # right endpoint = 1
d <- dst_gpd(0, 1, -3) # right endpoint = 1/3
e <- dst_gpd(0, 1, 0)
f <- dst_norm(0, 1)
g <- dst_empirical(-10:10)

test_that("graft EVI works", {
  expect_identical(
    evi(graft_right(a, b, sep_y = 0.5)),
    1
  )
  expect_identical(
    evi(graft_right(a, c, sep_y = 0.5)),
    -2
  )
  expect_identical(
    evi(graft_right(b, f, sep_y = 0.5)),
    0
  )
})

check_mix <- function(..., evi) {
  n <- length(list(...))
  expect_identical(evi(mix(...)), evi)
}

test_that("mixture EVI works", {
  check_mix(a, b, evi = 1)
  check_mix(a, c, evi = -2)
  check_mix(a, c, d, evi = -2)
  check_mix(a, b, c, d, e, f, g, evi = 1)
  check_mix(e, f, a, evi = 0)
  check_mix(f, g, evi = 0)
  check_mix(a, c, d, g, evi = NaN)
  check_mix(b, g, evi = 1)
})
