test_that("slicing simplifies with finite distributions", {
  d <- dst_empirical(1:5)
  expect_error(slice_left(d, 5))
  expect_error(slice_right(d, 1))
  expect_true(is_finite_dst(slice_left(d, 2.5)))
  expect_true(is_finite_dst(slice_right(d, 2.5)))
  expect_true(is_finite_dst(slice_left(slice_right(d, 4), 2)))
  expect_equal(eval_pmf(slice_left(d, 1), at = 1:5), c(0, rep(0.25, 4)))
  expect_equal(
  	eval_pmf(slice_left(d, 2, include = FALSE), at = 1:5),
  	c(0, rep(0.25, 4))
  )
  expect_equal(eval_pmf(slice_right(d, 5), at = 1:5), c(rep(0.25, 4), 0))
  expect_equal(
  	eval_pmf(slice_right(d, 4, include = FALSE), at = 1:5),
  	c(rep(0.25, 4), 0)
  )
})

test_that("Range also gets sliced", {
	d1 <- dst_norm(0, 1)
	d2 <- dst_unif(0, 1)
	d3 <- dst_empirical(1:5)
	expect_equal(range(slice_left(d1, -2)), c(-2, Inf))
	expect_equal(range(slice_right(d2, 0.6, include = TRUE)), c(0, 0.6))
	expect_equal(range(slice_right(d3, 4.5)), c(1, 4))
})

test_that("breakpoint inclusion works with mixtures", {
	d <- mix(dst_norm(0, 1), dst_empirical(0:4))
	dl_inc <- slice_left(d, 0, include = TRUE)
	dl_not <- slice_left(d, 0, include = FALSE)
	dr_inc <- slice_right(d, 0, include = TRUE)
	dr_not <- slice_right(d, 0, include = FALSE)
	expect_equal(eval_pmf(dl_inc, at = 0, strict = FALSE), 0)
	expect_equal(eval_pmf(dl_not, at = 0, strict = FALSE), 0.2)
	expect_equal(eval_pmf(dr_inc, at = 0, strict = FALSE), 0)
	expect_equal(eval_pmf(dr_not, at = 0, strict = FALSE), 1 / 6)
})

test_that("slicing with infinity works as expected", {
	d <- dst_norm(0, 1)
	e <- dst_empirical(1:4)
	expect_error(slice_left(d, Inf))
	expect_error(slice_left(d, Inf, include = FALSE))
	expect_error(slice_right(d, -Inf))
	expect_error(slice_right(d, -Inf, include = FALSE))
	expect_error(slice_left(e, Inf))
	expect_error(slice_left(e, Inf, include = FALSE))
	expect_error(slice_right(e, -Inf))
	expect_error(slice_right(e, -Inf, include = FALSE))
})
