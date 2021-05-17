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
})


test_that("range for degenerate dst works", {
	expect_equal(range(dst_degenerate(0)), 0)
	expect_equal(range(dst_degenerate(-165465)), -165465)
	expect_equal(range(dst_degenerate(0.2)), 0.2)
	expect_equal(range(dst_degenerate(-2.26)), -2.26)
})
