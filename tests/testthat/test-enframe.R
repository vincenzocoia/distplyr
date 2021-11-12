test_that("enframe matches eval", {
	d <- distionary::dst_norm(0, 1)
	p <- 1:9/10
	x <- -3:3
	expect_equal(
		distionary::enframe_cdf(d, at = x)$cdf,
		distionary::eval_cdf(d, at = x))
	expect_equal(
		distionary::enframe_density(d, at = x)$density,
		distionary::eval_density(d, at = x))
	expect_equal(
		distionary::enframe_pmf(d, at = x, strict = FALSE)$pmf,
		distionary::eval_pmf(d, at = x, strict = FALSE))
	expect_equal(
		distionary::enframe_odds(d, at = x)$odds,
		distionary::eval_odds(d, at = x))
	expect_equal(
		distionary::enframe_survival(d, at = x)$survival,
		distionary::eval_survival(d, at = x))
	expect_equal(
		distionary::enframe_hazard(d, at = x)$hazard,
		distionary::eval_hazard(d, at = x))
	expect_equal(
		distionary::enframe_chf(d, at = x)$chf,
		distionary::eval_chf(d, at = x))
	expect_equal(
		distionary::enframe_quantile(d, at = p)$quantile,
		distionary::eval_quantile(d, at = p))
})

test_that("enframe throws error if an ellipsis entry is not a distribution", {
	d <- distionary::dst_norm(0, 1)
	expect_error(distionary::enframe_cdf(d, at = 1:10, strict = TRUE))
	expect_error(distionary::enframe_cdf(d, 5, at = 1:10))
	expect_error(distionary::enframe_cdf(at = 1:10))
})

test_that("column names match the function, by default.", {
	d <- distionary::dst_norm(0, 1)
	expect_equal(
		names(distionary::enframe_cdf(d, at = 0))[2L],
		"cdf"
	)
	expect_equal(
		names(distionary::enframe_pmf(d, at = 0, strict = FALSE))[2L],
		"pmf"
	)
	expect_equal(
		names(distionary::enframe_odds(d, at = 0))[2L],
		"odds"
	)
	expect_equal(
		names(distionary::enframe_density(d, at = 0))[2L],
		"density"
	)
	expect_equal(
		names(distionary::enframe_quantile(d, at = 0.1))[2L],
		"quantile"
	)
	expect_equal(
		names(distionary::enframe_chf(d, at = 0))[2L],
		"chf"
	)
	expect_equal(
		names(distionary::enframe_hazard(d, at = 0))[2L],
		"hazard"
	)
	expect_equal(
		names(distionary::enframe_survival(d, at = 0))[2L],
		"survival"
	)
})
