
<!-- README.md is generated from README.Rmd. Please edit that file -->

# distplyr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/vincenzocoia/distplyr.svg?branch=master)](https://travis-ci.org/vincenzocoia/distplyr)
[![Codecov test
coverage](https://codecov.io/gh/vincenzocoia/distplyr/branch/master/graph/badge.svg)](https://codecov.io/gh/vincenzocoia/distplyr?branch=master)
<!-- badges: end -->

The goal of `distplyr` is to provide a unified interface for
distributions by bundling various distributional quantities together,
such as a density function, mean, quantiles, and others. It allows you
to make non-standard yet useful distributions that go beyond the typical
empirical and parametric distributions, by allowing you to combine
and/or transform distributions. The name is inspired by the `dplyr`
package.

## Usage

``` r
library(distplyr)
```

Make a Normal distribution with mean 2 and variance 5:

``` r
(d1 <- dst_norm(2, 5))
#> Unnamed distribution.
#> 
#> Parameters:
```

Evaluate the survival function to obtain the probability of exceeding 6.
What about the hazard function at 6?

``` r
eval_survival(d1, at = 6)
#> [1] 0.03681914
eval_hazard(d1, at = 6)
#> [1] 0.9783186
```

Combine this distribution with another, say to form a mixture
distribution:

``` r
d2 <- dst_norm(-5, 1)
d3 <- mix(d1, d2, probs = c(0.4, 0.6))
# plot(d3)
```

What’s the mean of the mixture distribution? Standard deviation?

``` r
get_mean(d3)
#> [1] -2.2
get_sd(d3)
#> [1] 3.789459
```

Generate some data from this new mixture distribution:

``` r
set.seed(1)
y <- eval_randfn(d3, at = 100)
```

Make an empirical distribution from the generated data, and compare the
cdf with the original:

``` r
d4 <- stepdst(y)
plot(d4, "cdf")
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

``` r
plot(d3, "cdf", from = min(y), to = max(y))
```

<img src="man/figures/README-unnamed-chunk-8-2.png" width="100%" />

## `distplyr` in Context

Note that `distplyr` is *not* a modelling package, meaning it won’t
optimize a distribution’s fit to data.

The
[`distributions3`](https://cran.r-project.org/web/packages/distributions3/index.html)
package is a similar package in that it bundles parametric distributions
together using S3 objects, but does not handle step distributions.

The [`distr`](https://cran.r-project.org/web/packages/distr/index.html)
package allows you to make distributions including empirical ones, and
transform them, using S4 classes.

-----

Please note that the ‘distplyr’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
