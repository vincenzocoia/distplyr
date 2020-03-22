
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

The goal of distplyr is to provide a unified interface for manipulating
distributions. The name is inspired by the `dplyr` package.

The need for this package arose as I was trying to implement
distributional forecasting models. I found that I was home-baking
functions like a cdf or quantile function, and spent far too much energy
managing and distinguishing between distribution quantities throughout
the analysis. This package bundles these all together in a single
“distribution” object.

The
[`distributions3`](https://cran.r-project.org/web/packages/distributions3/index.html)
package is close to what I’ve been looking for, but I wanted to be able
to work with any distribution, including non-parametric or
semi-parametric ones, and I think in order to do that with
`distributions3`, you’d have to contribute to the package.

Please note that the ‘distplyr’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.

``` r
library(distplyr)
library(magrittr)
library(testthat)
```

## Usage

### Making a Distribution

Access a distribution from a parametric family using `dst_*()`
functions. Make a Uniform(2, 3) distribution:

``` r
(my_unif <- dst_unif(2, 3))
#> A Uniform distribution.
#> 
#> Parameters:
```

Or, a GPD:

``` r
(my_gpd <- dst_gpd(loc = 7, scale = 1, shape = 0.5))
#> A GPD distribution.
#> 
#> Parameters:
```

You can make an empirical distribution from data as well, by making a
step distribution:

``` r
(my_step <- stepdst(Sepal.Length, data = iris))
#> Unnamed distribution.
#> 
#> Parameters:
```

A degenerate distribution is still valid, and can be specified directly,
or as a result of parameter boundaries:

``` r
expect_equal(
    dst_norm(mu = 5, var = 0),
    dst_degen(5)
)
```

### Evaluating a Distribution

Let’s use `my_gpd` from earlier.

Easy to plot:

``` r
# plot(my_gpd, "cdf")
# plot(my_gpd, "probfn")
```

Means are easy to find – no more looking up formulas:

``` r
get_mean(my_gpd)
#> [1] 9
```

So are variances, skewnesses, etc:

``` r
get_var(my_gpd)
#> [1] Inf
# median(my_dst)
```

Evaluating distribution-related functions, such as cdf, density, etc. is
easy:

``` r
eval_cdf(my_gpd, at = 6:10)
#> [1] 0.0000000 0.0000000 0.5555556 0.7500000 0.8400000
eval_probfn(my_gpd, at = 6:10)
#> [1] 0.0000000 1.0000000 0.2962963 0.1250000 0.0640000
eval_hazfn(my_gpd, at = 6:10)
#> [1] 0.0000000 1.0000000 0.6666667 0.5000000 0.4000000
```

Generate some data:

``` r
eval_randfn(my_gpd, 10)
#>  [1]  7.191411  7.248137  7.384061  9.034786  7.649470 15.602773  8.037122
#>  [8]  7.895555  7.266341  7.153382
```

Or, just get the functions themselves:

``` r
cdf <- get_cdf(my_gpd)
curve(cdf, 6, 10)
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" style="display: block; margin: auto;" />

### Manipulate distributions

You can `shift_left()` or `shift_right()` (or just `shift()`), or
`scale_divide()` or `scale_multiply()`.

``` r
# my_dst %>% 
#   shift_left(by = 1) %>% 
#   scale_divide(by = 0.5)
```

Graft `my_gpd` onto `my_step`, the empirical distribution from earlier:

``` r
my_graft <- my_step %>%
    graft_right(my_gpd, sep_y = 7)
```

``` r
cdf <- get_cdf(my_graft)
curve(cdf, 4, 9, n = 1001)
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="100%" />

Obtain a mixture distribution:

``` r
# mix(..., probs = ...)
```

## MLE?

(maybe for another package)

``` r
# fam <- function(a, b) 
#   dst_unif(a, b) %>% 
#   get_nllh(data = iris$Sepal.Length)
#   
# 
# dist3 %>% 
#   get_nllh(data = ...)
# 
# nllh(~ Sepal.Length, data = iris, families = c("unif", "norm"))
```
