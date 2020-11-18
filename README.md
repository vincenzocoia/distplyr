
<!-- README.md is generated from README.Rmd. Please edit that file -->

# distplyr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/vincenzocoia/distplyr.svg?branch=master)](https://travis-ci.org/vincenzocoia/distplyr)
[![Codecov test
coverage](https://codecov.io/gh/vincenzocoia/distplyr/branch/master/graph/badge.svg)](https://codecov.io/gh/vincenzocoia/distplyr?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/distplyr)](https://CRAN.R-project.org/package=distplyr)
<!-- badges: end -->

The purpose of `distplyr` is to equip every analyst with a tool to
seemlessly draw powerful insights using distributions. Distributions add
colour to your analysis. They show the complete picture of uncertainty.

Use `distplyr` to:

  - Create and meld distributions using a wide pallet of base forms and
    tools.
  - Draw properties from those distributions.

Many distributions in practice are built in “layers”, by transforming
and combining other distributions. The result is a tailored distribution
that does not follow a basic parametric form such as “Normal” or
“Exponential”. The motivation behind the name of `distplyr` is that
distributions are built by manipulation, akin to the package `dplyr`.

**Note**: This package is still in its infancy. There are many other
critical features to come. Expect breaking changes over the first
several versions.

## Design Choices

`distplyr`:

  - Keeps all components of a distribution together in a single object.
  - Computes only when needed, by dispatching an appropriate S3 method
    on call.
  - Manages the discrete components of all distributions, often arising
    from empirical estimates.

## Basic Usage

``` r
library(distplyr)
```

There are many parametric families of distributions at your disposal.
Here is a Uniform distribution:

``` r
(d1 <- dst_unif(2, 5))
#> Uniform Distribution
#> 
#> Parameters:
#> # A tibble: 2 x 2
#>   parameter value
#>   <chr>     <dbl>
#> 1 min           2
#> 2 max           5
#> 
#> Number of Discontinuities:  0
```

Evaluate or enframe functional representations, such as the cdf and
hazard function:

``` r
eval_cdf(d1, at = 3)
#> [1] 0.3333333
enframe_hazard(d1, at = 3)
#> # A tibble: 1 x 2
#>    .arg .hazard
#>   <dbl>   <dbl>
#> 1     3     0.5
```

Make a mixture distribution by combining some distributions:

``` r
(d2 <- mix(dst_norm(-5, 1), dst_norm(0, 1), weights = c(1, 4)))
#> Mixture Distribution
#> 
#> Components:
#> # A tibble: 2 x 2
#>   distribution weight
#>   <chr>         <dbl>
#> 1 Gaussian        0.2
#> 2 Gaussian        0.8
#> 
#> Number of Discontinuities:  0
plot(d2, n = 1001)
#> Warning in get_lower(cdf, level = at[1L]): This function doesn't work properly
#> yet!
#> Warning in get_higher(cdf, level = at[n_x]): This function doesn't work properly
#> yet!
#> Warning in get_lower(cdf, level = at[1L]): This function doesn't work properly
#> yet!
#> Warning in get_higher(cdf, level = at[n_x]): This function doesn't work properly
#> yet!
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" style="display: block; margin: auto;" />

Make a graft distribution by replacing a distribution’s tail:

``` r
d3 <- stepdst(mpg, data = mtcars)
d4 <- graft_right(d3, dst_gpd(25, 5, 2), sep_y = 25)
plot(d4, "cdf", n = 1001, to = 34)
plot(d3, "cdf", n = 1001, lty = 2, add = TRUE)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" style="display: block; margin: auto;" />

Calculate mean, variance, etc:

``` r
variance(d1)
#> [1] 0.75
skewness(d2)
#> [1] -1.073313
mean(d3)
#> [1] 20.09062
evi(d4)
#> [1] 2
```

## Installation

`distplyr` is not on CRAN yet, so the best way to install it is:

``` r
devtools::install_github("vincenzocoia/distplyr")
```

## `distplyr` in Context

`distplyr` is *not* a modelling package, meaning it won’t optimize a
distribution’s fit to data.

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
