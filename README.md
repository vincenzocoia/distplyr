
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
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
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
critical features to come. Expect breaking changes as long as this
package is marked as “Experimental”.

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

Empirical distributions are accomodated, too.

``` r
(d2 <- stepdst(mpg, data = mtcars))
#> Step Distribution
#> 
#> Number of Discontinuities:  25
```

Manipulate distributions. Here’s an example of a mixture distribution of
two Normals:

``` r
(d3 <- mix(
  dst_norm(-5, 1), 
  dst_norm(0, 1), 
  weights = c(1, 4)
))
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
plot(d3)
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

Generate a sample from a distribution.

``` r
realise(d3, n = 10)
#>  [1] -0.97745078  0.36756790 -5.17486764 -0.05309186  1.47034339 -1.76827802
#>  [7]  0.10127566  0.44638520 -0.08511528  0.80953957
```

Calculate properties of a distribution.

``` r
mean(d1)
#> [1] 3.5
variance(d2)
#> [1] 35.18897
median(d3)
#> Warning in get_lower(cdf, level = at[1L]): This function doesn't work properly
#> yet!
#> Warning in get_higher(cdf, level = at[n_x]): This function doesn't work properly
#> yet!
#> [1] -0.3186384
evi(d1)
#> [1] -1
```

Evaluate distributional representations:

``` r
eval_density(d1, at = c(2, 3.5, 4.5))
#> [1] 0.3333333 0.3333333 0.3333333
enframe_cdf(d2, at = 1:5)
#> # A tibble: 5 x 2
#>    .arg  .cdf
#>   <int> <dbl>
#> 1     1     0
#> 2     2     0
#> 3     3     0
#> 4     4     0
#> 5     5     0
enframe_hazard(d3, at = 1:5)
#> # A tibble: 5 x 2
#>    .arg .hazard
#>   <int>   <dbl>
#> 1     1    1.53
#> 2     2    2.37
#> 3     3    3.28
#> 4     4    4.23
#> 5     5    5.19
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
together using S3 objects, but is less flexible.

The [`distr`](https://cran.r-project.org/web/packages/distr/index.html)
package allows you to make distributions including empirical ones, and
transform them, using S4 classes. distplyr aims to provide a simpler
interface using S3 objects.

-----

Please note that the ‘distplyr’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
