
<!-- README.md is generated from README.Rmd. Please edit that file -->

# distplyr <img src="man/figures/distplyr-240x278.png" align="right" height="150"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/vincenzocoia/distplyr/workflows/R-CMD-check/badge.svg)](https://github.com/vincenzocoia/distplyr/actions)
[![Codecov test
coverage](https://codecov.io/gh/vincenzocoia/distplyr/branch/master/graph/badge.svg)](https://codecov.io/gh/vincenzocoia/distplyr?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/distplyr)](https://CRAN.R-project.org/package=distplyr)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The purpose of `distplyr` is to equip every analyst with a tool to
seemlessly draw powerful insights using distributions. Distributions add
colour to your analysis. They show the complete picture of uncertainty.

Use `distplyr` to:

-   Create and meld distributions using a wide pallet of base forms and
    tools.
-   Draw properties from those distributions.

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

-   Keeps all components of a distribution together in a single object.
-   Computes only when needed, by dispatching an appropriate S3 method
    on call.
-   Manages the discrete components of all distributions, often arising
    from empirical estimates.

## Basic Usage

``` r
library(distplyr)
```

``` r
mix(dst_norm(-5, 1), dst_norm(0, 1), weights = c(1, 4))
#> Mixture Distribution
#> 
#> Components: 
#> # A tibble: 2 × 2
#>   distributions probs
#>   <named list>  <dbl>
#> 1 <norm>          0.2
#> 2 <norm>          0.8
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

------------------------------------------------------------------------

Please note that the ‘distplyr’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
