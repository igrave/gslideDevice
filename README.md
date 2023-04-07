
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gslideDevice

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/gslideDevice)](https://CRAN.R-project.org/package=gslideDevice)
<!-- badges: end -->

gslideDevice lets you draw directly onto google slides from R. This
package implements the drawing primitives of an R graphics device as API
calls.

## Installation

You can install the development version of gslideDevice from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("igrave/gslideDevice")
```

## Example

Load the package and authenticate with google:

``` r
library(gslideDevice)
auth_gsd("google.account@example.com")
```
