
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rtumblr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/Rtumblr)](https://CRAN.R-project.org/package=Rtumblr)
[![R-CMD-check](https://github.com/schochastics/Rtumblr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/schochastics/Rtumblr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/schochastics/Rtumblr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/schochastics/Rtumblr?branch=main)
<!-- badges: end -->

RTumblr is a wrapper for the official Tumblr API. (An [existing
CRAN](https://cran.r-project.org/web/packages/tumblR/index.html) package
has been archived)

## Installation

You can install the development version of RTumblr like so:

``` r
devtools::install_github("schochastics/RTumblr")
```

## API keys

You need to register an app here: <https://www.tumblr.com/oauth/apps>
Then, create an environment variable called “RTUMBLR_TOKEN” (for
instance in `.Renviron`) from the consumer_key and the consumer_secret
as follows. If you have

``` r
consumer_key = 123456789
consumer_secret = abcdefghi
```

Then your environment variable RTUMBLR_TOKEN should be
`123456789;abcdefghi`

``` r
library(RTumblr)
## basic example code
```
