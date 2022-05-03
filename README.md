
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pitcheR

<!-- badges: start -->
<!-- badges: end -->

The goal of pitcheR is to analyze statcast data from MLB pitchers with
summary tables and advanced graphics. Statcast data is not the normal
baseball data you see on the back of baseball cards. The files that work
with this package track many in-depth variables, such as spin rate and
exact location, for every single pitch thrown by a given pitcher. Our
package’s functionality will hopefully provide insights into MLB
pitchers that standard data and graphs cannot give you.

## Installation

You can install the development version of pitcheR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kieran41/pitcheR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
#library(pitcheR)
## basic example code
## IMPORTANT: all  input files must be the standard .csv statcast files downloaded from their website

#creates a plot displaying vertical and horizontal break by pitch type for Clayton Kershaw
#break_func(file = "kershaw.csv")
```

## Extended Information

While it is not included in our package, there is a package called
baseballr that allows you to scrape the exact files that go with our
package. You are free to check out this package here:

<https://billpetti.github.io/baseballr/>
