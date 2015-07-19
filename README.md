<!-- README.md is generated from README.Rmd. Please edit that file -->
dplyr
=====

[![Build Status](https://travis-ci.org/hadley/dplyr.png?branch=master)](https://travis-ci.org/hadley/dplyr)

dplyr is the next iteration of plyr, focussed on tools for working with data frames (hence the `d` in the name). It has three main goals:

-   Identify the most important data manipulation tools needed for data analysis and make them easy to use from R.

-   Provide blazing fast performance for in-memory data by writing key pieces in [C++](http://www.rcpp.org/).

-   Use the same interface to work with data no matter where it's stored, whether in a data frame, a data table or database.

You can install:

-   the latest released version from CRAN with

    ``` r
    install.packages("dplyr")
    ```

-   the latest development version from github with

    ``` r
    if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }
    devtools::install_github("hadley/lazyeval")
    devtools::install_github("hadley/dplyr")
    ```

You'll probably also want to install the data packages used in most examples: `install.packages(c("nycflights13", "Lahman"))`.
