<!-- README.md is generated from README.Rmd. Please edit that file -->
imhistR
=======

imhistR is the easy image processing tool. This package focussed for Luminance, RGB, Lab, and HSB(HSV) with image. It motivated to visualize image color information.

It have two main goals:

-   Draw an image histogram quickly in R.

-   Calculate their descriptive stastics (Mean, SD, Skewness, Kurtsis) by one command.

Install:

-   The latest development version get from github with

    ``` r
    if (!require('devtools')) install.packages('devtools')
    devtools::install_github("mokazuma/imhistR")
    ```

-   Usage example:

    ``` r
    require("imhistR")
    lrgbhist(input="https://www.r-project.org/Rlogo.png", 
             mode="url", output="Rlogo", endoff=TRUE)
    ```

You get an image histogram and their descriptive stastics.

The Japanese version of package vignette is available [here](http://rpubs.com/mokazuma/imhistR).
