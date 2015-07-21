<!-- README.md is generated from README.Rmd. Please edit that file -->
imhistR
=======

imhistR is the easy image processing tool. This package focussed for luminance, RGB, Lab, and HSB(HSV) with image. It motivated to visualize image color information. It have two main goals:

-   Draw an image histgramn quickly in R.

-   Calculate descriptive stastics (Mean, SD, Skewness, Kurtsis) by one command.

Install:

-   The latest development version get from github with

    ``` r
    if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }
    devtools::install_github("mokazuma/imhistR")
    ```

-   Usage example:

    ``` r
    require("imhistR")
    lrgbhist(input="http://www.ess.ic.kanagawa-it.ac.jp/std_img/colorimage/Lenna.jpg", mode="url", hist="Lenna")
    ```

You get an image histgram and their descriptive stastics.
