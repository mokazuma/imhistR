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
    # You run following two line cord only when first time to use this package.
    install.packages("devtools")  # if you are still not download
    devtools::install_github("mokazuma/imhistR")
    ```

-   Usage example:

    ``` r
    require("imhistR")
    lrgbhist(input="http://www.ess.ic.kanagawa-it.ac.jp/std_img/colorimage/Lenna.jpg", 
             mode="url", output="Lenna")
    ```

You get an image histgram and their descriptive stastics.
