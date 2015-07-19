<!-- README.md is generated from README.Rmd. Please edit that file -->
imhistR
=======

imhistR is the easy image processing tool. This package focussed for luminance, RGB, Lab, and HSB(HSV) with image. It motivated to visualize image luminance and color space. It have two main goals:

-   Draw image histgramn quickly in R.

-   Calculate descriptive stastics (Mean, SD, Skew, Kurtsis) by one command.

Install:

-   the latest development version get from github with

    ``` r
    if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }
    devtools::install_github("mokazuma/imhistR")
    ```

For example,

    ```R
    require("imhistR")
    lrgbhist("http://blog-imgs-35-origin.fc2.com/i/m/a/imagingsolution/Lenna.png", mode="url", hist="Lena")
    ```
