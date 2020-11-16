writing functions
================

setups

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)
theme_set(theme_minimal() +  theme(legend.position = "bottom"))

options(
  ggplots2.continuous.color = "viridis",
  ggplots2.continuous.fill = "viridus"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)
(x_vec - mean(x_vec))/sd(x_vec)
```

    ##  [1]  0.51568844 -0.44693096  0.59696509 -1.04052722 -1.42565192  0.04993637
    ##  [7]  0.99841043 -0.90135098  1.55106333 -0.54092093 -0.02360795 -0.07159675
    ## [13] -0.92423460  1.09581144 -0.78940377 -0.21143205 -0.30989081  0.66988629
    ## [19]  0.23818108 -2.19036988  0.12639678 -1.69416150 -0.53535337  0.32246463
    ## [25]  0.69264419  1.13218387  0.04750053  2.70496498  0.15522235  0.20811289

I want a function to compute z\_scores

``` r
z_scores = function(x){
  if(!is.numeric(x)){
    stop("input must be numeric")
  }
  if (length(x)<3){
    stop("input must have at least three numbers")
  }
 z = (x-mean(x))/sd(x)
  return(z)
}

z_scores(x_vec)
```

    ##  [1]  0.51568844 -0.44693096  0.59696509 -1.04052722 -1.42565192  0.04993637
    ##  [7]  0.99841043 -0.90135098  1.55106333 -0.54092093 -0.02360795 -0.07159675
    ## [13] -0.92423460  1.09581144 -0.78940377 -0.21143205 -0.30989081  0.66988629
    ## [19]  0.23818108 -2.19036988  0.12639678 -1.69416150 -0.53535337  0.32246463
    ## [25]  0.69264419  1.13218387  0.04750053  2.70496498  0.15522235  0.20811289

Try my function on some other things.

``` r
z_scores(3)
```

    ## Error in z_scores(3): input must have at least three numbers

``` r
z_scores("my name is Minjie")
```

    ## Error in z_scores("my name is Minjie"): input must be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): input must be numeric

``` r
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): input must be numeric
