Iteration and listcols
================

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

## Lists

You can put anything in a list.

``` r
l = list(
vec_numeric = 5:8,
vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
mat = matrix(1:8, nrow = 2, ncol = 4),
summary = summary(rnorm(100))
)
```

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
l[["vec_numeric"]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## for loop

Create a new list.

``` r
list_norm =
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = 0.2),
    d = rnorm(20, mean = -3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 3.9697271 0.6640311 2.1069329 1.6622210 3.8506620 2.2730080 3.3350532
    ##  [8] 3.9018744 3.3850280 4.7924892 3.5653933 1.6474676 1.9455066 4.5012752
    ## [15] 2.2357354 2.2551108 2.6134633 3.2571992 3.6128225 2.2611345
    ## 
    ## $b
    ##  [1] -3.0454521 -1.5379707  3.7751868  3.2961656 -4.4567027  6.3270586
    ##  [7] -3.6108729  4.5143657 -7.2619658  0.8608199 -4.5887342  0.5073213
    ## [13]  3.0222299  1.6545457 -3.0996761 -3.9517579  3.2088192 -8.8511590
    ## [19] -5.4740041 -0.3880906  9.3254734 -1.2402298  2.3293300 -7.0876660
    ## [25] -3.8445169  5.7432417 10.5053167  1.9086110 10.4958567  2.7326210
    ## 
    ## $c
    ##  [1] 10.043739  9.683498  9.898786 10.300627  9.696568 10.012229  9.905275
    ##  [8]  9.934374  9.983967 10.055275  9.988703  9.872314 10.143276 10.140392
    ## [15]  9.966909  9.703071  9.962447  9.875535  9.926525  9.760733 10.157794
    ## [22]  9.849295 10.003758 10.166262 10.166482  9.997026 10.397131  9.785628
    ## [29]  9.955193  9.927304 10.009725 10.142578  9.809880 10.129266 10.163128
    ## [36] 10.023043 10.231832  9.749051  9.913494  9.885690
    ## 
    ## $d
    ##  [1] -1.800224 -3.640136 -5.295886 -2.590207 -3.465451 -4.869991 -3.227953
    ##  [8] -4.825742 -3.674112 -2.148385 -5.553443 -2.083392 -4.096073 -3.409312
    ## [15] -2.908213 -4.374460 -2.652545 -3.600256 -3.667112 -1.032265

Pause and get my old function.

``` r
mean_and_sd = function(x){
  if(!is.numeric(x)){
    stop("input must be numeric")
  }
  if (length(x)<3){
    stop("input must have at least three numbers")
  }
 mean_x = mean(x)
 sd_x = sd(x)
 tibble(
   mean=mean_x,
   sd = sd_x
 )
}
```

I can apply that function to each list element.

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.89  1.08

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.392  5.19

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.166

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.45  1.19

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4){
  output[[i]] = mean_and_sd(list_norm[[i]])
}
```
