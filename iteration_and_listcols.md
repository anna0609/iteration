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
    ##  [1] 3.2004317 2.4279468 2.6257342 3.7319617 3.0904243 2.1433231 2.9699174
    ##  [8] 4.2142346 4.1478453 1.9781079 3.4149161 2.1277391 2.4297542 4.8310757
    ## [15] 4.3738926 3.3130231 1.5669428 3.6991967 2.6942503 0.5849403
    ## 
    ## $b
    ##  [1] -11.5032273   8.0877751   6.0334051  -1.0811888  -6.3281588   0.5056681
    ##  [7]   2.4609573  -3.4043360  -6.0627760  -3.5963577  -2.7444649  -2.4868288
    ## [13]  -8.9803214   3.6319703  -2.1496950   7.6901422   0.4049258   2.7269635
    ## [19]   1.1834071  -3.3357266  -3.3974301   4.6020056  -2.9068787   7.8243216
    ## [25]   6.7371344  -5.3683760   0.5272444   4.7081242   1.2488441  -3.6760338
    ## 
    ## $c
    ##  [1] 10.283271 10.168146  9.800508 10.162859 10.282281  9.957609 10.285149
    ##  [8]  9.781069  9.986564  9.604010  9.945259  9.736554  9.764582  9.786523
    ## [15]  9.947896 10.197770  9.761362  9.779843  9.940548 10.213097  9.721562
    ## [22] 10.089077  9.850855 10.464015 10.100550 10.206938 10.024380  9.943268
    ## [29] 10.133637  9.822332  9.989817 10.233388 10.317931  9.904134  9.872431
    ## [36]  9.827121 10.200107 10.018246 10.228504 10.012809
    ## 
    ## $d
    ##  [1] -4.385727 -2.391924 -4.180532 -3.532009 -3.028329 -1.294191 -3.946510
    ##  [8] -4.274214 -3.053140 -2.901441 -3.496622 -3.402045 -1.393086 -2.619951
    ## [15] -3.075961 -1.906905 -3.472638 -3.485007 -4.113308 -2.025971

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
    ## 1  2.98  1.04

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.288  5.07

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.205

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.10 0.923

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4){
  output[[i]] = mean_and_sd(list_norm[[i]])
}
```

## Let’s try map\!

``` r
output = map(list_norm, mean_and_sd) ## map(input list, function apply)
```

What if you want a different function..?

``` r
output = map(list_norm, median) 
```
