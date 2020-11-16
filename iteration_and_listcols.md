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
    ##  [1] 2.606554 1.842938 0.890297 2.139694 2.393367 3.138288 3.462114 1.947034
    ##  [9] 2.946421 2.472932 2.165533 3.403476 2.671908 4.267903 2.979279 3.284663
    ## [17] 3.463196 3.957939 2.431482 5.216048
    ## 
    ## $b
    ##  [1]  13.5389466  -6.4986580   4.4321698  -0.9105588   3.0712783  -7.7125715
    ##  [7]  -1.5066116  -0.2478305   0.2670874 -12.9345736  -1.8569234   0.4570762
    ## [13]   4.4455431  -5.5577373  -2.9812612   9.5448108   1.5751565   2.3193666
    ## [19]   2.1772253  -9.1949777  10.7542711  -3.0897279  -5.7487963  -1.5153034
    ## [25]  -0.8141884   1.2837881   3.2863592   4.6794772   1.9769133  10.3289087
    ## 
    ## $c
    ##  [1] 10.270541  9.970822 10.205226 10.333693 10.199518  9.981725  9.503013
    ##  [8] 10.218546 10.001200 10.069509 10.019079  9.995785  9.758411 10.153042
    ## [15]  9.738149 10.339509 10.006168  9.901374 10.523408  9.822817  9.757138
    ## [22]  9.960562  9.670485 10.139314 10.057720 10.260515  9.761865 10.357968
    ## [29]  9.788655  9.947713 10.255461  9.865540 10.219479 10.003663 10.106843
    ## [36]  9.620519  9.942656  9.702163  9.976645 10.131667
    ## 
    ## $d
    ##  [1] -1.7243490 -3.0565511 -2.7360269 -1.1740944 -5.1191881 -3.6037486
    ##  [7] -4.2755126 -3.8904974 -3.3613327 -2.0429351 -2.7819522 -3.4283530
    ## [13] -2.8418455 -2.8508210 -1.7601546 -2.6094176 -2.1051305 -0.2146779
    ## [19] -3.6258484 -3.6325357

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
    ## 1  2.88 0.958

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.452  5.98

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.228

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.84  1.13

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

``` r
output = map_dbl(list_norm, median) ##number
```

``` r
output = map_df(list_norm, mean_and_sd, .id = "input") ##datarame, track input
```

## List columns\!

``` r
listcol_df =
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1] 2.606554 1.842938 0.890297 2.139694 2.393367 3.138288 3.462114 1.947034
    ##  [9] 2.946421 2.472932 2.165533 3.403476 2.671908 4.267903 2.979279 3.284663
    ## [17] 3.463196 3.957939 2.431482 5.216048
    ## 
    ## $b
    ##  [1]  13.5389466  -6.4986580   4.4321698  -0.9105588   3.0712783  -7.7125715
    ##  [7]  -1.5066116  -0.2478305   0.2670874 -12.9345736  -1.8569234   0.4570762
    ## [13]   4.4455431  -5.5577373  -2.9812612   9.5448108   1.5751565   2.3193666
    ## [19]   2.1772253  -9.1949777  10.7542711  -3.0897279  -5.7487963  -1.5153034
    ## [25]  -0.8141884   1.2837881   3.2863592   4.6794772   1.9769133  10.3289087
    ## 
    ## $c
    ##  [1] 10.270541  9.970822 10.205226 10.333693 10.199518  9.981725  9.503013
    ##  [8] 10.218546 10.001200 10.069509 10.019079  9.995785  9.758411 10.153042
    ## [15]  9.738149 10.339509 10.006168  9.901374 10.523408  9.822817  9.757138
    ## [22]  9.960562  9.670485 10.139314 10.057720 10.260515  9.761865 10.357968
    ## [29]  9.788655  9.947713 10.255461  9.865540 10.219479 10.003663 10.106843
    ## [36]  9.620519  9.942656  9.702163  9.976645 10.131667
    ## 
    ## $d
    ##  [1] -1.7243490 -3.0565511 -2.7360269 -1.1740944 -5.1191881 -3.6037486
    ##  [7] -4.2755126 -3.8904974 -3.3613327 -2.0429351 -2.7819522 -3.4283530
    ## [13] -2.8418455 -2.8508210 -1.7601546 -2.6094176 -2.1051305 -0.2146779
    ## [19] -3.6258484 -3.6325357

``` r
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations.

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.88 0.958

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.452  5.98

Can I just map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.88 0.958
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.452  5.98
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.228
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.84  1.13

So… can I add a list column?

``` r
listcol_df =
listcol_df %>% 
  mutate(summary = map(samp, mean_and_sd),
         medians = map_dbl(samp, median)
         )
```
