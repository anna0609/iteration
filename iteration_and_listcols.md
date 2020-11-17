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
library(rnoaa)
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

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
    ##  [1] 3.765936 2.015872 3.041568 2.419722 5.467023 3.774825 4.991697 2.234919
    ##  [9] 3.538316 2.969870 2.580916 3.090718 4.596598 1.824942 2.151525 3.592307
    ## [17] 2.746369 4.701739 3.829782 4.541059
    ## 
    ## $b
    ##  [1]  -3.77385183   4.40620941   0.99695402 -10.29286604  -5.31916906
    ##  [6]  -3.38290728  -4.56283018 -14.83877246   2.39069731   0.03239987
    ## [11]   3.03726690  -8.99627808  -6.04184849  -3.87918989   1.10690432
    ## [16]   6.56675798   5.31151886  -1.34711569   0.04933053   4.43347950
    ## [21]   3.36982912   8.14957410  -0.92490068   3.55101303  -1.72867292
    ## [26]  -5.03736343  -1.35373782   4.35880568  -1.66327820   1.31225776
    ## 
    ## $c
    ##  [1] 10.053629 10.095670  9.502074  9.992682 10.097573 10.145854 10.075053
    ##  [8]  9.957088 10.263315 10.068531  9.823728 10.107767  9.870340 10.326263
    ## [15]  9.951802 10.123027 10.117711  9.890066  9.916117 10.361456  9.775157
    ## [22] 10.405125  9.741106  9.927209 10.122173 10.087290  9.622634 10.159889
    ## [29] 10.029410 10.437350 10.078064  9.871684 10.339734 10.332083 10.093286
    ## [36] 10.065086 10.008599 10.020537  9.921270  9.814405
    ## 
    ## $d
    ##  [1] -1.038384 -4.319456 -3.011217 -2.345050 -3.138928 -3.552169 -3.987214
    ##  [8] -1.909591 -3.092790 -4.916991 -3.896881 -3.394767 -3.281351 -3.663524
    ## [15] -3.221494 -2.721805 -2.597709 -2.714110 -5.535083 -4.812260

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
    ## 1  3.39  1.06

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.802  5.21

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.204

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.36  1.06

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
    ##  [1] 3.765936 2.015872 3.041568 2.419722 5.467023 3.774825 4.991697 2.234919
    ##  [9] 3.538316 2.969870 2.580916 3.090718 4.596598 1.824942 2.151525 3.592307
    ## [17] 2.746369 4.701739 3.829782 4.541059
    ## 
    ## $b
    ##  [1]  -3.77385183   4.40620941   0.99695402 -10.29286604  -5.31916906
    ##  [6]  -3.38290728  -4.56283018 -14.83877246   2.39069731   0.03239987
    ## [11]   3.03726690  -8.99627808  -6.04184849  -3.87918989   1.10690432
    ## [16]   6.56675798   5.31151886  -1.34711569   0.04933053   4.43347950
    ## [21]   3.36982912   8.14957410  -0.92490068   3.55101303  -1.72867292
    ## [26]  -5.03736343  -1.35373782   4.35880568  -1.66327820   1.31225776
    ## 
    ## $c
    ##  [1] 10.053629 10.095670  9.502074  9.992682 10.097573 10.145854 10.075053
    ##  [8]  9.957088 10.263315 10.068531  9.823728 10.107767  9.870340 10.326263
    ## [15]  9.951802 10.123027 10.117711  9.890066  9.916117 10.361456  9.775157
    ## [22] 10.405125  9.741106  9.927209 10.122173 10.087290  9.622634 10.159889
    ## [29] 10.029410 10.437350 10.078064  9.871684 10.339734 10.332083 10.093286
    ## [36] 10.065086 10.008599 10.020537  9.921270  9.814405
    ## 
    ## $d
    ##  [1] -1.038384 -4.319456 -3.011217 -2.345050 -3.138928 -3.552169 -3.987214
    ##  [8] -1.909591 -3.092790 -4.916991 -3.896881 -3.394767 -3.281351 -3.663524
    ## [15] -3.221494 -2.721805 -2.597709 -2.714110 -5.535083 -4.812260

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
    ## 1  3.39  1.06

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.802  5.21

Can I just map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.39  1.06
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.802  5.21
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.204
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.36  1.06

So… can I add a list column?

``` r
listcol_df =
listcol_df %>% 
  mutate(summary = map(samp, mean_and_sd),
         medians = map_dbl(samp, median)
         )
```

## Weather data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## using cached file: /Users/Anna/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2020-10-07 13:24:28 (7.525)

    ## file min/max dates: 1869-01-01 / 2020-10-31

    ## using cached file: /Users/Anna/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2020-10-07 13:24:14 (1.699)

    ## file min/max dates: 1965-01-01 / 2020-03-31

    ## using cached file: /Users/Anna/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2020-10-07 13:24:41 (0.88)

    ## file min/max dates: 1999-09-01 / 2020-10-31

Get our listcolumns…

``` r
weather_nest =
  weather_df %>% 
  nest(data = date:tmin)
```

``` r
weather_nest %>% pull(name)
```

    ## [1] "CentralPark_NY" "Waikiki_HA"     "Waterhole_WA"

``` r
weather_nest %>% pull(data)
```

    ## [[1]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

``` r
weather_nest$data[[3]]
```

    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

Suppose i want to regress `tmax` on `tmin` for each station. This works…

``` r
lm(tmax ~ tmin, data = weather_nest$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

Let’s write a function.

``` r
weather_lm = function(df){
  lm(tmax ~ tmin, data = df)
}

output = vector("list", 3)
for(i in 1:3){
  output[[i]] = weather_lm(weather_nest$data[[i]])
}
```

What abpit a map…\!?

``` r
map(weather_nest$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

What about a map in a list column?

``` r
weather_nest =
weather_nest %>% 
  mutate(models = map(data, weather_lm))

weather_nest$models
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221
