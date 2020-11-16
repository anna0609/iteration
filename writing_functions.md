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

    ##  [1] -1.042530305 -2.852751595 -0.005708364 -1.266250969 -1.044081533
    ##  [6]  0.060916742 -1.254659033  0.996632126  0.394931490 -0.053666573
    ## [11] -0.655131013 -0.191468422  0.242830966  0.371207849  0.474316989
    ## [16]  2.267622472  0.539072825 -1.142359115  1.071169748  1.048829744
    ## [21] -0.253218262  0.802703082  0.620762002 -0.396460254  0.235260033
    ## [26]  0.412484146  0.336045280  1.472521968 -0.510666347 -0.678355677

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

    ##  [1] -1.042530305 -2.852751595 -0.005708364 -1.266250969 -1.044081533
    ##  [6]  0.060916742 -1.254659033  0.996632126  0.394931490 -0.053666573
    ## [11] -0.655131013 -0.191468422  0.242830966  0.371207849  0.474316989
    ## [16]  2.267622472  0.539072825 -1.142359115  1.071169748  1.048829744
    ## [21] -0.253218262  0.802703082  0.620762002 -0.396460254  0.235260033
    ## [26]  0.412484146  0.336045280  1.472521968 -0.510666347 -0.678355677

Try my function on some other things. These should give errors.

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

## Multiple outputs

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

check that the function works

``` r
x_vec = rnorm(100, mean = 3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.31  4.29

## Multiple inputs

I’d like to do this with a function.

``` r
sim_data = 
  tibble(
    x = rnorm(100, mean = 4, sd =3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.18  3.27

``` r
sim_mean_sd = function(sample_size, mu = 3, sigma = 4) ##default can overwritten
  { 
  sim_data = 
  tibble(
    x = rnorm(n = sample_size, mean = mu, sd = sigma)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
}

sim_mean_sd(sample_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.87  3.42

``` r
sim_mean_sd(mu = 6, sample_size = 100, sigma = 3) #can change position
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.55  2.91

``` r
sim_mean_sd(sample_size = 100) #by default
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.79  4.23
