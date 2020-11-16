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

    ##  [1] -0.75251694 -0.32694126  1.93528026  0.41855459  0.93460086 -0.01034709
    ##  [7] -1.06848316 -0.59156994  1.38518595 -1.19471633 -0.72554062 -0.24301139
    ## [13] -2.25658043 -1.12454787  1.37321477  2.57490932  0.69886823  0.28291105
    ## [19] -0.46220400 -0.01121193 -0.67356279 -0.15951009  0.55268486 -0.53416109
    ## [25] -0.09378006  0.26011498  0.93563313 -0.31761460 -0.46416345 -0.34149495

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

    ##  [1] -0.75251694 -0.32694126  1.93528026  0.41855459  0.93460086 -0.01034709
    ##  [7] -1.06848316 -0.59156994  1.38518595 -1.19471633 -0.72554062 -0.24301139
    ## [13] -2.25658043 -1.12454787  1.37321477  2.57490932  0.69886823  0.28291105
    ## [19] -0.46220400 -0.01121193 -0.67356279 -0.15951009  0.55268486 -0.53416109
    ## [25] -0.09378006  0.26011498  0.93563313 -0.31761460 -0.46416345 -0.34149495

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
    ## 1  3.28  3.81

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
    ## 1  4.39  2.82

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
    ## 1  6.21  3.20

``` r
sim_mean_sd(mu = 6, sample_size = 100, sigma = 3) #can change position
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.22  2.87

``` r
sim_mean_sd(sample_size = 100) #by default
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.89  4.09

## Let’s review Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>% ## get the first digit of number between 0-9 at the beginning of that string
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% ## get rid of "\n"
  str_trim()

reviews_page1 = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

what about the next page of reviews…

Let’s turn that code into a function

``` r
read_page_reviews = function(url){
html = read_html(url)

review_titles = 
  html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>% ## get the first digit of number between 0-9 at the beginning of that string
  as.numeric()

review_text = 
  html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% ## get rid of "\n"
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)

reviews

}
```

Let me try my function.

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=3"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 x 3
    ##    title                     stars text                                         
    ##    <chr>                     <dbl> <chr>                                        
    ##  1 👍                            5 "Exactly as described and came on time 👍"   
    ##  2 A top favorite movie !!       5 "Love this movie, needed to add it to my col…
    ##  3 Best.Movie!                   5 "I enjoyed showing my children this \"classi…
    ##  4 Great Movie                   5 "I love this movie. Showed it to my middle s…
    ##  5 Tina, you fat lard, come…     5 "A very quotable, awkard and hilarious movie…
    ##  6 Funny!                        4 "It is a great movie although it’s a little …
    ##  7 Excellent for families        5 "Highly recommend for family entertainment"  
    ##  8 Hilarious!                    5 "Hilarious!"                                 
    ##  9 Excellent in all fronts.      5 "Excellent in all fronts."                   
    ## 10 good                          5 "good"

Let’s read a few pages of reviews.

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:50)

all_reviews =
bind_rows(
read_page_reviews(dynamite_urls[1]),
read_page_reviews(dynamite_urls[2]),
read_page_reviews(dynamite_urls[3]),
read_page_reviews(dynamite_urls[4]),
read_page_reviews(dynamite_urls[5])
  )
```

## Mean scoping example

``` r
f = function(x){
  z=x+y
  z
}

x = 1
y = 2
f(x = y) ## f(x = 2)
```

    ## [1] 4

## Functions as arguments

``` r
my_summary = function(x, summ_func){
  
  summ_func(x)
  
}
x_vec = rnorm(100,3,7)

mean(x_vec)
```

    ## [1] 1.450907

``` r
median(x_vec)
```

    ## [1] 0.960772

``` r
my_summary(x_vec, IQR)
```

    ## [1] 7.717029
