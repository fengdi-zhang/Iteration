writing_functions
================
2022-10-29

## Z-scores!!

Let’s compute the z-score version of a list of numbers.

``` r
x_vec = rnorm(25, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.65002374 -0.20871837 -0.36449399 -0.16240195 -1.20258715 -0.40640086
    ##  [7] -0.08736782 -0.15006198 -1.32209649 -0.21742056 -1.24980807 -0.38607964
    ## [13]  2.01877160 -0.66282170  0.57784799  0.59219580  1.89622584 -0.67918079
    ## [19] -0.47450063 -1.40494499  0.25211011  2.44677629  0.26920543 -0.41474048
    ## [25]  0.69046865

``` r
z_scores = function(x) {
  
  if((!is.numeric(x))) {
    stop("Z scores only work for numbers")
  }
  
  if (length(x) < 3) {
    stop("Z scores only work if you have three or more numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  z
  
}

z_scores(x_vec)
```

    ##  [1]  0.65002374 -0.20871837 -0.36449399 -0.16240195 -1.20258715 -0.40640086
    ##  [7] -0.08736782 -0.15006198 -1.32209649 -0.21742056 -1.24980807 -0.38607964
    ## [13]  2.01877160 -0.66282170  0.57784799  0.59219580  1.89622584 -0.67918079
    ## [19] -0.47450063 -1.40494499  0.25211011  2.44677629  0.26920543 -0.41474048
    ## [25]  0.69046865

``` r
#z_scores(3)
#can't take sd of 3
```

## Let’s have mulitple outpus

Let’s get mean and sd from the vector input

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(mean = mean_x, 
       sd = sd_x) #can also use list
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.71  3.95

## Let’s start with mulitple inputs!

``` r
x_vec = rnorm(n = 25, mean = 7, sd = 4)

tibble(
  mean = mean(x_vec),
  sd = sd(x_vec)
)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.99  4.26

Can I do this using a function?

``` r
sim_mean_sd = function(n_obs, true_mean = 7, true_sd = 4) {
  
  x = rnorm(n = n_obs, mean = true_mean, sd = true_sd)

  tibble(
    mean = mean(x),
    sd = sd(x)
  )
}
```

``` r
sim_mean_sd(25, 100, 7)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  100.  7.34

## Scraping Amazon

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
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

``` r
read_page_reviews <- function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim() %>% 
    str_subset("The media could not be loaded.", negate = TRUE) %>% 
    str_subset("^$", negate = TRUE)
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}

url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base, 1:5)

dynamite_reviews = bind_rows(
  read_page_reviews(vec_urls[1]),
  read_page_reviews(vec_urls[2]),
  read_page_reviews(vec_urls[3]),
  read_page_reviews(vec_urls[4]),
  read_page_reviews(vec_urls[5])
)

dynamite_reviews
```

    ## # A tibble: 50 × 3
    ##    title                                stars text                              
    ##    <chr>                                <dbl> <chr>                             
    ##  1 70’s and 80’s Schtick Comedy             5 "…especially funny if you have ev…
    ##  2 Amazon Censorship                        5 "I hope Amazon does not censor my…
    ##  3 Watch to say you did                     3 "I know it's supposed to be a cul…
    ##  4 Best Movie Ever!                         5 "We just love this movie and even…
    ##  5 Quirky                                   5 "Good family film"                
    ##  6 Funny movie - can't play it !            1 "Sony 4k player won't even recogn…
    ##  7 A brilliant story about teenage life     5 "Napoleon Dynamite delivers dry h…
    ##  8 HUHYAH                                   5 "Spicy"                           
    ##  9 Cult Classic                             4 "Takes a time or two to fully app…
    ## 10 Sweet                                    5 "Timeless Movie. My Grandkids are…
    ## # … with 40 more rows
