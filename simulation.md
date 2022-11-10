simulations
================

## Simulations

``` r
sim_mean_sd = function(n, mu = 2, sigma = 3) {
  
  sim_data = tibble(
    x = rnorm(n, mean = mu, sd = sigma),
  )
  
  sim_data %>% 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
}
```

How did we use this before?

``` r
sim_mean_sd(n = 30)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.29      3.03

How can we use this now?

Let’s start with a for loop

``` r
output = vector("list", length = 100)

for (i in 1:100) {
  output[[i]] = sim_mean_sd(n = 30)
}

bind_rows(output)
```

    ## # A tibble: 100 × 2
    ##    mu_hat sigma_hat
    ##     <dbl>     <dbl>
    ##  1   1.64      3.34
    ##  2   1.91      2.87
    ##  3   2.13      2.21
    ##  4   1.98      2.74
    ##  5   3.06      2.47
    ##  6   1.96      3.32
    ##  7   2.06      2.72
    ##  8   2.72      2.64
    ##  9   2.31      2.90
    ## 10   1.66      3.64
    ## # … with 90 more rows

Let’s use list columns instead

``` r
sim_results_df = 
  expand_grid(
    sample_size = 30,
    iteration = 1:100
  ) %>% #give all possible combinations of this sample size and this iteration
  mutate(
    estimate_df = map(sample_size, sim_mean_sd)
  ) %>%
  unnest(estimate_df)
```

``` r
sim_results_df %>%
  ggplot(aes(x = mu_hat)) +
  geom_density()
```

<img src="simulation_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

## what about changing sample size

``` r
sim_results_df = 
  expand_grid(
    sample_size = c(30, 60, 120, 240),
    iteration = 1:100
  ) %>% #give all possible combinations of this sample size and this iteration
  mutate(
    estimate_df = map(sample_size, sim_mean_sd)
  ) %>%
  unnest(estimate_df)
```

``` r
sim_results_df %>%
  mutate(
    sample_size = str_c("N = ", sample_size),
    sample_size = fct_inorder(sample_size)
  ) %>% # violin need x to be factor not number
  ggplot(aes(x = sample_size, y = mu_hat)) +
  geom_violin()
```

<img src="simulation_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

``` r
sim_results_df %>%
  mutate(
    sample_size = str_c("N = ", sample_size),
    sample_size = fct_inorder(sample_size)
  ) %>% 
  group_by(sample_size) %>%
  summarize(
    em_st_err = sd(mu_hat)
  )
```

    ## # A tibble: 4 × 2
    ##   sample_size em_st_err
    ##   <fct>           <dbl>
    ## 1 N = 30          0.517
    ## 2 N = 60          0.378
    ## 3 N = 120         0.318
    ## 4 N = 240         0.201

## Let’s see two inputs

``` r
#cache=TRUE saves the result, don't rerun as long as code unchange
#if this code chunk depends on other code chunk, changing the other code chunk doesn not cause this code chunk to rerun, which can cause problems
sim_results_df = 
  expand_grid(
    sample_size = c(30, 60, 120, 240),
    true_sigma = c(6, 3),
    iteration = 1:1000
  ) %>% #give all possible combinations of this sample size and this iteration
  mutate(
    estimate_df = map2(.x = sample_size, .y = true_sigma, ~sim_mean_sd(n = .x, sigma = .y))
  ) %>% #map2 lets sim_mean_sd take two inputs, need to name inputs explicitly
  unnest(estimate_df)
```

``` r
sim_results_df %>%
  mutate(
    sample_size = str_c("N = ", sample_size),
    sample_size = fct_inorder(sample_size)
  ) %>% # violin need x to be factor not number
  ggplot(aes(x = sample_size, y = mu_hat)) +
  geom_violin() + 
  facet_grid(. ~true_sigma)
```

<img src="simulation_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />
