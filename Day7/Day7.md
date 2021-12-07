Day7
================
Johannes Friedrich
12/7/2021

``` r
library(tidyverse)
library(parallel)
```

``` r
## read file
input <- scan(file = "input.txt", what = character()) %>% 
  str_split(",") %>% 
  unlist() %>% 
  as.integer()
```

``` r
df <-   input %>% 
  as_tibble() %>% 
  mutate(id = row_number())

get_differences <- function(fuel){
  df %>%
  mutate(fuel = fuel,
         diff = abs(value-fuel)) %>% 
  summarise(sum_diff = sum(diff)) %>% 
  pull(sum_diff)
}
```

## Puzzle 1

``` r
## brute force
parallel::mclapply(1:1000, get_differences) %>% 
  unlist() %>% 
  min()
```

    ## [1] 344735

## Puzzle 2

``` r
get_differences <- function(fuel){
  df %>%
  mutate(fuel = fuel,
         diff = abs((value-fuel))) %>% 
    rowwise() %>% 
    mutate(cumsum = sum(0:diff)) %>% 
    ungroup() %>% 
    summarise(sum = sum(cumsum)) %>% 
  pull(sum)
}
```

``` r
## brute force
parallel::mclapply(1:1000, get_differences) %>% 
  unlist() %>% 
  min()
```

    ## [1] 96798233
