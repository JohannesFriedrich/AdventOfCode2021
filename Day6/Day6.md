Day6
================
Johannes Friedrich
12/6/2021

``` r
library(tidyverse)
library(gmp)
```

``` r
## read file
input <- scan(file = "input.txt", what = character(), sep = "\n") %>% 
  str_split(",") %>% 
  unlist() %>% 
  as.integer()
```

``` r
## Because we can not be sure that always 1-5 is in the starting values
table_template <- tibble(
  value = c(0L:8L),
  n = rep(0L,9)
)

table <- input %>% 
  as.tibble() %>% 
  count(value) %>% 
  mutate(n = as.integer(n),
         value = as.integer(value))
```

    ## Warning: `as.tibble()` was deprecated in tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

``` r
table <- union(table_template, table) %>% 
  group_by(value) %>% 
  summarise(n=sum(n))
```

## Puzzle 1

``` r
data <- as.bigz(table$n)
# 0 114  47  51  36  52   0   0   0

generation_generator <- function(days){
  for (day in 1:days){
    ## get new fishes from pos [1]
    new_fishes <- data[1]
    
    ## remove 0 and shift everything -1
    # 114  47  51  36  52   0   0   0
    data <- data[-1]
    
    ## add new fishes at pos 7 
    # 114  47  51  36  52   0   0+new_fishes   0
    data[7] <- data[7]+new_fishes
    
    ## add new_fishes to new 9th position
    # 114  47  51  36  52   0   0+new_fishes   0 new_fishes
    data <- c(data,new_fishes)
  }
  return(sum(data))
}
```

``` r
generation_generator(80)
```

    ## Big Integer ('bigz') :
    ## [1] 361169

## Puzzle 2

``` r
generation_generator(256)
```

    ## Big Integer ('bigz') :
    ## [1] 1634946868992
