Day1
================
Johannes Friedrich
12/1/2021

``` r
library(tidyverse)
```

## Puzzle 1

``` r
## read file
input <- scan(file = "input.txt", what = integer())
```

``` r
sum(diff(input) >0)
```

    ## [1] 1759

## Puzzle 2

``` r
library(zoo)
```

``` r
temp <- rollsum(input, 3, align = "center")

sum(diff(temp) >0)
```

    ## [1] 1805
