Day10
================
Johannes Friedrich
12/10/2021

``` r
library(tidyverse)
```

``` r
## read file
input <- scan(file = "input.txt", what = character(), sep="\n", blank.lines.skip=FALSE) %>% 
  str_split("")
```

``` r
brackets <- data_frame(open = c("(","[","{", "<"),
                       close =c (")", "]", "}", ">"),
                      points = c(3L,57L,1197L,25137L),
                      points_p2 = c(1L,2L,3L,4L))
```

    ## Warning: `data_frame()` was deprecated in tibble 1.1.0.
    ## Please use `tibble()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

``` r
expect_next_close <- function(current, queue){
  
  if (current %in% brackets$open){
    queue <- c(brackets$open[which(brackets$open %in% current)], queue)
  } else {
    if (brackets$open[which(current == brackets$close)] != queue[1]){
      ## ERROR
      return(brackets$points[which(brackets$close %in% current)])
    } else {
      ## Remove from queue
      queue <- queue[-1]
    }
  }
  return(queue)
}

#### 
lines <- function(line){
  opening_queue <- c()

  for (element in line){
    opening_queue <- expect_next_close(element, opening_queue)
    if (is.numeric(opening_queue)) break
  }
  #if (!is.numeric(opening_queue)) opening_queue <- 0
  return(opening_queue)
}
```

## Puzzle 1

``` r
temp <-sapply(input,lines)
sum(as.integer(unlist(temp)),na.rm = TRUE)
```

    ## Warning: NAs introduced by coercion

    ## [1] 392043

## Puzzle 2

``` r
complete_brackets <- function(line){
  if (is.integer(line)) return(NA)
  counter <- 0
  line_points <- 0
  for (element in line){
    counter <- counter + 1
    line_points <- line_points * 5 + brackets$points_p2[which(brackets$open %in% element)]
  }
  return(max(line_points))
}
```

``` r
## use temp from above
median(sapply(temp, complete_brackets), na.rm = TRUE)
```

    ## [1] 1605968119
