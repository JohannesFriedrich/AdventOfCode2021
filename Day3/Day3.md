Day3
================
Johannes Friedrich
12/3/2021

``` r
library(tidyverse)
```

## Puzzle 1

``` r
## read file

input <- scan(file = "input.txt", what = character(), sep = "\n") %>% 
  map_dfr( ~as_tibble(t(.x))) %>% 
  rename(Value=V1) %>% 
  separate(Value, paste0("Bit", 1:12), sep= c(1:11), convert = TRUE)
```

``` r
compare <- function(col, bit2count){
    
    if(sum(col) == length(col)/2){
      if(bit2count == "mcb") return(1)
      else if (bit2count == "lcb") return(0)
    }
  
    if(sum(col) > length(col)/2) { ## more 1 than 0
      if (bit2count == "mcb") return(1)
      else if (bit2count == "lcb") return(0)
  
    } else { ## more 0 than 1
      if (bit2count == "mcb") return(0)
      else if (bit2count == "lcb") return(1)
    }
}

toDec <- function(bin){
  Reduce(function(x,y) x*2+y, bin)
}
```

``` r
gamma_rate <- input %>% 
  summarise(across(1:12, ~ compare(.x,"mcb"))) %>% 
  slice_head() %>% 
  unlist(., use.names=FALSE) %>% 
  toDec()

epsilon_rate <- input %>% 
  summarise(across(1:12, ~ compare(.x,"lcb"))) %>% 
  slice_head() %>% 
  unlist(., use.names=FALSE) %>% 
  toDec()

gamma_rate * epsilon_rate
```

    ## [1] 1131506

## Puzzle 2

``` r
colname <- names(input)
copy <- input

## oxygen
bit <- 0
while (nrow(copy) >1){
  bit <- bit + 1
  
  mcb <- compare(copy[[colname[bit]]], "mcb")
  
  copy <- copy %>% 
    filter(.data[[colname[bit]]] == mcb)
  if (nrow(copy)==1) {
    oxygen <- copy %>% 
      slice_head() %>% 
      unlist(., use.names=FALSE) %>% 
      toDec()
    break
    }
}
```

``` r
copy <- input
bit <- 0
while (nrow(copy) >1){
  bit <- bit + 1
  
  lcb <- compare(copy[[colname[bit]]], "lcb")
  
  copy <- copy %>% 
    filter(.data[[colname[bit]]] == lcb)
  
  if (nrow(copy)==1) {
    CO2 <- copy %>% 
      slice_head() %>% 
      unlist(., use.names=FALSE) %>% 
      toDec()
    break
    }
}
```

``` r
oxygen * CO2
```

    ## [1] 7863147
