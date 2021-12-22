Day22
================
Johannes Friedrich
12/22/2021

``` r
library(tidyverse)
library(gmp)
```

``` r
## read file
input <- scan(file = "input.txt", what = character(), sep="\n", blank.lines.skip=FALSE)
```

``` r
data <- input %>% 
  str_split(" ")

commands <- sapply(data, function(x){
  return(c(command = x[1]))
})

coord <- sapply(data, function(x){
  str_split(x[2],",") %>% unlist() %>% 
    str_sub(3) %>% 
    lapply(function(x){str_split(x,"\\.\\.")}) %>% 
    unlist(recursive = FALSE) %>% 
    lapply(function(x){as.integer(x)+50L})
}) %>% 
  split(1:3) %>% 
  setNames(c("x","y","z"))
  
grid <- array(FALSE,dim = c(101,101,101))
```

## Puzzle 1

``` r
for (command in 1:length(commands)){
  if (coord$x[[command]][1] >= 0 & coord$x[[command]][2] <= 101 & coord$y[[command]][1] >= 0 & coord$y[[command]][2] <= 101 & coord$z[[command]][1] >= 0 & coord$z[[command]][2] <= 101){
    if (commands[command] == "on"){
      grid[coord$x[[command]][1]:coord$x[[command]][2], coord$y[[command]][1]:coord$y[[command]][2],coord$z[[command]][1]:coord$z[[command]][2]] <- TRUE
    } else if (commands[command] == "off"){
     grid[coord$x[[command]][1]:coord$x[[command]][2], coord$y[[command]][1]:coord$y[[command]][2],coord$z[[command]][1]:coord$z[[command]][2]] <- FALSE
    }
}
}

length(which(grid == TRUE))
```

    ## [1] 603661
