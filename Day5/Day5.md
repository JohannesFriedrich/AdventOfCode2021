Day5
================
Johannes Friedrich
12/5/2021

``` r
library(tidyverse)
```

``` r
## read file

input <- scan(file = "input.txt", what = character(), sep = "\n") %>% 
  str_split(" -> ") %>% 
  unlist() %>% 
  str_split(",") %>% 
  unlist() %>% 
  as.integer() %>% 
  matrix(ncol = 4, byrow = TRUE)
```

``` r
max_grid_dim <- c(max(input[,1], input[,3]), max(input[,2], input[,4]))

grid <- matrix(0,ncol=max_grid_dim[2]+1, nrow = max_grid_dim[1]+1)

get_int_points <- function(x1,y1,x2,y2){
    return(cbind(x1:x2,y1:y2))
}
```

## Puzzle 1

``` r
## just horizontal & vertical lines
hor_ver <- input[which(input[,1] == input[,3] |input[,2] == input[,4]),]

## loop over all entries of hor_ver
for (row in 1:nrow(hor_ver)){
  ## get all intermediate points
  points <- get_int_points(hor_ver[row,1],hor_ver[row,2],hor_ver[row,3],hor_ver[row,4])
  
  ## tick these intermediate points
  for (point in 1:nrow(points)){
    # add plus 1 because puzzle starts at 0
    grid[points[point,1]+1, points[point,2]+1] <- grid[points[point,1]+1, points[point,2]+1] + 1
  }
}
```

``` r
length(which(grid >= 2))
```

    ## [1] 6267

## Puzzle 2

``` r
## reset grid
grid <- matrix(0,ncol=max_grid_dim[2]+1, nrow = max_grid_dim[1]+1)
```

``` r
## now loop over all rows
for (row in 1:nrow(input)){
  ## get all intermediate points
  points <- get_int_points(input[row,1],input[row,2],input[row,3],input[row,4])
  
  ## tick these intermediate points
  for (point in 1:nrow(points)){
    # add plus 1 because puzzle starts at 0
    grid[points[point,1]+1, points[point,2]+1] <- grid[points[point,1]+1, points[point,2]+1] + 1
  }
}
```

``` r
length(which(grid >= 2))
```

    ## [1] 20196
