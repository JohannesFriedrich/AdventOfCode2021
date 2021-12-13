Day13
================
Johannes Friedrich
12/13/2021

``` r
library(tidyverse)
```

``` r
## read file
input <- scan(file = "input.txt", what = character(), sep="\n", blank.lines.skip=FALSE)

fold <- which(input == "")

coord <- input[1:(fold-1)] %>%  
  str_split(",") %>% 
  sapply(function(x){as.integer(x)+1L}) %>% t()
coord <- coord[,c(2,1)]

folds <- input[(fold+1):length(input)] %>% 
  str_remove("fold along ") %>% 
  str_split("=") %>% 
  sapply(function(x){
    t <- as.integer(x[2])
    names(t) <- x[1]
    return(t)
  })

x_max <- folds[which(names(folds)=="x")[1]]
y_max <- folds[which(names(folds)=="y")[1]]

grid <- matrix(0L, nrow = 2*y_max+1, ncol = 2*x_max+1)


## fill with 1
grid[coord] <- 1L
```

``` r
matrix <- grid
fold <- folds[1]
fold_matrix <- function(matrix,fold){
  
  if (names(fold) == "x"){
    
    left_matrix <- matrix[,1:fold]
    right_matrix <- matrix[,ncol(matrix):(ncol(matrix)-fold+1)]
    return(left_matrix+right_matrix)
    
  } else if (names(fold) == "y"){
    upper_matrix <- matrix[1:fold,]
    lower_matrix <- matrix[nrow(matrix):(nrow(matrix)-fold+1),]
    return(upper_matrix+lower_matrix)
  }
}
```

## Puzzle 1

``` r
result <- function(matrix, fold, puzzle1){
  
  for (fold in 1:length(folds)){
    matrix <- fold_matrix(matrix, folds[fold])
    if (fold == 1 & puzzle1 == TRUE) return(length(matrix[which(matrix>=1)]))
  }
  
  return(matrix)
  
}
```

``` r
matrix <- grid
result(matrix, folds, puzzle1 = TRUE)
```

    ## [1] 687

## Puzzle 2

``` r
matrix <- grid
result(matrix, folds, puzzle1 = FALSE)
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
    ## [1,]    3   40   22    8    0    0   14   12    0     0     2     0     0    25
    ## [2,]    5    0    0    0    0    5    0    0    5     0     1     0     2     0
    ## [3,]   12   10   34    0    0    2    0    0    0     0    19     5     0     0
    ## [4,]    1    0    0    0    0   11    0   12    6     0     4     0     8     0
    ## [5,]    5    0    0    0    0    3    0    0   14     0     5     0     9     0
    ## [6,]    6    0    0    0    0    0   10    6    6     0     6     0     0     1
    ##      [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26]
    ## [1,]     0     0     9     6     0     0    14     0     0     4     0     2
    ## [2,]     0     1     0     0     5     0     8     0    12     0     0    12
    ## [3,]     0     2     0     0     0     0    26     8     0     0     0     2
    ## [4,]     0    11     0     0     0     0    10     0     7     0     0     6
    ## [5,]     0     9     0     0     3     0     6     0     9     0     0     9
    ## [6,]     0     0     1    18     0     0    14     0     0     4     0     9
    ##      [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35] [,36] [,37] [,38]
    ## [1,]    13     6     0     0    10     4    13     3     0     0     9    19
    ## [2,]     0     0     7     0     0     0     0     2     0     2     0     0
    ## [3,]     2     3     0     0     0     0     4     0     0     2     0     0
    ## [4,]     0     0    36     0     0     4     0     0     0     1     0    10
    ## [5,]     0     0    12     0     3     0     0     0     0     9     0     0
    ## [6,]     1    21     0     0    18    14     4     7     0     0     9     3
    ##      [,39] [,40]
    ## [1,]     0     0
    ## [2,]     1     0
    ## [3,]     0     0
    ## [4,]     7     0
    ## [5,]     4     0
    ## [6,]     6     0

``` r
## FGKCKBZG
```
