Day21
================
Johannes Friedrich
12/21/2021

``` r
library(tidyverse)
```

``` r
## read file
```

``` r
dice <- rep(1:100,21)
players <- split(lapply(split(dice, ceiling(seq_along(dice)/3)), sum),1:2)

calc_points <- function(player){
  start <- start[player]
  points <- unname((start+cumsum(unlist(players[[player]])))%%10)
  points[which(points ==0)] <- 10
  points <- cumsum(points)
}

get_result <- function(player_1, player_2){
  if (length(which(player_1 <= 1000)) < length(which(player_2 <= 1000))){
    ## player 1 wins
    print("Player 1 won!")
    die_rolled <- length(which(player_1 <= 1000))*3*2-3
    looser_points <- player_2[length(which(player_1 <= 1000))-1]
  } else {
    print("Player 2 won!")
    die_rolled <- (length(which(player_2 <= 1000))+1)*3*2
    looser_points <- player_1[length(which(player_2 <= 1000))+1]
  }
  
  return(die_rolled*looser_points)
}
```

## Puzzle 1

``` r
start <- c(1,3)

points_p1 <- calc_points(1)
points_p2 <- calc_points(2)
get_result(points_p1,points_p2)
```

    ## [1] "Player 2 won!"

    ## [1] 897798
