Day4
================
Johannes Friedrich
12/4/2021

``` r
library(tidyverse)
```

``` r
## read file

tables <- read_table2("input.txt", col_names = FALSE, 
     col_types = cols(X1 = col_integer(), 
         X2 = col_integer(), X3 = col_integer(), 
         X4 = col_integer(), X5 = col_integer()), 
     skip = 1) %>% 
  split(rep(1:100,each=5))

## get numbers to play Bingo 
con <- file("input.txt","r") 
numbers <-  con%>% 
  readLines(n=1) %>% 
  str_split(",") %>% 
  unlist() %>% 
  as.integer()

close(con)
```

``` r
tables_mat <- tables_mat_remove <- lapply(1:length(tables), function(x){
  as.matrix(tables[[x]])
})

tables_empty <- lapply(1:length(tables), function(x){
  matrix(0,5,5)
})

## check if this table wins by adding rows and cols and check if 5
check_table <- function(table){
  if(any(rowSums(table) == nrow(table)) | any(colSums(table) == ncol(table))){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

## calculate winning numbers
get_winning_numbers <- function(table, number){
  return(sum(table) * number)
}

## add 1 to the parallel matrix or 0, if number was found on index
manipulate_matrix <- function(table, index, number){
  table[index[,1],index[,2]] <- number
  return(table)
}
```

## Puzzle 1

``` r
bEnded <- FALSE

for (number in numbers){
  ## find number in all tables
  for (table in 1:length(tables_mat)){
    index <- which(tables_mat[[table]] == number, arr.ind = TRUE)
    if(nrow(index) != 0){
      ## set to 1 in parallel matrix and to 0 in copy of initial matrix
        tables_empty[[table]] <- manipulate_matrix(tables_empty[[table]], index,1)
        tables_mat_remove[[table]] <- manipulate_matrix(tables_mat_remove[[table]] , index,0)
    }
    
    if (check_table(tables_empty[[table]])){
      # check winner -> break if found
      winning_number <- get_winning_numbers(tables_mat_remove[[table]], number)
      
      bEnded <- TRUE
    }
    if (bEnded) break
  }
  if (bEnded) break
}
```

``` r
print(winning_number)
```

    ## [1] 58374

## Puzzle 2

``` r
## get the initial matrices again
tables_mat <- tables_mat_remove <- lapply(1:length(tables), function(x){
  as.matrix(tables[[x]])
})

tables_empty <- lapply(1:length(tables), function(x){
  matrix(0,5,5)
})

is_winner <- rep(1,length(tables))
bEnded <- FALSE

for (number in numbers){
  
  ## find number in all tables
  for (table in 1:length(tables_mat)){
    index <- which(tables_mat[[table]] == number, arr.ind = TRUE)
    if(nrow(index) != 0){
      ## set to 1 in parallel matrix and to 0 in copy of initial matrix
        tables_empty[[table]] <- manipulate_matrix(tables_empty[[table]], index,1)
        tables_mat_remove[[table]] <- manipulate_matrix(tables_mat_remove[[table]] , index,0)}
    
    if (check_table(tables_empty[[table]])){
      # check winner -> break if found
      is_winner[[table]] <- 0
      
      if (sum(is_winner) == 0){
        ## all matrices are filled -> find winner
        print(table)
        winning_number <- get_winning_numbers(tables_mat_remove[[table]], number)
        bEnded <- TRUE
        break
      }
    }
    if (bEnded) break
  }
  if (bEnded) break
}
```

    ## [1] 17

``` r
print(winning_number)
```

    ## [1] 11377
