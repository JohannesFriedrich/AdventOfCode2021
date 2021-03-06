Day14
================
Johannes Friedrich
12/14/2021

``` r
library(tidyverse)
```

``` r
## read file
input <- scan(file = "input.txt", what = character(), sep="\n", blank.lines.skip=FALSE) 

start <- input[1]

recipe <- input[3:length(input)] %>% 
  enframe(name=NULL) %>% 
  separate(value,c("Combo", "Insert"))
```

``` r
split_polymer <- function(string){
  sub <- sapply(1:nchar(string), function(k){
    substring(string,k,k+1)})
  if (nchar(sub[length(sub)]) == 1) return(sub[-length(sub)])
}

find_insert <- function(string){
  recipe %>% 
    filter(Combo == string) %>% 
    pull(Insert)
}

insert_letter <- function(letter, string) {       # Create own function
  gsub("^(.{1})(.*)$",
       paste0("\\1", letter, "\\2"),
       string)
}

combine_string <- function(string){
  ## string1: 
  if (length(string)>=2){
    first <- ifelse(nchar(string[1]) == 3, substring(string[1],1,2), substring(string[1],1,1))
    if (length(string)>2){
      middle <- sapply(string[2:(length(string)-1)], function(x){
        
        ifelse(nchar(x) == 3, substring(x,1,2), substring(x,1,1))
        
      })
      middle <- paste(middle, collapse = "")
    } else {
      middle <- NULL
    }
    
    ## last string
    last <- string[length(string)]
    
    return(paste0(first, middle, last))
  } else {
    return(string)
  }
}

get_new_polymere <- function(string){
  
  split_polymer(string) %>% 
  
  sapply(function(x){
  
    x %>% 
      find_insert() %>% 
      insert_letter(x)}) %>% 
  combine_string()
  
}
```

## Puzzle 1

``` r
result <- start
for (run in 1:10){

  result <- get_new_polymere(result)
}
```

``` r
table <- table(strsplit(result,""))

max(table)-min(table)
```

    ## [1] 2447

## Puzzle 2
