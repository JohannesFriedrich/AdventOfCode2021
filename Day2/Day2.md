Day2
================
Johannes Friedrich
12/2/2021

``` r
library(tidyverse)
```

## Puzzle 1

``` r
## read the file

input <- scan(file = "input.txt", what = character(), sep = "\n") %>% 
  str_split(" ") %>% 
  map_dfr( ~as.data.frame(t(.x))) %>% 
  rename(Command=V1, Value=V2) %>% 
  mutate(Value= as.numeric(Value))
```

``` r
forward <- input %>% 
   filter(Command == "forward") %>% 
   summarise(sum = sum(Value)) %>% 
   select(sum)

up <- input %>% 
   filter(Command == "up") %>% 
   summarise(sum = sum(Value)) %>% 
   select(sum)

down <- input %>% 
   filter(Command == "down") %>% 
   summarise(sum = sum(Value)) %>% 
   select(sum)

forward$sum * (down$sum - up$sum)
```

    ## [1] 1451208

## Puzzle 2

``` r
forward <- 0
down <- 0
aim <- 0

for(i in 1:(nrow(input))){
  if (input$Command[i] == "forward"){
    forward <- forward + input$Value[i]
    down <- down + aim*input$Value[i]
  }
  if (input$Command[i] == "up"){
    aim <- aim - input$Value[i]
  }
    if (input$Command[i] == "down"){
    aim <- aim + input$Value[i]
  }
}

forward*down
```

    ## [1] 1620141160
