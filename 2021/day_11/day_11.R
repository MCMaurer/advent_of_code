library(tidyverse)

m <- scan("2021/day_11/input.txt")

m <- m %>% 
  paste0(collapse = "") %>% 
  str_split("") %>% 
  unlist() %>% 
  as.numeric() %>% 
  matrix(nrow = length(m), byrow = T)

make_neighbor_mats <- function(m, neighbors = 9){
  N <- apply(m, 2, lag, default = 0)
  S <- apply(m, 2, lead, default = 0)
  W <- t(apply(m, 1, lag, default = 0))
  E <- t(apply(m, 1, lead, default = 0))
  
  if(neighbors == 4){
    return(list(N = N, S = S, W = W, E = E))
  } else {
    NW <- apply(W, 2, lag, default = 0)
    NE <- apply(E, 2, lag, default = 0)
    SW <- apply(W, 2, lead, default = 0)
    SE <- apply(E, 2, lead, default = 0)
    
    return(list(N = N, S = S, W = W, E = E, NW = NW, NE = NE, SW = SW, SE = SE))
  }
}

get_changes <- function(m){
  
  make_neighbor_mats(m) %>% 
    map(~.x > 9) %>% 
    map(replace_na, F) %>% 
    reduce(`+`)
}

run_step <- function(d, dummy){
  
  print(dummy)
  
  m <- d$m
  flashes <- d$flashes
  
  m <- m + 1
  
  while(length(m[m > 9 & !is.na(m)]) > 0){
    changes <- get_changes(m)
    m[m > 9] <- NA
    m <- m + changes
  }
  
  if(length(m[is.na(m)]) == length(m)){
    stop("they all flashed")
  }
  flashes <- length(m[is.na(m)]) + flashes
  m[is.na(m)] <- 0
  
  return(list(m = m, flashes = flashes))
}

ml <- list(m = m, flashes = 0)

reduce(1:100, run_step, .init = ml) %>% 
  pluck("flashes")

# part 2

reduce(1:1000, run_step, .init = ml)