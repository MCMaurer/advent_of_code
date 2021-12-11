library(tidyverse)

m <- scan("2021/day_11/input.txt") 

m <- m %>% 
  paste0(collapse = "") %>% 
  str_split("") %>% 
  unlist() %>% 
  as.numeric() %>% 
  matrix(nrow = length(m), byrow = T)

na_F <- function(x){
  x2 <- if_else(is.na(x), F, x)
  dim(x2) <- dim(x)
  return(x2)
}

get_changes <- function(m){
  t <- apply(m, 2, lag, default = 0)
  b <- apply(m, 2, lead, default = 0)
  l <- t(apply(m, 1, lag, default = 0))
  r <- t(apply(m, 1, lead, default = 0))
  
  tl <- apply(l, 2, lag, default = 0)
  tr <- apply(r, 2, lag, default = 0)
  bl <- apply(l, 2, lead, default = 0)
  br <- apply(r, 2, lead, default = 0)
  
  neighbor_mats <- list(t = t, b = b, l = l, r = r, tl= tl, tr = tr, bl = bl, br = br)
  
  changes <- map(neighbor_mats, ~ .x > 9) %>% 
    map(na_F) %>% 
    reduce(`+`)
  
  return(changes)
}

run_step <- function(d){
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

run_step(ml) %>% 
  run_step()

m2 <- ml

for (i in 1:100) {
  m2 <- run_step(m2)
}

m2$flashes

# part 2

m2 <- ml

for (i in 1:1000) {
  print(i)
  m2 <- run_step(m2)
}
