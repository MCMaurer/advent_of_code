library(tidyverse)

d <- read_table("2021/day_3/input.txt", col_names = "bin")

d <- str_split_fixed(d$bin, "", n = nchar(d$bin[1])) %>% 
  as_tibble() %>% 
  mutate(across(.fns = as.numeric))

# part 1 ------------------------------------------------------------------

x <- colSums(d)/nrow(d)
  
gamma <- round(x, 0) %>% 
  str_c(collapse = "") %>% 
  strtoi(base = 2)

epsilon <- (1-round(x, 0)) %>% 
  str_c(collapse = "") %>% 
  strtoi(base = 2)

gamma*epsilon


# part 2 ------------------------------------------------------------------


do2 <- d

for (i in 1:ncol(do2)) {
  
  x <- (colSums(do2) / nrow(do2)) + 0.0000001
  
  mc <- round(x)
  
  do2 <- do2[do2[, i] == mc[i], ]

if(nrow(do2) == 1) stop("all done")

}

o2_rating <- do2 %>% 
  colSums() %>% 
  str_c(collapse = "") %>% 
  strtoi(base = 2)

dco2 <- d

for (i in 1:ncol(dco2)) {

  x <- (colSums(dco2) / nrow(dco2)) + 0.0000001
  mc <- round(x)
  lc <- 1 - mc
  
  dco2 <- dco2[dco2[,i] == lc[i],]
  
  if(nrow(dco2) == 1) stop("all done")
}

dco2

co2_rating <- dco2 %>% 
  colSums() %>% 
  str_c(collapse = "") %>% 
  strtoi(base = 2)

o2_rating*co2_rating


# nicer approach --------------------------------------------------------

# mostly my work here, a couple tricks from others. reduce(rbind) from Emil Hvitfeldt, reduce() calls at the end from David Robinson

d <- readLines("2021/day_3/input.txt")
d <- d %>% 
  str_split("") %>% 
  map(as.numeric) %>% 
  reduce(rbind)

get_common <- function(d, most = T) {
  x <- (colSums(d) / nrow(d)) + 1e-6
  
  if (!most) {
    x <- 1 - x
  }
  
  round(x, 0) 
}

to_dec <- function(d){
  d %>%
    str_c(collapse = "") %>%
    strtoi(base = 2)
}

ans1 <- to_dec(get_common(d)) * to_dec(get_common(d, F))

ans1

do2 <- d

common_filter <- function(d, i, most = T){
  comm <- get_common(d, most = most)
  
  res <-  d[d[, i] == comm[i], ]
  
  if(!is.matrix(res)){
    done(res)
  } else {
    res
  }
}

o2 <- reduce(1:12, common_filter, .init = d)
co2 <- reduce(1:12, common_filter, .init = d, most = F)

ans2 <- to_dec(o2)*to_dec(co2)
ans2


