library(tidyverse)

d <- read_lines("2021/day_9/input.txt")

n_col <- str_length(d[1])
n_row <- length(d)
n_row
n_col

d <- d %>% 
  str_c(collapse = "") %>% 
  str_split("") %>% 
  pluck(1) %>% 
  as.numeric() %>% 
  matrix(nrow = n_row, byrow = T)

# taking a naive approach to start

less_than <- function(x, y){
  if(is_empty(y)){
    ans <- TRUE
  } else {
    ans <- x < y
  }
  return(ans)
}

is_min <- function(row, col, d){
  f <- d[row, col]
  a <- tryCatch(d[row+1, col], error = function(e) e <- numeric(0))
  b <- tryCatch(d[row-1, col], error = function(e) e <- numeric(0))
  l <- tryCatch(d[row, col-1], error = function(e) e <- numeric(0))
  r <- tryCatch(d[row, col+1], error = function(e) e <- numeric(0))

  less_than(f, a) & less_than(f, b) & less_than(f, l) & less_than(f, r)
}

x <- rep(1:n_row, n_col)

y <- rep(1:n_col, n_row) %>% sort()

mins <- map2_lgl(x, y, is_min, d = d)

sum(d[mins]+1)


# part 2 ------------------------------------------------------------------

# why reinvent the wheel?
library(raster)

m <- d
m[m != 9] <- 1
m[m == 9] <- 0

r <- raster(m)
plot(r)
regions <- clump(r, directions = 4)
plot(regions)

freq(regions) %>% 
  as_tibble() %>% 
  filter(!is.na(value)) %>% 
  slice_max(count, n = 3) %>% 
  pull(count) %>% 
  prod()
