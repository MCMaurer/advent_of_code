library(tidyverse)
library(expm)

input <- scan("2021/day_6/input.txt", sep = ",") %>% 
  factor(levels = 0:8) %>% 
  table() %>% 
  as.numeric() %>% 
  rev()

m <- matrix(0, nrow = 9, ncol = 9)
m[cbind(2:9, 1:8)] <- 1
m[c(1,3), 9] <- 1

(m %^% 80 %*% init) %>% 
  sum()

# part 2
options(scipen = 999)

(m %^% 256 %*% init) %>% 
  sum()

(m %^% 8080 %*% init) %>% 
  sum()

# other approaches --------------------------------------------------------

# another way to represent the matrix. You can get here by using dput(matrix(0, ncol = 9, nrow = 9)) and then just breaking it into lines. You can then manually change entries, which can be a nice visual check

m <- structure(c(
  0, 0, 0, 0, 0, 0, 0, 0, 1, 
  1, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 1, 0, 0, 0, 0, 0, 0, 1, 
  0, 0, 1, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 1, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 1, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 1, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 1, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 1, 0), 
  .Dim = c(9L, 9L))


# you can also just use reduce() here instead of only on the matrix
reduce(1:80, ~ m %*% .x, .init = init) %>% 
  sum()

# eigenvalue decomp approach
matrix.power <- function(A, n) {
  e <- eigen(A)
  return(e$vectors %*% diag(e$values^n) %*% solve(e$vectors) %>% Re())
}

counts <- init

microbenchmark::microbenchmark(
  matrix.power(m, 256) %*% init,
  reduce(1:256, ~ m %*% .x, .init = init),
  (m %^% 256) %*% init,
  for (i in seq_len(256)) {
    n0 <- counts[9]
    counts[-1] <- counts[-9]
    counts[3] <- counts[3] + n0
    counts[1] <- n0
  }
)

# matrix multiplication is faster here because R uses some good C libraries for matrix algebra, whereas for loops in R are relatively slow. However, this probably isn't going to be true in a compiled language, since matrix operations are N^3 or whatever, or "The algorithm uses O(log2(k)) matrix multiplications." according to expm. Whereas the hash map or vector + for loop approach is O(N). I think. 

