library(tidyverse)

d <- read_csv("2021/day_6/input.txt", col_names = F) %>% 
  pivot_longer(everything()) %>% 
  pull(value)

dd <- 8-d

mat_pow <- function(x, n) reduce(replicate(n, x, simplify = F), `%*%`)

c(0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 ,
  1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
  0 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 1 ,
  0 , 0 , 1 , 0 , 0 , 0 , 0 , 0 , 0 ,
  0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 , 0 ,
  0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 ,
  0 , 0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 
  0 , 0 , 0 , 0 , 0 , 0 , 1 , 0 , 0 ,
  0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 0) %>% 
  matrix(., nrow = 9, byrow = T) -> m

# more efficient way to make matrix, I just like the above for visualization

m <- matrix(0, nrow = 9, ncol = 9)
m[cbind(2:9, 1:8)] <- 1
m[c(1,3), 9] <- 1

init <- table(dd) %>% 
  as_tibble() %>% 
  mutate(dd = as.numeric(dd)) %>% 
  bind_rows(tibble(dd = (0:8)[!(0:8 %in% dd)], n = 0)) %>% 
  arrange(dd) %>% 
  pull(n)

# part 1
res <- (mat_pow(m, 80) %*% init)
sum(res)

# part 2
res <- (mat_pow(m, 256) %*% init)

format(sum(res), scientific = F)

