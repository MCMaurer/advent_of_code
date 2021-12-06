library(tidyverse)
d <- read_csv("2021/day_6/input.txt", col_names = F) %>% 
  pivot_longer(everything()) %>% 
  pull(value)

dd <- 8-d

mat_pow = function(x, n) Reduce(`%*%`, replicate(n, x, simplify = FALSE))

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
sum(res)

format(sum(res), scientific = F)

