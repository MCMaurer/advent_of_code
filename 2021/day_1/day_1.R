library(tidyverse)
library(slider)


# Part 1 ------------------------------------------------------------------


d <- read_table("2021/day_1/input.txt", col_names = "depths") 

d %>% 
  mutate(lag = slide_dbl(depths, ~.x, .before = 1, 
                         .after = -1, .complete = T),
         increase = depths > lag) %>% 
  summarise(num_increases = sum(increase, na.rm = T))


# Part 2 ------------------------------------------------------------------


d %>% 
  mutate(sum_3 = slide_dbl(depths, sum, 
                           .before = 0, .after = 2, 
                           .complete = T),
         lag_sum_3 = slide_dbl(sum_3, ~.x, 
                               .before = 1, .after = -1, 
                               .complete = T),
         increase = sum_3 > lag_sum_3) %>% 
  summarise(num_increases = sum(increase, na.rm = T))
