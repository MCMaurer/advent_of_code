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


# shortest -------------------------------------------------------------------------

sum(lag(d$depths) < d$depths, na.rm = T)

d <- d %>% 
  mutate(sum_3 = slide_dbl(depths, sum, 
                           .before = 0, .after = 2, 
                           .complete = T))

sum(lag(d$sum_3) < d$sum_3, na.rm = T)


# Emil Hvitfeldt's approach -------------------------------------------------

input <- scan("2021/day_1/input.txt")

sum(diff(input, lag = 1) > 0)

sum(diff(input, lag = 3) > 0)

# you don't actually need to compare the whole sums of the windows. Two overlapping windows share all the intermediate values, it's just the first and last values that actually differ, so you can just compare those directly. So you just get the differences between lagged values