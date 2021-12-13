library(tidyverse)

d <- read_lines("2021/day_13/input.txt")

instructions <- d[str_detect(d, "fold")] %>% 
  tibble(x = .) %>% 
  mutate(x = str_remove(x, "fold along ")) %>% 
  separate(x, c("axis", "value"), convert = T)

d <- d[str_detect(d, ",")] %>% 
  tibble(x = .) %>% 
  separate(x, c("x", "y"), convert = T)

do_fold <- function(d, dir, val){
  d[dir] <- val - abs(d[dir] - val)
  return(distinct(d))
}

# part 1
do_fold(d, folds$axis[1], folds$value[1]) %>% 
  nrow()

# part 2
reduce2(folds$axis, folds$value, do_fold, .init = d) %>% 
  ggplot(aes(x = x, y = -y)) +
  geom_tile() +
  coord_fixed()



