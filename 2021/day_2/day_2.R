library(tidyverse)
library(slider)

d <- read_table("2021/day_2/input.txt", col_names = c("dir", "len"))


# part 1 ------------------------------------------------------------------

d %>% 
  group_by(dir) %>% 
  summarise(total_len = sum(len)) %>% 
  pivot_wider(names_from = dir, values_from = total_len) %>% 
  mutate(ans = (down - up)*forward)


# part 2 loop ------------------------------------------------------------------

aim <- 0
horiz <- 0
depth <- 0

for (i in seq_along(d$dir)) {
  
  if (d$dir[i] == "up"){
    aim <- aim - d$len[i]
  }
  
  if (d$dir[i] == "down"){
    aim <- aim + d$len[i]
  }
  
  if (d$dir[i] == "forward"){
    horiz <- horiz + d$len[i]
    depth <- depth + d$len[i] * aim
  }
  
}

horiz*depth

# part 2 tidy -------------------------------------------------------------

d %>% 
  mutate(len = if_else(dir == "up", -len, len),
         aim = if_else(dir == "forward", 0, len),
         cumu_aim = slide_sum(aim, before = Inf)) %>% 
  filter(dir == "forward") %>% 
  summarise(depth = sum(len*cumu_aim),
            horiz = sum(len),
            ans = depth*horiz)


