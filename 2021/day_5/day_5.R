library(tidyverse)

d <- read_delim("2021/day_5/input.txt", 
                delim = " -> ", 
                col_names = c("c1", "c2"),
                col_types = "cc") %>% 
  separate(c1, into = c("x1", "y1")) %>% 
  separate(c2, into = c("x2", "y2")) %>% 
  mutate(across(.fns = as.numeric)) %>% 
  mutate(dir = case_when(
    x1 == x2 ~ "h",
    y1 == y2 ~ "v",
    TRUE ~ "d"
  ))


# part 1 ------------------------------------------------------------------

dhv <- d %>% 
  filter(dir != "d")

get_coords <- function(x1, y1, x2, y2, dir){
  if(dir == "v") x <- tibble(x = x1:x2, y = y1)
  if(dir == "h") x <- tibble(x = x1, y = y1:y2)
  return(x)
}

dhv %>% 
  rowwise() %>% 
  mutate(coords_covered = list(get_coords(x1, y1, x2, y2, dir))) %>% 
  unnest_wider(coords_covered) %>% 
  unnest(cols = c(x,y)) %>% 
  count(x, y, name = "n_overlaps") %>% 
  count(n_overlaps, name = "n_points") %>% 
  filter(n_overlaps > 1) %>% 
  summarise(sum(n_points))


# part 2 ------------------------------------------------------------------

get_coords <- function(x1, y1, x2, y2, dir){
  if(dir == "v") x <- tibble(x = x1:x2, y = y1)
  if(dir == "h") x <- tibble(x = x1, y = y1:y2)
  if(dir == "d") x <- tibble(x = x1:x2, y = y1:y2)
  return(x)
}

d %>%
  rowwise() %>% 
  mutate(coords_covered = list(get_coords(x1, y1, x2, y2, dir))) %>% 
  unnest_wider(coords_covered) %>% 
  unnest(cols = c(x,y)) %>% 
  count(x, y, name = "n_overlaps") %>% 
  count(n_overlaps, name = "n_points") %>% 
  filter(n_overlaps > 1) %>% 
  summarise(sum(n_points))
