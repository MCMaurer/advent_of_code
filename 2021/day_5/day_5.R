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

get_coords <- function(x1, y1, x2, y2, dir){
  if(dir == "v") x <- tibble(x = x1:x2, y = y1)
  if(dir == "h") x <- tibble(x = x1, y = y1:y2)
  if(dir == "d") x <- tibble(x = x1:x2, y = y1:y2)
  return(x)
}

# part 1 ------------------------------------------------------------------

d %>% 
  filter(dir != "d") %>% 
  rowwise() %>% 
  mutate(coords_covered = list(get_coords(x1, y1, x2, y2, dir))) %>% 
  unnest_wider(coords_covered) %>% 
  unnest(cols = c(x,y)) %>% 
  count(x, y, name = "n_overlaps") %>% 
  count(n_overlaps, name = "n_points") %>% 
  filter(n_overlaps > 1) %>% 
  summarise(ans = sum(n_points))


# part 2 ------------------------------------------------------------------

d %>%
  rowwise() %>% 
  mutate(coords_covered = list(get_coords(x1, y1, x2, y2, dir))) %>% 
  unnest_wider(coords_covered) %>% 
  unnest(cols = c(x,y)) %>% 
  count(x, y, name = "n_overlaps") %>% 
  count(n_overlaps, name = "n_points") %>% 
  filter(n_overlaps > 1) %>% 
  summarise(ans = sum(n_points))


d %>%
  rowwise() %>% 
  mutate(coords_covered = list(get_coords(x1, y1, x2, y2, dir))) %>% 
  unnest_wider(coords_covered) %>% 
  unnest(cols = c(x,y)) %>% 
  group_by(x,y) %>% 
  mutate(n_overlaps = n()) %>% 
  ggplot(aes(x = x, y = y, color = n_overlaps, alpha = n_overlaps)) +
  geom_point() +
  scale_alpha_continuous(range = c(0.02, 0.3)) +
  scale_color_viridis_c(option = "A", direction = -1) +
  theme_bw()

ggsave("2021/day_5/danger_grid.jpg")

d %>%
  rowwise() %>% 
  mutate(coords_covered = list(get_coords(x1, y1, x2, y2, dir))) %>% 
  unnest_wider(coords_covered) %>% 
  unnest(cols = c(x,y)) %>% 
  group_by(x,y) %>% 
  mutate(n_overlaps = n()) %>% 
  ggplot(aes(x = x, y = y, fill = n_overlaps)) +
  geom_raster() +
  scale_fill_viridis_c(option = "A", direction = -1) +
  theme_bw()



# Even tidier -------------------------------------------------------------

# inspired by David Robinson's approach

d <- read_delim("2021/day_5/input.txt", 
                delim = " -> ", 
                col_names = c("c1", "c2"),
                col_types = "cc") %>% 
  separate(c1, into = c("x1", "y1")) %>% 
  separate(c2, into = c("x2", "y2")) %>% 
  mutate(x = map2(x1, x2, seq),
         y = map2(y1, y2, seq))

d %>% 
  filter(x1 == x2 | y1 == y2) %>% 
  unnest(c(x,y)) %>% 
  count(x,y) %>% 
  summarise(sum(n>1))

d %>% 
  unnest(c(x,y)) %>% 
  count(x,y) %>% 
  summarise(sum(n>1))
  