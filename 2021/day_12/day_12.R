library(tidyverse)
library(tidygraph)

# strategy:
# join the dataframe to itself
# check to see if any have hit "end". remove those routes, add them to "finished routes".
# check to see if any have gone back to "start". remove those routes.

# check to see if any have repeated a small letter:
# this one is trickier. might have to keep a column that is the "current route", and then to str_detect. something like "if the proposed step is lowercase, detect whether it is already in the "current route". If so, discard that route


routes <- read_table("2021/day_12/input.txt", col_names = "segs") %>% 
  separate(segs, c("from", "to"))

routes_rev <- routes %>%
  mutate(new_start = to, new_end = from) %>%
  select(from = new_start, to = new_end)

routes <- bind_rows(routes, routes_rev)

d <- routes %>% 
  filter(from == "start") %>% 
  mutate(route = paste(from, to, sep = ",")) %>%
  select(route, to)

good_routes <- NULL

while (nrow(d) > 0) {
  
  d <- d %>% 
    filter(to != "end") %>% 
    rename(from = to) %>% 
    left_join(routes, by = "from")
  
  d <- d %>% 
    mutate(ok_routes = case_when(
      to == "end" ~ TRUE,
      to == "start" ~ FALSE,
      str_detect(to, "[A-Z]") ~ TRUE,
      str_detect(route, to) ~ FALSE,
      TRUE ~ TRUE
    )) %>% 
    filter(ok_routes) %>% 
    mutate(route = paste(route, to, sep = ",")) %>% 
    select(route, to)
  
  good_routes <- d %>% 
    filter(to == "end") %>% 
    select(route) %>% 
    bind_rows(good_routes)
}


nrow(good_routes)

# part 2 ------------------------------------------------------------------

d <- routes %>% 
  filter(from == "start") %>% 
  mutate(route = paste(from, to, sep = ","),
         small_twice = F) %>%
  select(route, small_twice, to)

good_routes <- NULL

while (nrow(d) > 0) {

  d <- d %>%
    filter(to != "end") %>%
    rename(from = to) %>%
    left_join(routes, by = "from")

  d <- d %>%
    select(route, small_twice, to)

  d <- d %>%
    mutate(ok_routes = case_when(
      to == "end" ~ TRUE,
      to == "start" ~ FALSE,
      str_detect(to, "[A-Z]") ~ TRUE,
      small_twice & str_detect(route, paste0(",", to)) ~ FALSE,
      TRUE ~ TRUE
    )) %>%
    filter(ok_routes) %>%
    mutate(route = paste(route, to, sep = ",")) %>%
    mutate(is_to_small = str_detect(to, "[a-z]") & to != "start" & to != "end",
           to_end_start = to %in% c("end", "start"),
           to_count = str_count(route, paste0(",", to)),
           small_twice = case_when(
             !small_twice & is_to_small & !to_end_start & to_count == 2 ~ TRUE,
             TRUE ~ small_twice
           )) %>%
    select(route, small_twice, to)

  good_routes <- d %>%
    filter(to == "end") %>%
    select(route) %>%
    bind_rows(good_routes)
}

nrow(good_routes)
