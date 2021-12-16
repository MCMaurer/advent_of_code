library(tidyverse)
library(expm)

start <- read_lines("2021/day_14/input.txt") %>% 
  pluck(1)

rules <- read_delim("2021/day_14/input.txt", delim = " -> ", 
                    skip = 2, col_names = c("pair", "insert")) %>% 
  mutate(pairs = str_split(pair, "")) %>% 
  unnest_wider(pairs) %>% 
  mutate(new = paste0(...1, insert, ...2)) %>% 
  select(pair, new)



do_insertion <- function(start, dummy, rules){
  print(dummy)
  d <- start %>% 
    str_split("") %>%
    unlist() %>% 
    tibble(value = .) %>% 
    mutate(next_val = lead(value)) %>% 
    filter(!is.na(next_val)) %>% 
    mutate(pair = paste0(value, next_val)) %>% 
    left_join(rules, by = "pair") %>% 
    pull(new)
  
  first <- d[1]
  others <- d[-1]
  
  others <- str_extract(others, "[A-Z][A-Z]$")
  
  paste0(others, collapse = "") %>% 
    paste0(first, .)
}

ans <- reduce(1:10, do_insertion, .init = start, rules = rules)

ans %>% 
  str_split("") %>% 
  unlist() %>% 
  tibble(x = .) %>% 
  count(x) %>% 
  arrange(n) %>% 
  slice(1, nrow(.)) %>% 
  summarise(diff(n))



# part 2 good -------------------------------------------------------------

# I had thought of a frequency table + transition matrix approach immediately, just like Day 6, but could not figure out how to map from old pairs to new pairs. I looked at the R4DS thread, not at any answers in particular, but saw someone mention that each pair generates two new pairs. Then it clicked, that the transition matrix would be how each possible input pair generates TWO new output pairs. The first letter of the pair + the new letter, and the new letter + the second letter of the pair. The rest of the work here is mine, after that bit of inspiration

start <- read_lines("2021/day_14/input.txt") %>% 
  pluck(1)

rules <- read_delim("2021/day_14/input.txt", delim = " -> ", 
                    skip = 2, col_names = c("pair", "insert")) %>% 
  mutate(pairs = str_split(pair, "")) %>% 
  unnest_wider(pairs) 

rules <- rules %>% 
  mutate(new_1 = paste0(...1, insert),
         new_2 = paste0(insert, ...2)) %>% 
  select(pair, new_1, new_2)

rules

m <- matrix(0L, nrow = nrow(rules), ncol = nrow(rules))

colnames(m) <- rules$pair
rownames(m) <- rules$pair

m[cbind(rules$new_1, rules$pair)] <- 1L
m[cbind(rules$new_2, rules$pair)] <- 1L


start_pair <- start %>% 
  str_split("") %>% 
  unlist() %>% 
  tibble(x = .) %>% 
  mutate(y = lead(x)) %>% 
  filter(!is.na(y)) %>% 
  mutate(pair = paste0(x,y)) %>% 
  pull(pair)

sv <- as.integer(rules$pair %in% start_pair) %>% 
  set_names(rules$pair)

res <- (m %^% 40) %*% sv 

options(scipen = 999)

reduce(1:40, ~ m %*% .x, .init = sv)

(m %^% 40) %*% sv %>% 
  tibble(name = rownames(.), value = .) %>% 
  mutate(name = str_split(name, "")) %>% 
  unnest(name) %>% 
  group_by(name) %>% 
  summarise(value = sum(value)/2) %>% 
  arrange(value) %>% 
  slice(1,nrow(.)) %>% 
  pull(value) %>% 
  ceiling() %>% 
  diff()
