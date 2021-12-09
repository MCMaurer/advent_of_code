library(tidyverse)

d <- read_lines("2021/day_8/input.txt")

d %>% 
  as_tibble() %>% 
  separate(value, c("seven", "four"), sep = " \\| ") %>% 
  mutate(entry = row_number()) %>% 
  select(four, entry) %>% 
  rowwise() %>% 
  mutate(four = str_split(four, " "),
         type = 4) %>% 
  unnest(four) %>% 
  rename(value = four) %>% 
  mutate(str_len = str_length(value)) %>% 
  summarise(part_1 = sum(str_len %in% c(2, 3, 4, 7)))

solve_pattern <- function(d){
  n <- vector(mode = "list", length = 10)
  n <- set_names(n, 0:9)
  
  v <- d %>% 
    str_split(" \\| ") %>% 
    pluck(1) %>% 
    str_split(" ")
  
  v <- set_names(v, c("pattern", "output"))
  
  p <- v$pattern
  
  n$`1` <- p[str_length(p) == 2]
  n$`4` <- p[str_length(p) == 4]
  n$`7` <- p[str_length(p) == 3]
  n$`8` <- p[str_length(p) == 7]
  
  
  str_inter_l <- function(unknown, known){
    uv <- map(unknown, ~str_split(.x, "") %>% pluck(1))
    kv <- str_split(known, "") %>% pluck(1)
    map(uv, ~intersect(.x, kv)) %>% 
      map_dbl(length)
  }
  
  n$`6` <- p[str_inter_l(p, n$`1`) == 1 & str_length(p) == 6]
  n$`9` <- p[str_inter_l(p, n$`4`) == 4 & str_length(p) == 6]
  n$`0` <- p[str_length(p) == 6 & p != n$`6` & p != n$`9`]
  
  n$`3` <- p[str_inter_l(p, n$`7`) == 3 & str_length(p) == 5]
  n$`5` <- p[str_inter_l(p, n$`6`) == 5 & str_length(p) == 5]
  n$`2` <- p[str_length(p) == 5 & p != n$`3` & p != n$`5`]
  
  sort_letters <- function(x){
    str_split(x, "") %>% 
      map(sort) %>% 
      map_chr(paste, collapse = "")
  }
  
  key <- tibble(digit = names(n) %>% as.numeric(),
                pattern = unlist(n) %>% sort_letters())
  
  tibble(pattern = v$output %>% sort_letters()) %>% 
    left_join(key) %>% 
    pull(digit) %>% 
    paste0(collapse = "") %>% 
    as.numeric()
}

map(d, solve_pattern) %>% 
  unlist() %>% 
  sum()


# trying David Robinson's solution ----------------------------------------


d <- read_lines("2021/day_8/input.txt")%>% 
  as_tibble() %>% 
  separate(value, c("pattern", "output"), sep = " \\| ") %>% 
  mutate(entry = row_number())

sort_letters <- function(x){
  str_split(x, "") %>% 
    map(sort) %>% 
    map_chr(paste, collapse = "")
}

get_fingerprint <- function(d, key){
  spl <- d %>% 
    mutate(letter = str_split(pattern, "")) %>% 
    unnest(letter)
  
  spl %>% 
    inner_join(spl, by = c("letter", group_vars(d)), 
               suffix = c("", "2")) %>% 
    count({{key}}, letter) %>% 
    group_by({{key}}, .add = T) %>% 
    summarise(fingerprint = paste0(sort(n), collapse = "/"))
}

fingerprints <- "abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg" %>% str_split(" ") %>% pluck(1) %>% 
  tibble(digit = 0:9, pattern = .)

fingerprints

fingerprints %>% 
  get_fingerprint(digit) %>% 
  left_join(fingerprints)

fingerprints <- fingerprints %>% 
  get_fingerprint(digit)



d_key <- d %>% 
  separate_rows(pattern) %>% 
  mutate(pattern = sort_letters(pattern)) %>% 
  group_by(entry) %>% 
  get_fingerprint(pattern) %>% 
  inner_join(fingerprints, by = "fingerprint")

d %>% 
  separate_rows(output) %>% 
  mutate(output = sort_letters(output)) %>% 
  inner_join(d_key, by = c("entry", "output" = "pattern")) %>% 
  group_by(entry) %>% 
  summarise(value = as.numeric(paste0(digit, collapse = ""))) %>% 
  pull(value) %>% 
  sum()
