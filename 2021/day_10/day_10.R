library(tidyverse)

d <- read_table("2021/day_10/input.txt", col_names = "str") 

key <- tibble(open = c("(", "[", "{", "<"),
              close = c(")", "]", "}", ">"),
              open_r = c("\\(", "\\[", "\\{", "<"),
              close_r = c("\\)", "\\]", "\\}", ">"))

valid_pairs <- tibble(pair = c("()", "[]", "{}", "<>"),
                      pair_r = c("\\(\\)", "\\[\\]", "\\{\\}", "<>"))

remove_pairs <- function(str){
  while (str_detect(str, paste0(valid_pairs$pair_r, collapse = "|"))) {
    str <- str_remove_all(str, paste0(valid_pairs$pair_r, collapse = "|"))
  }
  return(str)
}

invalid_pairs <- expand_grid(open_r = key$open_r, 
            close_r = key$close_r) %>% 
  mutate(comb = map2_chr(open_r, close_r, paste0)) %>% 
  tidygraph::anti_join(key)

scores <- tibble(inv_char = key$close,
                 inv_char_r = key$close_r,
                 score = c(3, 57, 1197, 25137))

d %>% 
  mutate(str_inv = map_chr(str, remove_pairs)) %>% 
  mutate(inv_p = str_extract(str_inv, 
                             paste0(invalid_pairs$comb, collapse = "|"))) %>% 
  mutate(inv_char = str_extract(inv_p, ".$")) %>% 
  left_join(scores, by = "inv_char") %>% 
  summarise(sum(score, na.rm = T))

# part 2

di <- d %>% 
  mutate(str_inv = map_chr(str, remove_pairs)) %>% 
  mutate(inv_p = str_extract(str_inv, 
                             paste0(invalid_pairs$comb, collapse = "|"))) %>% 
  mutate(inv_char = str_extract(inv_p, ".$")) %>% 
  left_join(scores, by = "inv_char") %>% 
  filter(is.na(score)) %>% 
  select(str_inv)

str_rev <- function(str){
  str_v <- str_split(str, "") %>% pluck(1)
  paste0(rev(str_v), collapse = "")
}

scores_i <- tibble(open = key$open,
                   score = 1:4)

score_string <- function(str){
  str_vec <- str_split(str, "") %>% pluck(1)
  score <- 0
  for (i in 1:length(str_vec)) {
    score <- score*5
    score <- score + scores_i$score[scores_i$open == str_vec[i]]
  }
  return(score)
}

di %>% 
  mutate(str_r = map_chr(str_inv, str_rev),
         score = map_dbl(str_r, score_string)) %>% 
  pull(score) %>% 
  median()
