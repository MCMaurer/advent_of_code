library(tidyverse)

d <- read_lines("2021/day_8/input.txt")%>% 
  as_tibble() %>% 
  separate(value, c("seven", "four"), sep = " \\| ") %>% 
  mutate(entry = row_number())

d %>% 
  select(four, entry) %>% 
  rowwise() %>% 
  mutate(four = str_split(four, " "),
         type = 4) %>% 
  unnest(four) %>% 
  rename(value = four) %>% 
  mutate(str_len = str_length(value)) %>% 
  summarise(part_1 = sum(str_len %in% c(2, 3, 4, 7)))


d <- read_lines("2021/day_8/input.txt")

# just the ugliest thing I've ever written
get_ans <- function(dt){
  
  dt <- dt %>% 
    as_tibble() %>% 
    separate(value, c("seven", "four"), sep = " \\| ") %>% 
    mutate(entry = row_number())
  
  dt7 <- dt %>% 
    select(seven, entry) %>% 
    rowwise() %>% 
    mutate(seven = str_split(seven, " "),
           type = 7) %>% 
    unnest(seven) %>% 
    rename(value = seven)
  
  dt4 <- dt %>% 
    select(four, entry) %>% 
    rowwise() %>% 
    mutate(four = str_split(four, " "),
           type = 4) %>% 
    unnest(four) %>% 
    rename(value = four)
  
  dd_tot <- bind_rows(dt4, dt7) %>% 
    mutate(str_len = str_length(value))
  
  known_segs <- segs %>% filter(str_len %in% c(2,4,3,7))
  
  ddt <- dd_tot %>% 
    filter(type == 7) %>% 
    left_join(known_segs)
  
  find_a <- function(vals){
    ones <- str_split(vals[1], "") %>% pluck(1)
    sevens <- str_split(vals[2], "") %>% pluck(1)
    sevens[!(sevens %in% ones)]
  }
  
  ddt_known <- ddt %>%
    filter(!is.na(digit)) %>% 
    select(value, digit)
  
  ddt_a <- ddt %>% 
    filter(digit %in% c(1,7)) %>% 
    arrange(digit) %>% 
    pull(value) %>% 
    find_a()
  
  ddt <- ddt %>% 
    mutate(new_val = str_remove(value, ddt_a))
  
  find_f_and_6 <- function(vals){
    ones <- str_split(vals[1], "") %>% pluck(1)
    six1 <- str_split(vals[2], "") %>% pluck(1)
    six2 <- str_split(vals[3], "") %>% pluck(1)
    six3 <- str_split(vals[4], "") %>% pluck(1)
    
    f1 <- six1[six1 %in% ones]
    f2 <- six2[six2 %in% ones]
    f3 <- six3[six3 %in% ones]

    if(length(f1) == 1){
      f <- f1
      six <- six1
    }
    if(length(f2) == 1){
      f <- f2
      six <- six2
    }
    if(length(f3) == 1){
      f <- f3
      six <- six3
    }
    
    return(list(f = f, six = str_c(six, collapse = "")))
  }
  
  ddt_f_6 <- ddt %>% 
    filter(str_len %in% c(2,6)) %>% 
    arrange(str_len) %>% 
    pull(value) %>% 
    find_f_and_6()
  
  ddt_f_6
  
  ddt$digit[ddt$value == ddt_f_6$six] <- 6
  
  ddt <- ddt %>% 
    mutate(new_val = str_remove(new_val, ddt_f_6$f))

  ddt_c <- ddt %>% 
    filter(digit == 1) %>% 
    pull(new_val)

  ddt <- ddt %>% 
    mutate(new_val = str_remove(new_val, ddt_c))
  
  ddt$digit[str_length(ddt$new_val) == 2 & is.na(ddt$digit)] <- 3
  
  find_b_d_g <- function(vals){
    threes <- str_split(vals[1], "") %>% pluck(1)
    fours <- str_split(vals[2], "") %>% pluck(1)
    
    d <- threes[threes %in% fours]
    g <- threes[!(threes %in% fours)]
    b <- fours[!(fours %in% threes)]
    
    return(list(d = d, g = g, b = b))
  }
  
  ddt_d_g_b <- ddt %>% 
    filter(digit %in% 3:4) %>% 
    arrange(digit) %>% 
    pull(new_val) %>% 
    find_b_d_g()
  
  ddt <- ddt %>% 
    mutate(new_val = str_remove(new_val, ddt_d_g_b$d),
           new_val = str_remove(new_val, ddt_d_g_b$g),
           new_val = str_remove(new_val, ddt_d_g_b$b))
  
  ddt <- ddt %>% 
    mutate(digit = case_when(
      str_len == 5 & str_length(new_val) == 1 & is.na(digit) ~ 2,
      str_len == 5 & str_length(new_val) == 0 & is.na(digit) ~ 5,
      str_len == 6 & str_length(new_val) == 1 & is.na(digit) ~ 0,
      str_len == 6 & str_length(new_val) == 0 & is.na(digit) ~ 9,
      TRUE ~ digit
    ))
  
  ddt_key <- ddt %>% 
    select(value, digit)
  
  sort_chars <- function(x){
    x %>% 
      str_split("") %>% 
      pluck(1) %>% 
      str_sort() %>% 
      str_c(collapse = "")
  }
  
  ddt_key <- ddt_key %>% 
    rowwise() %>% 
    mutate(value = sort_chars(value))
  
  dt4 %>% 
    rowwise() %>% 
    mutate(value = sort_chars(value)) %>% 
    select(value) %>% 
    left_join(ddt_key) %>% 
    pull(digit) %>% 
    as.character() %>% 
    str_c(collapse = "") %>% 
    as.numeric()
}

dt <- "eb cbgfae cabdf fedab efb adgcef cbgaefd egdb dbgefa eafgd | dfbae be gdafe gcefab"

get_ans(dt)



ans <- map(d, get_ans)

reduce(ans, `+`)


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
