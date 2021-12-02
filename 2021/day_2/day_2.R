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


# David Robinson's approach -----------------------------------------------

# creating XY coord movements for each direction
directions <- tribble(
  ~dir, ~x, ~y,
  "down", 0, 1,
  "up", 0,-1,
  "forward", 1, 0)

joined <- d %>% 
  inner_join(directions)


## Part 1
joined %>%
  summarize(horizontal = sum(len * x),
            depth = sum(len * y),
            product = horizontal * depth)


# Part 2
joined %>%
  mutate(aim = cumsum(y * len)) %>%
  summarize(horizontal = sum(len * x),
            depth = sum(aim * len * x),
            product = horizontal * depth)


# TJ Mahr's approach ------------------------------------------------------

# turns the string of forward 3 down 4 etc. into forward(3)\ndown(4), which is a series of R commands, then defines the functions forward, down, and up, and then runs that sequence of now-valid R commands

f02b_find_aimed_submarine_product <- function(x) {
  code <- x
  r_code <- gsub("(\\w+) (\\d+)", "\\1(\\2)", code)
  
  x <- 0
  y <- 0
  aim <- 0
  
  down <- function(d) {
    aim <<- aim + d
    c(x, y, aim)
  }
  
  up <- function(d) down(-1 * d)
  
  forward <- function(d) {
    y <<- y + d * aim
    x <<- x + d
    c(x, y, aim)
  }
  
  source(exprs = parse(text = r_code), local = TRUE)
  x * y
}

readLines("2021/day_2/input.txt") %>% 
  f02b_find_aimed_submarine_product()

readLines("2021/day_2/input.txt") %>% 
  gsub("(\\w+) (\\d+)", "\\1(\\2)", .)


# hrbrmstr's approach -----------------------------------------------------

# https://rud.is/b/2021/12/02/2021-advent-of-code-day-02-dont-try-this-at-home-edition/

input <- scan("2021/day_2/input.txt", what = list(character(), integer())) %>% 
  setNames(c("command", "value"))

up <- \(x) aim <<- aim - x
down <- \(x) aim <<- aim + x
forward <- function(x){
  horiz <<- horiz + x
  depth <<- depth + aim*x
}

aim <- horiz <- depth <- 0

for (idx in seq_along(input$command)) {
  get(input$command[idx], mode = "function")(input$value[idx])
}

horiz*depth

get(input$command[2], mode = "function")
