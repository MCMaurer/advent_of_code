library(tidyverse)

nums <- read_lines("2021/day_4/input.txt", n_max = 1) %>% 
  str_split(",") %>% 
  pluck(1) %>% 
  as.numeric()

boards <- read_table("2021/day_4/input.txt", 
                     skip = 2, col_names = letters[1:5]) %>% 
  mutate(board = ((row_number()-1) %/% 5)+1) %>% 
  group_by(board) %>% 
  group_split(.keep = F) %>% 
  map(as.matrix)

check_win <- function(mat){
  mat <- is.na(mat)
  tots <- c(
    #sum(diag(mat), na.rm = T),
    #sum(diag(mat[,rev(sequence(NCOL(mat)))]), na.rm = T),
    rowSums(mat, na.rm = T),
    colSums(mat, na.rm = T))
  nrow(mat) %in% tots
}

update_score <- function(num, mat){
  mat[mat == num] <- NA
  mat
}

find_win <- function(mat, vals){
  for (i in 1:length(vals)) {
    mat <- update_score(vals[i], mat)
    if(check_win(mat)){
      res <- list(win_mat = mat, i = i)
      return(res)
    }
  }
}

bt <- tibble(boards = boards)

bt <- bt %>% 
  rowwise() %>% 
  mutate(res = list(find_win(boards, nums))) %>% 
  unnest_wider(res)

bt %>% 
  slice_min(i, n=1) %>% 
  mutate(win_num = nums[i],
         win_sum = sum(unlist(win_mat), na.rm = T),
         ans = win_num*win_sum)


# part 2 ------------------------------------------------------------------

bt %>% 
  slice_max(i, n=1) %>% 
  mutate(win_num = nums[i],
         win_sum = sum(unlist(win_mat), na.rm = T),
         ans = win_num*win_sum)

