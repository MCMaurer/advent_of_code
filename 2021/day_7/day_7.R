library(tidyverse)

dt <- scan("2021/day_7/input.txt", sep = ",")

# part 1
sum(abs(dt - median(dt)))

# part 2
# change the loss function

cost <- function(n){
  map_dbl(n, ~ reduce(1:.x, `+`, .init = 0))
}

tot_cost <- function(midpoint, pos){
  dist <- abs(pos - midpoint)
  cost <- cost(dist)
  sum(cost)
}

res <- optimise(f = tot_cost, quantile(dt, c(0.4, 0.6)), pos = dt, tol = 1)

tot_cost(round(res$minimum), dt)

# even quicker, just using mean as midpoint instead of median
tot_cost(floor(mean(dt)), dt)


# fastest approach --------------------------------------------------------

dt <- scan("2021/day_7/input.txt", sep = ",")

# part 1
sum(abs(dt - median(dt)))

# part 2
cost_fast <- function(n){
  n * (n+1) / 2
}

tc_fast <- function(midpoint, pos){
  sum(cost_fast(abs(pos - midpoint)))
}

# just check the mean
tc_fast(floor(mean(dt)), dt)
