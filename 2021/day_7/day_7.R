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

# just check the mean. floor vs. ceiling depending on range of input values

if(diff(range(dt)) %% 2 == 0){
  m <- ceiling(mean(dt))
} else {
  m <- floor(mean(dt))
}

sum(cost_fast(abs(dt - m)))

# trying another input ----------------------------------------------------

d2 <- scan("2021/day_7/other_input", sep = ",")

diff(range(d2))
tc_fast(floor(mean(d2)), d2) > tc_fast(ceiling(mean(d2)), d2)

diff(range(dt))
tc_fast(floor(mean(dt)), dt) > tc_fast(ceiling(mean(dt)), dt)

# yeah I was right, floor vs ceiling depends on if the range of input values is odd or even in length
