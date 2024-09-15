# Load relevant libraries
library(tidyverse)
library(rstan)

# yj (number of tumors on rats)
success <- c(
  rep(0,14), rep(1,8), rep(2,9), 1, 5, 2, 5, 3, 2, 7, 7, 3,
  3, 2, 9, 10, 4, 4, 4, 4, 4, 4, 4, 10, 4, 4, 4, 5, 11, 12, 
  5, 5, 6, 5, 6, 6, 6, 6, 16, 15, 15, 9
)

# nj (total number of rats in each group)
total <- c(
  rep(20,7), rep(19,4), rep(18,2), 17, rep(20,4), rep(19,2),
  18, 18, 25, 24, 23, rep(20,6), 10, 49, 19, 46, 27, 17, 49,
  47, 20, 20, 13, 48, 50, 20, 20, 20, 20, 20, 20, 20, 48, 19,
  19, 19, 22, 46, 49, 20, 20, 23, 19, 22, 20, 20, 20, 52, 47, 
  46, 24
)

# Success rates
success_rate <- success / total

# Print success rates
print(success_rate)
