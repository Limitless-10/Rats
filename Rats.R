# Load relevant libraries
library(tidyverse)
library(rstan)

### Historical Data

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

# Binomial model for number of tumours

# For convenience, prior distribution from conjugate Beta(alpha,beta)
beta_hist <- function(mean,var) {
  # Construct alpha and beta from methods of moments
  alpha <- (((mean*(1-mean))/(var))-1)*mean
  beta <- (((mean*(1-mean))/(var))-1)*(1-mean)
  
  # Return list of results
  return(list(alpha=alpha,beta=beta))
}

# Success rates
success_rate <- success / total

# Construct plot data
plot_data <- data.frame(y= success_rate, x = total)

# Plot y_j/n_j against n_j
pair_plot <- ggplot(plot_data, aes(y=y,x=x)) +
  geom_point(colour="skyblue",size=3) +
  geom_line(colour="darkblue", linetype="dashed") +
  labs(y = "Tumour Rate", x = "Experiment Size", title = "Rat Tumour Rate vs Experiment Size") +
  theme_minimal()

test <- beta_hist(mean(success_rate),var(success_rate))

# Print success rates
print(success_rate)

## Gamma approximation

stirling_gamma <- function(x) {
  sqrt(2 * pi / x) * (x / exp(1))^x
}

lanczos_gamma <- function(x) {
  g <- 7
  p <- c(0.99999999999980993, 676.5203681218851, -1259.1392167224028, 
         771.32342877765313, -176.61502916214059, 12.507343278686905, 
         -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7)
  
  if (x < 0.5) {
    return(pi / (sin(pi*x) * lanczos_gamma(1-x)))
  } else {
    x <- x - 1
    a <- p[1]
    for (i in 2:length(p)) {
      a <- a + p[i] / (x+i-1)
    }
    t <- x + g + 0.5
    sqrt(2*pi) * t^(x+0.5) * exp(-t) * a
  }
}

recursive_gamma <- function(x) {
  if (x == 1) {
    return(1)
  } else if (x == 0.5) {
    return(sqrt(pi))
  } else {
    return((x-1) * recursive_gamma(x-1))
  }
}

x <- 5.5

# Stirling's approximation
stirling_val <- stirling_gamma(x)
print(paste("Stirling's approximation:", stirling_val))

# Lanczos approximation
lanczos_val <- lanczos_gamma(x)
print(paste("Lanczos approximation:", lanczos_val))

# Recursive method
recursive_val <- recursive_gamma(x)
print(paste("Recursive method:", recursive_val))

# R's built-in gamma function
r_gamma_val <- gamma(x)
print(paste("R's built-in gamma function:", r_gamma_val))










