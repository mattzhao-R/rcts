pacman::p_load(tidyverse,lubridate,sandwich,lmtest,fixest)

# Simulations ----
## Checking Main Spec ----

wages <- rep(seq(15,40,5),1000)
rank <- rank(runif(6000))
remoteYes <- ifelse(rank <= length(rank) / 2, 1, 0)
applyProb <- rnorm(6000, mean = 0.3, sd = 0.05) + (log10(wages) / 10) + 
  0.2 * remoteYes # probability of applying increases with wage and if remote
applyYes <- ifelse(applyProb >= 0.5, 1, 0)

fake_data <- data.frame(
  wages,
  remoteYes,
  applyProb,
  applyYes
)

m1 <- lm(wages ~ remoteYes:applyYes, data = fake_data)
m2 <- lm(wages ~ applyYes + remoteYes:applyYes, data = fake_data)

# Power Calculation ----
## Calculating Sample Size ----
### DGP ----



## 