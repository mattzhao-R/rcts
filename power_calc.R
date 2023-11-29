pacman::p_load(tidyverse,lubridate,sandwich,lmtest,fixest)

# Simulations ----
## Checking Wage Spec ----

wages <- rep(seq(15,40,5),100)
rank <- rank(runif(600))
remoteYes <- ifelse(rank <= length(rank) / 2, 1, 0)
applyProb <- rnorm(600, mean = 0.3, sd = 0.05) + (log10(wages) / 10) + 
  0.2 * remoteYes # probability of applying increases with wage and if remote
applyYes <- ifelse(applyProb >= 0.5, 1, 0)

fake_data <- data.frame(
  wages,
  remoteYes,
  applyProb,
  applyYes
)

main_spec <- lm(wages ~ applyYes + remoteYes:applyYes, data = fake_data)

fake_data %>% 
  group_by(remoteYes, applyYes) %>%
  summarise(wg = mean(wages))

# Power Calculation ----
## Main Spec ----
### Sample Size ----
#### DGP ----
apply_dgp <- function(n){
  rank <- rank(runif(n))
  remoteYes <- ifelse(rank <= length(rank) / 2, 1, 0)
  baseapplyProb <- rnorm(n, mean = 0.2, sd = 0.3)
  applyProb <- baseapplyProb + 0.2 * remoteYes
  applyYes <- ifelse(applyProb >= 0.5, 1, 0)
  
  apply_data <- data.frame(
    applyYes,
    remoteYes,
    baseapplyProb,
    applyProb,
    applyYes
  )
  
  apply_data
}

#### Simulation + Graph ----
power_vec <- c()
n_vals <- seq(50,200,10)

for (n in n_vals) {
  pvals <- c()
  
  for (i in 1:1000) {
    df <- apply_dgp(n)
    model <- lm(applyYes ~ remoteYes, data = df)
    pval <- summary(model)$coefficients['remoteYes','Pr(>|t|)']
    pvals <- c(pvals,pval)
  }
  
  power <- sum(pvals < 0.05) / 1000 * 100
  power_vec <- c(power_vec, power)
}

ggplot(mapping = aes(x = n_vals, y = power_vec)) + 
  geom_point() + 
  labs(x = 'Sample Size', y = 'Power (%)')


## Secondary Spec ----
### Calculating Sample Size ----
#### DGP ----

attr_dgp <- function(n){
  wages <- rep(seq(15,35,5), n / 5)
  rank <- rank(runif(n))
  remoteYes <- ifelse(rank <= length(rank) / 2, 1, 0)
  baseapplyProb <- rnorm(n, mean = 0.4, sd = 0.05) + (log10(wages) / 10)
  applyProb <- baseapplyProb + 
    0.2 * remoteYes # probability of applying increases with wage and if remote
  applyYes <- ifelse(applyProb >= 0.5, 1, 0)
  
  fake_data <- data.frame(
    wages,
    remoteYes,
    baseapplyProb,
    applyProb,
    applyYes
  )
  
  fake_data
}

# napply <- c()
# for (i in 1:10000) {
#   df <- attr_dgp(100)
#   na <- sum(ifelse(df$baseapplyProb >= 0.5,1,0))
#   napply <- c(napply,na)
# }

#### Simulation + Graph ----
power_vec <- c()
n_vals <- seq(100,5000,500)

for (n in n_vals) {
  pvals <- c()
  
  for (i in 1:1000) {
    df <- attr_dgp(n)
    model <- lm(wages ~ applyYes + remoteYes:applyYes, data = df)
    pval <- summary(model)$coefficients['applyYes:remoteYes','Pr(>|t|)']
    pvals <- c(pvals,pval)
  }
  
  power <- sum(pvals < 0.05) / 1000 * 100
  power_vec <- c(power_vec, power)
}

ggplot(mapping = aes(x = n_vals, y = power_vec)) + 
  geom_point() + 
  labs(x = 'Sample Size', y = 'Power (%)')

