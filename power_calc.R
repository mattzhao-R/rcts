pacman::p_load(tidyverse,lubridate,sandwich,lmtest,fixest)

# Final Proposal ----
## Main Spec ----
### DGP ----

main_dgp <- function(n,mde,elas,var){
  elasLS <- elas # elasticity of 0.3 - 1% increase in wage corresponds to
                  # 0.3% increase in quantity of labor supplied
  wages <- c(0,
             ((20-15)/15)*elasLS,
             ((25-15)/15)*elasLS,
             ((30-15)/15)*elasLS,
             ((35-15)/15)*elasLS
             )
  wageProb <- rep(wages, n / 5)
  rank <- rank(runif(n))
  remoteYes <- ifelse(rank <= length(rank) / 2, 1, 0)
  baseapplyProb <- rnorm(n, mean = 0.369, sd = var ** 0.5) + 
    wageProb * 0.37
  baseapplyProbbounded <- ifelse(baseapplyProb <= 0, 0, baseapplyProb)
  applyProb <- baseapplyProbbounded + mde * remoteYes
  applyYes <- ifelse(applyProb >= 0.5, 1, 0)
  
  apply_data <- data.frame(
    applyYes,
    remoteYes,
    baseapplyProb,
    applyProb,
    applyYes,
    wages
  )
  
  apply_data
}


#### wage vs no wage distribution ----
nowage <- c()
wage <- c()

n <- 300
elasLS <- 0.3
wages <- c(0,
           ((20-15)/15)*elasLS,
           ((25-15)/15)*elasLS,
           ((30-15)/15)*elasLS,
           ((35-15)/15)*elasLS
)
wageProb <- rep(wages, n / 5)

for (i in 1:1000){
  wage_distr <- rnorm(n, mean = 0.37, sd = var ** 0.5) + wageProb * 0.37
  nowage_distr <- rnorm(n, mean = 0.37 + 0.4666667, sd = var ** 0.5)
  nowage <- c(nowage,mean(ifelse(nowage_distr > 0.5,1,0)))
  wage <- c(wage,mean(ifelse(wage_distr > 0.5,1,0)))
}

tempdf <- data.frame(
  nowage,
  wage
) %>% 
  pivot_longer(cols = c(nowage,wage),
               names_to = 'name',
               values_to = 'value')

ggplot(mapping = aes(x = value, fill = name),data=tempdf) +
  geom_histogram()


### Calc ----
power_vec <- c()
n_vals <- seq(200,700,50)

for (n in n_vals) {
  pvals <- c()
  
  for (i in 1:1000) {
    df <- main_dgp(n,mde=0.15,0.3,0.25)
    model <- lm(applyYes ~ remoteYes, data = df)
    pval <- summary(model)$coefficients['remoteYes','Pr(>|t|)']
    pvals <- c(pvals,pval)
  }
  
  power <- sum(pvals < 0.05) / 1000 * 100
  power_vec <- c(power_vec, power)
}

ggplot(mapping = aes(x = n_vals, y = power_vec)) + 
  geom_point() + 
  labs(x = 'Sample Size', y = 'Power (%)') +
  ggtitle('Necessary Sample Size at MDE = 0.15') +
  theme(plot.title = element_text(family = 'serif', hjust = 0.5, size = 12), 
        axis.title = element_text(family = 'serif', size = 10),
        axis.text = element_text(family = 'serif',size=8)) + 
  geom_hline(yintercept = 80, alpha = 0.5, linetype = 2)
ggsave(filename = 'final_main_n.png',
       path = "./rcts/power_outputs",
       device = "png",
       width = 15,
       height = 8,
       limitsize = FALSE,
       dpi = 300,
       units = "cm")

### Sensitivity ----
#### elasticity ----
# test 0.05 - 0.7
elas_vals <- seq(0.1,0.7,0.2)
n_vals <- seq(200,700,50)
sens_df <- data.frame()

for (n in n_vals) {
  power_vec <- c()
  for (elas in elas_vals) {
    pvals <- c()
    
    for (i in 1:1000) {
      df <- main_dgp(n,mde=0.15,elas=elas,var=0.25)
      model <- lm(applyYes ~ remoteYes, data = df)
      pval <- summary(model)$coefficients['remoteYes','Pr(>|t|)']
      pvals <- c(pvals,pval)
    }
    
    power <- sum(pvals < 0.05) / 1000 * 100
    power_vec <- c(power_vec, power)
  }
  temp <- data.frame(
    power_vec,
    elas_vals,
    rep(n,length(power_vec))
  )
  sens_df <- bind_rows(sens_df, temp)
}
colnames(sens_df) <- c('power','elas','n')

ggplot(data = sens_df,
       mapping = aes(x = n, y = power, colour = factor(elas))) + 
  geom_smooth(method = 'loess', se = F) + 
  labs(x = 'Sample Size', y = 'Power (%)', colour = 'Elasticity') +
  ggtitle('Power Sensitivity to Elasticity') +
  theme(plot.title = element_text(family = 'serif', hjust = 0.5, size = 12), 
        axis.title = element_text(family = 'serif', size = 10),
        axis.text = element_text(family = 'serif',size=8)) + 
  geom_hline(yintercept = 80, alpha = 0.5, linetype = 2)
ggsave(filename = 'final_main_sens_elas.png',
       path = "./rcts/power_outputs",
       device = "png",
       width = 15,
       height = 8,
       limitsize = FALSE,
       dpi = 300,
       units = "cm")


# variance ----
# test var 0.2 - 0.5 (sd 0.45 - 0.71)
var_vals <- c(0.15,0.25,0.35,0.45)
n_vals <- seq(200,700,50)
sens_df <- data.frame()

for (n in n_vals) {
  power_vec <- c()
  for (var in var_vals) {
    pvals <- c()
    
    for (i in 1:1000) {
      df <- main_dgp(n,mde=0.15,elas=0.3,var=var)
      model <- lm(applyYes ~ remoteYes, data = df)
      pval <- summary(model)$coefficients['remoteYes','Pr(>|t|)']
      pvals <- c(pvals,pval)
    }
    
    power <- sum(pvals < 0.05) / 1000 * 100
    power_vec <- c(power_vec, power)
  }
  temp <- data.frame(
    power_vec,
    var_vals,
    rep(n,length(power_vec))
  )
  sens_df <- bind_rows(sens_df, temp)
}
colnames(sens_df) <- c('power','var','n')

ggplot(data = sens_df,
       mapping = aes(x = n, y = power, colour = factor(var))) + 
  geom_smooth(method = 'loess', se = F) + 
  labs(x = 'Sample Size', y = 'Power (%)', colour = 'Variance') +
  ggtitle('Power Sensitivity to Variance') +
  theme(plot.title = element_text(family = 'serif', hjust = 0.5, size = 12), 
        axis.title = element_text(family = 'serif', size = 10),
        axis.text = element_text(family = 'serif',size=8)) + 
  geom_hline(yintercept = 80, alpha = 0.5, linetype = 2)
ggsave(filename = 'final_main_sens_var.png',
       path = "./rcts/power_outputs",
       device = "png",
       width = 15,
       height = 8,
       limitsize = FALSE,
       dpi = 300,
       units = "cm")



## Secondary ----
### DGP ----
sec_dgp <- function(n,mde,elas,var){
  elasLS <- elas # elasticity of 0.3 - 1% increase in wage corresponds to 
  # 0.3% increase in quantity of labor supplied
  wages <- c(0,
             ((20-15)/15)*elasLS,
             ((25-15)/15)*elasLS,
             ((30-15)/15)*elasLS,
             ((35-15)/15)*elasLS
  )
  wageProb <- rep(wages, n / 5)
  femaleYes <- rep(c(0,0,0,1,1))
  
  rank <- rank(runif(n))
  remoteYes <- ifelse(rank <= length(rank) / 2, 1, 0)
  baseapplyProb <- rnorm(n, mean = 0.37, sd = var ** 0.5) + wageProb + 
    0.2 * femaleYes
  baseapplyProbbounded <- ifelse(baseapplyProb <= 0, 0, baseapplyProb)
  applyProb <- baseapplyProbbounded + mde * remoteYes
  applyYes <- ifelse(applyProb >= 0.5, 1, 0)
  
  apply_data <- data.frame(
    applyYes,
    remoteYes,
    baseapplyProb,
    applyProb,
    applyYes,
    wages
  )
  
  apply_data
}


### Calc ----





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
### DGP ----
apply_dgp <- function(n,mde){
  wages <- rep(seq(15,35,5), n / 5)
  rank <- rank(runif(n))
  remoteYes <- ifelse(rank <= length(rank) / 2, 1, 0)
  baseapplyProb <- rnorm(n, mean = 0.083, sd = 0.25) + 
    (log10(wages) / 10)
  baseapplyProbbounded <- ifelse(baseapplyProb <= 0,0,baseapplyProb)
  applyProb <- baseapplyProbbounded + mde * remoteYes
  applyYes <- ifelse(applyProb >= 0.5, 1, 0)
  
  
  apply_data <- data.frame(
    applyYes,
    remoteYes,
    baseapplyProb,
    applyProb,
    applyYes,
    wages
  )
  
  apply_data
}

### Sample Size ----

power_vec <- c()
n_vals <- seq(50,200,10)

for (n in n_vals) {
  pvals <- c()
  
  for (i in 1:1000) {
    df <- apply_dgp(n,mde=0.2)
    model <- lm(applyYes ~ remoteYes, data = df)
    pval <- summary(model)$coefficients['remoteYes','Pr(>|t|)']
    pvals <- c(pvals,pval)
  }
  
  power <- sum(pvals < 0.05) / 1000 * 100
  power_vec <- c(power_vec, power)
}

ggplot(mapping = aes(x = n_vals, y = power_vec)) + 
  geom_point() + 
  labs(x = 'Sample Size', y = 'Power (%)') +
  ggtitle('Necessary Sample Size at MDE = 0.2') +
  theme(plot.title = element_text(family = 'serif', hjust = 0.5, size = 12), 
        axis.title = element_text(family = 'serif', size = 10),
        axis.text = element_text(family = 'serif',size=8)) + 
  geom_hline(yintercept = 80, alpha = 0.5, linetype = 2)
ggsave(filename = 'main_n_base.png',
       path = "./rcts/power_outputs",
       device = "png",
       width = 15,
       height = 8,
       limitsize = FALSE,
       dpi = 300,
       units = "cm")

### MDE Sensitivity ----

mde_vals <- seq(0.05,0.35,0.05)
n_vals <- seq(50,200,10)
sens_df <- data.frame()

for (n in n_vals) {
  power_vec <- c()
  for (mde in mde_vals) {
    pvals <- c()
    
    for (i in 1:1000) {
      df <- apply_dgp(n,mde)
      model <- lm(applyYes ~ remoteYes, data = df)
      pval <- summary(model)$coefficients['remoteYes','Pr(>|t|)']
      pvals <- c(pvals,pval)
    }
    
    power <- sum(pvals < 0.05) / 1000 * 100
    power_vec <- c(power_vec, power)
  }
  temp <- data.frame(
    power_vec,
    mde_vals,
    rep(n,length(power_vec))
  )
  sens_df <- bind_rows(sens_df, temp)
}
colnames(sens_df) <- c('power','mde','n')

ggplot(data = sens_df %>%
         filter(!(mde %in% c(0.05,0.35))),
       mapping = aes(x = n, y = power, colour = factor(mde))) + 
  # geom_point() + 
  geom_smooth(method = 'loess', se = F) + 
  labs(x = 'Sample Size', y = 'Power (%)', colour = 'MDE') +
  ggtitle('Power Sensitivity to MDE') +
  theme(plot.title = element_text(family = 'serif', hjust = 0.5, size = 12), 
        axis.title = element_text(family = 'serif', size = 10),
        axis.text = element_text(family = 'serif',size=8)) + 
  geom_hline(yintercept = 80, alpha = 0.5, linetype = 2)
ggsave(filename = 'main_sens_base.png',
       path = "./rcts/power_outputs",
       device = "png",
       width = 15,
       height = 8,
       limitsize = FALSE,
       dpi = 300,
       units = "cm")


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


