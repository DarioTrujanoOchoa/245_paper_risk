## Dario Trujano-Ochoa
## simulate r and elicit from different instruments to check the correlation
rm(list = ls())
# Generate r levels to create correlations ----
#r_simulated <- runif(min = -2,max = 2,n = 1000)
r_simulated <- rnorm(mean = 0.42,sd = 0.49,n = 1000)# noise
sd_epsilon  <- 0.2

# HL ----
## Payoffs ----
## 2019
option_A_payoffs <- c(40,32) # starting with the largest
option_B_payoffs <- c(77,2) # starting with the largest
## 2002
option_A_payoffs <- c(2,1.6) # starting with the largest
option_B_payoffs <- c(3.85,.1) # starting with the largest

# this file contains the function to elicit r
source("src/r_elicit_functions.R")

## Simulations ----
# for real r
r_HL <- rep(NA,length(r_simulated))
# for random utility deviations
r_HL_1 <- rep(NA,length(r_simulated))
r_HL_2 <- rep(NA,length(r_simulated))

for (r in r_simulated) {
  decision_index <- which(r_simulated==r)
  choices_r <- r_choices_HL(r = r,FUN = crra,
                            option_A_payoffs = option_A_payoffs,
                            option_B_payoffs = option_B_payoffs)
  # random deviations from the real r, two to take the correlations between repeated measures
  r_1 <- r + rnorm(mean = 0,sd = sd_epsilon,n = 1)
  choices_r_1  <- r_choices_HL(r = r_1,FUN = crra,
                               option_A_payoffs = option_A_payoffs,
                               option_B_payoffs = option_B_payoffs)
  r_2 <- r + rnorm(mean = 0,sd = sd_epsilon,n = 1)
  choices_r_2  <- r_choices_HL(r = r_2,FUN = crra,
                               option_A_payoffs = option_A_payoffs,
                               option_B_payoffs = option_B_payoffs)
  
  r_HL[decision_index] <- r_elicited_HL(choices_r)
  r_HL_1[decision_index] <- r_elicited_HL(choices_r_1)
  r_HL_2[decision_index] <- r_elicited_HL(choices_r_2)
}

### plots ----
pdf(file="results/corr_HL.pdf",
    width=8, height=4)
par(mfrow=c(1,2))
# relation with the real
plot(r_simulated,r_HL)
abline(a=0, b = 1)
cor.test(r_simulated,r_HL,na.action=na.omit)$estimate

# relation with the repeated mesure
plot(r_HL_1,r_HL_2)
abline(a=0, b = 1)
cor.test(r_HL_1,r_HL_2,na.action=na.omit)

dev.off()

## Different sd of epsilons ----

sds_epsilon <- seq(0,1.5,by=0.1)

corr_real_estimated_error <- rep(NA,length(sds_epsilon))
corr_error <- rep(NA,length(sds_epsilon))

for(sd_epsilon in sds_epsilon){
  sd_epsilon_index <- which(sds_epsilon==sd_epsilon)
  # for real r
  r_HL <- rep(NA,length(r_simulated))
  # for random utility deviations
  r_HL_1 <- rep(NA,length(r_simulated))
  r_HL_2 <- rep(NA,length(r_simulated))
  
  for (r in r_simulated) {
    decision_index <- which(r_simulated==r)
    choices_r <- r_choices_HL(r = r,FUN = crra,
                              option_A_payoffs = option_A_payoffs,
                              option_B_payoffs = option_B_payoffs)
    # random deviations from the real r, two to take the correlations between repeated measures
    r_1 <- r + rnorm(mean = 0,sd = sd_epsilon,n = 1)
    choices_r_1  <- r_choices_HL(r = r_1,FUN = crra,
                                 option_A_payoffs = option_A_payoffs,
                                 option_B_payoffs = option_B_payoffs)
    r_2 <- r + rnorm(mean = 0,sd = sd_epsilon,n = 1)
    choices_r_2  <- r_choices_HL(r = r_2,FUN = crra,
                                 option_A_payoffs = option_A_payoffs,
                                 option_B_payoffs = option_B_payoffs)
    
    r_HL[decision_index] <- r_elicited_HL(choices_r)
    r_HL_1[decision_index] <- r_elicited_HL(choices_r_1)
    r_HL_2[decision_index] <- r_elicited_HL(choices_r_2)
  }
  
  corr_real_estimated_error[sd_epsilon_index] <- cor.test(r_simulated,r_HL_1,na.action=na.omit)$estimate
  corr_error[sd_epsilon_index] <- cor.test(r_HL_1,r_HL_2,na.action=na.omit)$estimate
}

corr_real_estimated_error
corr_error

plot(sds_epsilon,corr_error,ylim = c(0,1),type = "l",col="darkred",
     main="Correlation as a function of the measurement error in HL")
points(sds_epsilon,corr_real_estimated_error,type = "l",col="darkgreen")
legend(x = 1,y=1,legend = c("error-error","real-error"),col = c("darkred","darkgreen"),lty = 1)

# EG ----
## Payoffs
## C&P 2015
safe_payoffs <- c(4,4) # safest option
event_B_down_jump <- 1
event_differential_proportion <- c(-1,2)
n_rows_EG <- 5


## Simulations ----
# for real r
r_EG <- rep(NA,length(r_simulated))
# for random utility deviations
r_EG_1 <- rep(NA,length(r_simulated))
r_EG_2 <- rep(NA,length(r_simulated))

for (r in r_simulated) {
  decision_index <- which(r_simulated==r)
  choices_r <- r_choices_EG(r = r,FUN = crra, 
                            safe_payoffs = safe_payoffs, 
                            event_B_down_jump = event_B_down_jump,
                            n_rows_EG=n_rows_EG)
  # random deviations from the real r, two to take the correlations between repeated measures
  r_1 <- r + rnorm(mean = 0,sd = sd_epsilon,n = 1)
  choices_r_1  <- r_choices_EG(r = r_1,FUN = crra,
                               safe_payoffs = safe_payoffs, 
                               event_B_down_jump = event_B_down_jump,
                               n_rows_EG=n_rows_EG)
  r_2 <- r + rnorm(mean = 0,sd = sd_epsilon,n = 1)
  choices_r_2  <- r_choices_EG(r = r_2,FUN = crra,
                               safe_payoffs = safe_payoffs, 
                               event_B_down_jump = event_B_down_jump,
                               n_rows_EG=n_rows_EG)
  
  r_EG[decision_index] <- r_elicited_EG(choices_r)
  r_EG_1[decision_index] <- r_elicited_EG(choices_r_1)
  r_EG_2[decision_index] <- r_elicited_EG(choices_r_2)
}

## plots ----
pdf(file="results/corr_EG.pdf",
    width=8, height=4)
par(mfrow=c(1,2))
# relation with the real 
plot(r_simulated,r_EG)
abline(a=0, b = 1)
cor.test(r_simulated,r_EG,na.action=na.omit)

# relation with the repeated measure
plot(r_EG_1,r_EG_2)
abline(a=0, b = 1)
cor.test(r_EG_1,r_EG_2,na.action=na.omit)

dev.off()

# Corr between tasks ----

pdf(file="results/corr_EG_HL.pdf",
    width=8, height=4)
par(mfrow=c(1,2))
# relation with the real 
plot(r_HL,r_EG)
abline(a=0, b = 1)
cor.test(r_HL,r_EG,na.action=na.omit)

# relation with the repeated measure
plot(r_HL_1,r_EG_1)
abline(a=0, b = 1)
cor.test(r_HL_1,r_EG_2,na.action=na.omit)

dev.off()

