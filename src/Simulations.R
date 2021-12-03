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
plot(r_simulated,r_HL,pch=16,cex=1.2,col=rgb(0,0,1,alpha = 0.05),
     main = "Real r and elicited r using HL",
     xlab="Simulated r",ylab="Elicited r")
abline(a=0, b = 1,col="blue")
cor.test(r_simulated,r_HL,na.action=na.omit)
text(x = 1,y=-1,paste("Corr=", 
                      round(cor.test(r_simulated,r_HL,na.action=na.omit)$estimate,4)))

# relation with the repeated mesure
plot(r_HL_1,r_HL_2,pch=16,cex=1.2,col=rgb(0,0,1,alpha = 0.05),
     main = "Two measures of r using HL",
     xlab="Elicited r with measurement error 1",
     ylab="Elicited r with measurement error 2")
abline(a=0, b = 1,col="blue")
cor.test(r_HL_1,r_HL_2,na.action=na.omit)
text(x = 1,y=-1,paste("Corr=", 
                      round(cor.test(r_HL_1,r_HL_2,na.action=na.omit)$estimate,4)))

dev.off()

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
sd_epsilon  <- 0.2

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

### plots ----
pdf(file="results/corr_EG.pdf",
    width=8, height=4)
par(mfrow=c(1,2))
# relation with the real 
plot(r_simulated,r_EG,pch=16,cex=1.2,col=rgb(0,0,1,alpha = 0.05),
     main = "Real r and elicited r using EG",
     xlab="Simulated r",ylab="Elicited r")
abline(a=0, b = 1,col="blue")
cor.test(r_simulated,r_EG,na.action=na.omit)
text(x = 1,y=.2,paste("Corr=", 
                      round(cor.test(r_simulated,r_EG,na.action=na.omit)$estimate,4)))

# relation with the repeated measure
plot(r_EG_1,r_EG_2,pch=16,cex=1.2,col=rgb(0,0,1,alpha = 0.05),
     main = "Between two measures of r using EG",
     xlab="Elicited r with measurement error 1",
     ylab="Elicited r with measurement error 2")
abline(a=0, b = 1,col="blue")
cor.test(r_EG_1,r_EG_2,na.action=na.omit)
text(x = 1,y=.2,paste("Corr=", 
                      round(cor.test(r_EG_1,r_EG_2,na.action=na.omit)$estimate,4)))

dev.off()

# Corr between tasks plots ----

pdf(file="results/corr_EG_HL.pdf",
    width=8, height=4)
par(mfrow=c(1,2))
# relation with the real 
plot(r_HL,r_EG,pch=16,cex=1.2,col=rgb(0,0,1,alpha = 0.05),
     main = "HL and EG without error",
     ylab=expression('r'[EG]),xlab=expression('r'[HL]))
abline(a=0, b = 1,col="blue")
cor.test(r_HL,r_EG,na.action=na.omit)
text(x = 1,y=.3,paste("Corr=", 
                      round(cor.test(r_HL,r_EG,na.action=na.omit)$estimate,4)))

# relation with the repeated measure
plot(r_HL_1,r_EG_1,pch=16,cex=1.2,col=rgb(0,0,1,alpha = 0.05),
     main = "HL and EG with error",
     ylab=expression('r'[EG]),xlab=expression('r'[HL]))
abline(a=0, b = 1,col="blue")
cor.test(r_HL_1,r_EG_2,na.action=na.omit)
text(x = -1,y=1.5,paste("Corr=", 
                      round(cor.test(r_HL_1,r_EG_2,na.action=na.omit)$estimate,4)))

dev.off()

# Simulations for different sd_epsilons ----

sds_epsilon <- seq(0,1.5,by=0.1)

### HL
corr_real_estimated_error <- rep(NA,length(sds_epsilon))
corr_error <- rep(NA,length(sds_epsilon))

### EG
corr_real_estimated_error_EG <- rep(NA,length(sds_epsilon))
corr_error_EG <- rep(NA,length(sds_epsilon))

# Correlation between instruments
corr_error_EG_HL_1 <- rep(NA,length(sds_epsilon))
corr_error_EG_HL_2 <- rep(NA,length(sds_epsilon))

for(sd_epsilon in sds_epsilon){
  sd_epsilon_index <- which(sds_epsilon==sd_epsilon)
  
  # HL
  # for real r
  r_HL <- rep(NA,length(r_simulated))
  # for random utility deviations
  r_HL_1 <- rep(NA,length(r_simulated))
  r_HL_2 <- rep(NA,length(r_simulated))
  
  # EG 
  # for real r
  r_EG <- rep(NA,length(r_simulated))
  # for random utility deviations
  r_EG_1 <- rep(NA,length(r_simulated))
  r_EG_2 <- rep(NA,length(r_simulated))
  
  for (r in r_simulated) {
    decision_index <- which(r_simulated==r)
    
    # HL
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
    
    # EG
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
  
  # HL
  corr_real_estimated_error[sd_epsilon_index] <- cor.test(r_simulated,r_HL_1,na.action=na.omit)$estimate
  corr_error[sd_epsilon_index] <- cor.test(r_HL_1,r_HL_2,na.action=na.omit)$estimate
  
  # EG
  corr_real_estimated_error_EG[sd_epsilon_index] <- cor.test(r_simulated,r_EG_1,na.action=na.omit)$estimate
  corr_error_EG[sd_epsilon_index] <- cor.test(r_EG_1,r_EG_2,na.action=na.omit)$estimate
  
  # Correlation between instruments
  corr_error_EG_HL_1[sd_epsilon_index] <- cor.test(r_EG_1,r_HL_1,na.action=na.omit)$estimate
  corr_error_EG_HL_2[sd_epsilon_index] <- cor.test(r_EG_2,r_HL_2,na.action=na.omit)$estimate
  
}

# HL
corr_real_estimated_error
corr_error

pdf(file="results/corr_sd_epsilon_HL.pdf",
    width=6, height=4)
par(mfrow=c(1,1))

plot(sds_epsilon,corr_error,ylim = c(0,1),type = "l",col="darkred",
     main="Correlation as a function of the measurement error in HL",
     xlab = expression(sigma[epsilon]),
     ylab = "Correlation")
points(sds_epsilon,corr_real_estimated_error,type = "l",col="darkgreen")
legend(x = 1,y=1,legend = c("error-error","real-error"),col = c("darkred","darkgreen"),lty = 1)

dev.off()

# EG 
corr_real_estimated_error_EG
corr_error_EG

pdf(file="results/corr_sd_epsilon_EG.pdf",
    width=6, height=4)

plot(sds_epsilon,corr_error_EG,ylim = c(0,1),type = "l",col="darkred",
     main="Correlation as a function of the measurement error in EG",
     xlab = expression(sigma[epsilon]),
     ylab = "Correlation")
points(sds_epsilon,corr_real_estimated_error_EG,type = "l",col="darkgreen")
legend(x = 1,y=1,legend = c("error-error","real-error"),col = c("darkred","darkgreen"),lty = 1)

dev.off()

# correlation between instruments
corr_error_EG_HL_1
pdf(file="results/corr_sd_epsilon_EG_HL.pdf",
    width=6, height=4)

plot(sds_epsilon,corr_error_EG_HL_1,ylim = c(0,1),type = "l",col="darkred",
     main="Correlation between HL and EG as a function of the measurement error",
     xlab = expression(sigma[epsilon]),
     ylab = "Correlation")
points(sds_epsilon,corr_error_EG_HL_2,type = "l",col="darkgreen")
legend(x = 1,y=1,legend = c("Simulation 1","Simulation 2"),col = c("darkred","darkgreen"),lty = 1)

dev.off()


