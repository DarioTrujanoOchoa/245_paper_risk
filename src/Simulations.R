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

crra <- function(eta=0.5,x=1){
  if(eta==1){log(x)}
  else{ x^(1-eta)/(1-eta)}
}

r_choices_HL <- function(r=.5,FUN=crra, 
                         option_A_payoffs=c(2,1.6), 
                         option_B_payoffs=c(3.85,.1)){
  prob_high_payoff <- seq(0.1,1,by=0.1)
  list_length <- length(prob_high_payoff)
  # Objects to be filled for return
  EuA <- rep(NA,list_length)
  EuB <- rep(NA,list_length)
  choose_A <- rep(NA,list_length)
  lower_bound_r <- rep(NA,list_length)
  # Risk analysis of the list (for each risk level r)
  for (pH in prob_high_payoff) {
    decision_index <- which(prob_high_payoff==pH)
    EuA[decision_index] <- c(pH,1-pH)%*%FUN(r,option_A_payoffs)
    EuB[decision_index] <- c(pH,1-pH)%*%FUN(r,option_B_payoffs)
    choose_A[decision_index] <- EuA[decision_index]>=EuB[decision_index]
    if(decision_index<list_length){
      # solve to find the r that make indifferent both lotteries
      lower_bound_r[[decision_index]] <- 
        nleqslv::nleqslv(.01,
                         fn = function(r){
                           c(pH,1-pH)%*%FUN(r,option_A_payoffs)-
                             c(pH,1-pH)%*%FUN(r,option_B_payoffs)})$x
    }else{# In the last option, B is dominated
      lower_bound_r[[decision_index]] <- NA
    }
    
  }
  choose_A[1] <- 1 # we force the first decision to be safe
  return(as.data.frame(cbind(EuA=EuA,EuB=EuB,
                             choose_A=choose_A,
                             lower_bound_r=lower_bound_r)))
}

r_elicited_HL <- function(choices){ # it uses the ouput from r_choices_HL
  last_A  <- choices$choose_A - dplyr::lead(choices$choose_A)
  first_B <- dplyr::lag(choices$choose_A) -choices$choose_A
  lower_bound <- choices$lower_bound_r[which(last_A==1)]
  upper_bound <- choices$lower_bound_r[which(first_B==1)]
  if(which(last_A==1)<(length(choices$choose_A)-1)){
    # the last option is always NA: no r can rationalize the last option to be B
    # if choosing the penultimate option, then the function returns the lower bound
    # this will create a censored data when r > the indiference point
    middle_r <- (upper_bound+lower_bound)/2
  }else{
    middle_r <- lower_bound
    # consider that last lower bound, this will create censored data
  }
  return(middle_r)
}

r_choices_HL(1.4)
r_elicited_HL(r_choices_HL(r = 1.4))

# # to try the functions
choices <- r_choices_HL(r = -1.8,FUN = crra,option_A_payoffs, option_B_payoffs)
choices
plot(choices$lower_bound_r)
r_elicited_HL(choices)

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

## plots ----
pdf(file="results/corr_HL.pdf",
    width=8, height=4)
par(mfrow=c(1,2))
# relation with the real
plot(r_simulated,r_HL)
abline(a=0, b = 1)
cor.test(r_simulated,r_HL,na.action=na.omit)

# relation with the repeated mesure
plot(r_HL_1,r_HL_2)
abline(a=0, b = 1)
cor.test(r_HL_1,r_HL_2,na.action=na.omit)

dev.off()

# EG ----
## Payoffs
## C&P 2015
safe_payoffs <- c(4,4) # safest option
event_B_down_jump <- 1
event_differential_proportion <- c(-1,2)
n_rows_EG <- 5

r_choices_EG <- function(r=.5,FUN=crra, 
                         safe_payoffs = c(4,4), 
                         event_B_down_jump = 1,
                         event_differential_proportion = c(-1,2),
                         n_rows_EG=5){
  prob_high_payoff <- rep(0.5,n_rows_EG)
  list_length <- length(prob_high_payoff)
  # Objects to be filled for return
  Eu_row <- rep(NA,list_length)
  Eu_next_row <- rep(NA,list_length)
  # choose_row <- rep(NA,list_length)
  lower_bound_r <- rep(NA,list_length)
  # Risk analysis of the list (for each risk level r)
  decision_index <- 1
  for (pH in prob_high_payoff) {
    row_payoffs <- safe_payoffs+(decision_index-1)*event_differential_proportion
    next_row_payoffs <- safe_payoffs+(decision_index)*event_differential_proportion
    Eu_row[decision_index] <- c(pH,1-pH)%*%FUN(r,row_payoffs)
    Eu_next_row[decision_index] <- c(pH,1-pH)%*%FUN(r,next_row_payoffs)
    # find the indifference point
    if(decision_index<list_length){
      # solve to find the r that make indifferent both lotteries
      lower_bound_r[decision_index] <- 
        nleqslv::nleqslv(.01,
                         fn = function(r){
                           c(pH,1-pH)%*%FUN(r,row_payoffs)-
                             c(pH,1-pH)%*%FUN(r,next_row_payoffs)})$x
    }else{# In the last option, B is dominated
      lower_bound_r[[decision_index]] <- 0
    }
    choose_row <- Eu_row==max(Eu_row)
    decision_index <- decision_index+1
  }
  #choose_row[1] <- 1 # we force the first decision to be safe
  return(as.data.frame(cbind(Eu_row=Eu_row,
                             choose_row=choose_row,
                             lower_bound_r=lower_bound_r)))
}

r_choices_EG()

r_elicited_EG <- function(choices){ # it uses the ouput from r_choices_EG
  row_chosen  <- which(choices$choose_row==1)
  if(row_chosen!=1){
    lower_bound <- choices$lower_bound_r[row_chosen]
    upper_bound <- choices$lower_bound_r[row_chosen-1]
    middle_r <- (upper_bound+lower_bound)/2
  }else{
    middle_r <- lower_bound
    # we can select the lower bound, this is too conservative and will lead to underestimation.
  }
  return(middle_r)
}

r_elicited_EG(r_choices_EG())

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
