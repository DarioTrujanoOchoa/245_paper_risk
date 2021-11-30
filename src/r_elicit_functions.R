## Dario Trujano-Ochoa
## Function to elicit r from elicitation instruments

crra <- function(eta=0.5,x=1){
  if(eta==1){log(x)}
  else{ x^(1-eta)/(1-eta)}
}

# HL ----
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

# # try the functions
# r_choices_HL(1.4)
# r_elicited_HL(r_choices_HL(r = 1.4))
# 
# # # to try the functions
# choices <- r_choices_HL(r = -1.8,FUN = crra,option_A_payoffs, option_B_payoffs)
# choices
# plot(choices$lower_bound_r)
# r_elicited_HL(choices)

# EG ----
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
# 
# r_elicited_EG(r_choices_EG())
