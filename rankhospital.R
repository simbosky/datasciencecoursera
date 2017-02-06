### Takes 3 args: 2 character State name (state), outcome (outcome), ranking (num) for hospital
### num can take "best", "worst" or an integer
### Returns character vector with name of hospital in that place
### ties broken by alphabetical order
### if num larger than num hospitals then return NA

rankhospital <- function(state, outcome, num="best"){
  ##set up libraries
  library(dplyr)
  
  ## read outcome data
  hospital_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## check that state and outcome are valid
  if(! state %in% hospital_data$State){ stop ("invalid state", call. = TRUE)}
  if(! outcome %in%  c("heart attack", "heart failure", "pneumonia")) {stop ("invalid outcome", call. = TRUE)}
  
  ##return hospital with that rank
  
  ### choose column for death rate based on outcome
  dr <- 0
  if(outcome == "heart attack"){dr <- 11 }
  else if(outcome == "heart failure"){dr <- 17}
  else { dr <- 23}
  
  ### create the best rate column and choose the winner
  winner <- hospital_data %>% mutate(best_rate = as.numeric(hospital_data[[dr]])) %>% 
    filter(State == state & !is.na(best_rate)) %>% 
    arrange(best_rate, Hospital.Name) %>% select(Hospital.Name, best_rate)
  
  ### find best and worst
  i <- 0
  if(num=="best"){i <- 1}
  else if(num=="worst"){i <- nrow(winner)}
  else if (num> nrow(winner)){return(NA)}
  else {i <- num}
  
  winner[i ,1]
  
}
