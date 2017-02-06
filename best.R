### function to return best state for a given outcome
### takes 2 arguments - 2 letter abbreviation of state and an outcome name from the outcome-of-care-measures
### outcomes can be "heart attack", "pneumonia", or "heart failure"
### hospital name is in the Hospital.Name variable
### ties sorted by alphabetical order

best <- function (state, outcome){
  ##set up libraries
  library(dplyr)
  
  ## read outcome data
  hospital_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## check that state and outcome are valid
  if(! state %in% hospital_data$State){ stop ("invalid state", call. = TRUE)}
  if(! outcome %in%  c("heart attack", "heart failure", "pneumonia")) {stop ("invalid outcome", call. = TRUE)}
  
  ## return hospital name in state with the lowest 30-day death rate
  
  # choose column for death rate based on outcome
  dr <- 0
  if(outcome == "heart attack"){dr <- 11 }
  else if(outcome == "heart failure"){dr <- 17}
  else { dr <- 23}
  
  #create the best rate column and choose the winner
  winner <- hospital_data %>% mutate(best_rate = as.numeric(hospital_data[[dr]])) %>% 
    filter(State == state & !is.na(best_rate)) %>% 
    arrange(best_rate, Hospital.Name) %>% select(Hospital.Name)
  
  #return winner
  winner[1,1]
  
}