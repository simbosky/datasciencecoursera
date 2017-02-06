### takes 2 args: outcome name (outcome) and hospital-ranking (num)
### returns a 2 column dataframe containing the hospital in each state that are the best in their respective states
### returns a value for every state which may be an NA
### remove NAs to decide ranking

rankall <- function(outcome, num="best"){

  ##set up libraries
  library(dplyr)
  
  ## set up list of states
  state=c('AL','AK','AZ','AR',
              'CA','CO','CT','DC','DE',
              'FL','GA','HI','ID',
              'IL','IN','IA','KS',
              'KY','LA','ME','MD',
              'MA','MI','MN','MS',
              'MO','MT','NE','NV',
              'NH','NJ','NM','NY',
              'NC','ND','OH','OK',
              'OR','PA','RI','SC',
              'SD','TN','TX','UT','VI',
              'VT','VA','WA','WV',
              'WI','WY'
              )
  
  ##set up hospitals and initialise to NA
  
  hospital=rep(NA, 50)
  rnk = rep(0, 50)
  #df <- data.frame(hospital, state, rnk)
  df <- data.frame(state)
  
  ## read outcome data
  hospital_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## check that outcome is valid
  if(! outcome %in%  c("heart attack", "heart failure", "pneumonia")) {stop ("invalid outcome", call. = TRUE)}
  
  ### choose column for death rate based on outcome
  dr <- 0
  if(outcome == "heart attack"){dr <- 11 }
  else if(outcome == "heart failure"){dr <- 17}
  else { dr <- 23}
  
  
  ### create the best rate column and choose the winner
  winner <- hospital_data %>% mutate(best_rate = as.numeric(hospital_data[[dr]]),state=State, hospital=Hospital.Name) %>% 
    select(hospital, state, best_rate) %>%
    filter(!is.na(best_rate)) %>%
    arrange(state, best_rate, hospital) %>%
    group_by(state) %>%
    mutate (rnk =row_number()) %>%
    select(hospital, state, rnk)
  
  
  if(num=="best"){
    
    df <- merge(df, winner[winner$rnk==1,], by="state", all.x= TRUE)
   
   }
  else if (num == "worst"){
    #return(output[output$rank==max(output$rank),])
    worst <- winner %>% group_by(state) %>% summarize(rnk = max(rnk))
    df <- merge(df, worst, by="state", all.x=TRUE)
    df <- merge(df, winner, by=c("state","rnk"), all.x=TRUE)
  }
  else{
    
    df <- merge(df, winner[winner$rnk==num,], by="state", all.x= TRUE)
    
  }
  
  df[,c("hospital", "state")]
  
  }