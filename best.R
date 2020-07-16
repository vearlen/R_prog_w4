best <- function(state,outcome = "heart attack"){
  library(tidyverse)
  #read outcome data
  input <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
  input[,11] <- suppressWarnings(as.numeric(input[,11]))
  input[,23] <- suppressWarnings(as.numeric(input[,23]))
  input[,17] <- suppressWarnings(as.numeric(input[,17]))
  
  #create variable with state names
  st_names <- unique(input$State)
    
  if(state %in% st_names){
    
  if(dis == "heart attack")
  {
  #return hospital name in selected state with heart attack
  best_name <- input %>%
    select(c(2,7,11))%>%
    filter(State == {{state}}) %>%
    arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
            Hospital.Name)%>%
    select(Hospital.Name) %>%
    slice_head() 
  best_name[[1]]
  }
  else if ( dis == "pneumonia"){
  #return hospital name in selected state with heart attack
  best_name <- input %>%
    select(c(2,7,23))%>%
    filter(State == {{state}}) %>%
    arrange(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
            Hospital.Name)%>%
    select(Hospital.Name) %>%
    slice_head() 
  best_name[[1]]
  }
  else if ( dis == "heart failure"){
    #return hospital name in selected state with heart attack
    best_name <- input %>%
      select(c(2,7,17))%>%
      filter(State == {{state}}) %>%
      arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
              Hospital.Name)%>%
      select(Hospital.Name) %>%
      slice_head() 
    best_name[[1]]
  }
  else{
    paste("Error in best(",state,",",dis,") : invalid outcome")}
  }
  else{
    paste("Error in best(",state,",",dis,") : invalid state")
  }
}