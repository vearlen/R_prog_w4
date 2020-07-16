rankall <- function(outcome = "heart attack", num = "best"){
  
  #read outcome data
  input <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
  
  if(outcome == "heart attack"){
    
    #convert character to numbers for heart attack
    input[,11] <- suppressWarnings(as.numeric(input[,11]))
    
    #save and rank all hospital within each state
    all_names <- input %>%
      select(c(2,7,11))%>%
      na.omit() %>%
      select(Hospital.Name, State,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,) %>%
      group_by(State) %>%
      arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name,.by_group = TRUE) %>%
      mutate(rank = row_number(), hospital = Hospital.Name, state=State)
    
    
    #save selected states
    if(num == "best"){
    selected <- all_names %>%
      group_by(state)%>%
      filter(rank == 1) %>%
      select(hospital, state)
      #print table
      selected
    }
    else if (num == "worst"){
      selected <- all_names %>%
        group_by(state)%>%
        filter(rank == max(rank)) %>%
        select(hospital, state)
      #print table
      selected
    }
    
    else{
      selected <- all_names %>%
        group_by(state)%>%
        filter(rank == {{num}}) %>%
        select(hospital,state)
      #print table
      selected
    }
  }
  
  else if(outcome == "pneumonia"){
    
    #convert character to numbers for pneumonia
    input[,23] <- suppressWarnings(as.numeric(input[,23]))
    
    #save and rank all hospital within each state
    all_names <- input %>%
      select(c(2,7,23))%>%
      na.omit() %>%
      select(Hospital.Name, State,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) %>%
      group_by(State) %>%
      arrange(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name,.by_group = TRUE) %>%
      mutate(rank = row_number(), hospital = Hospital.Name, state=State)
    
    
    #save selected states
    if(num == "best"){
      selected <- all_names %>%
        group_by(state)%>%
        filter(rank == 1) %>%
        select(hospital, state)
      #print table
      selected
    }
    else if (num == "worst"){
      selected <- all_names %>%
        group_by(state)%>%
        filter(rank == max(rank)) %>%
        select(hospital, state)
      #print table
      selected
    }
    
    else{
      selected <- all_names %>%
        group_by(state)%>%
        filter(rank == {{num}}) %>%
        select(hospital,state)
      #print table
      selected
    }
  }
  
  else if(outcome == "heart failure"){
    
    #convert character to numbers for heart failure
    input[,17] <- suppressWarnings(as.numeric(input[,17]))
    
    #save and rank all hospital within each state
    all_names <- input %>%
      select(c(2,7,17))%>%
      na.omit() %>%
      select(Hospital.Name, State,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) %>%
      group_by(State) %>%
      arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name,.by_group = TRUE) %>%
      mutate(rank = row_number(), hospital = Hospital.Name, state=State)
    
    
    #check what is input in num variable
    #and proceed accordingly
    
    if(num == "best"){
      selected <- all_names %>%
        group_by(state)%>%
        filter(rank == 1) %>%
        select(hospital, state)
      #print table
      selected
    }
    else if (num == "worst"){
      selected <- all_names %>%
        group_by(state)%>%
        filter(rank == max(rank)) %>%
        select(hospital, state)
      #print table
      selected
    }
    
    else{
      selected <- all_names %>%
        group_by(state)%>%
        filter(rank == {{num}}) %>%
        select(hospital,state)
      #print table
      selected
    }
  }
  
  else{
    paste("Error in best(",state,",",outcome,") : invalid outcome")
  }
}