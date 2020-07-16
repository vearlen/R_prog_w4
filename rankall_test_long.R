rankall_test2 <- function(outcome = "heart attack", num = "best"){
  
#read outcome data
  input <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
  
#process if selected heart attack
  if(outcome == "heart attack"){
    
#convert character to numbers for heart attack
    input[,11] <- suppressWarnings(as.numeric(input[,11]))
    
#save and rank all hospitals within each state
    all_names <- input %>%
      select(c(2,7,11))%>%
      na.omit() %>% #remove NA's
      select(Hospital.Name, State,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,) %>%
      group_by(State) %>%
      arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name,.by_group = TRUE) %>%
      mutate(rank = row_number(), hospital = Hospital.Name, state=State)
    
#create list of states with max hospital number   
    max_rank_state <- all_names %>%
      group_by(State)%>%
      mutate(state=State)%>%
      summarise(maxno = max(rank)) 
    
#process num input depends on value
    if(num == "best"){
      selected <- all_names %>%
        group_by(state)%>%
        filter(rank == 1) %>% #best equal 1
        select(hospital, state)
      #print table
      selected
    }
    else if (num == "worst"){
      selected <- all_names %>%
        group_by(state)%>%
        filter(rank == max(rank)) %>% # worst equal last/max
        select(hospital, state)
      #print table
      selected
    }

#case for numeric input of num        
    else{
      selected <- all_names %>%
        group_by(state)%>%
        filter(rank == {{num}}) %>%
        select(hospital,state)
      
#create data frame with cases per state less then input num
      na_hosp <- max_rank_state %>% 
        mutate( cutoff = (num > max_rank_state$maxno)) %>%
        filter (cutoff == TRUE) %>%
        rename(state = State) %>%
        mutate (hospital = NA)%>%
        select(hospital, state)   
      
#bind NA cases and valid and print
      result <- rbind(selected, na_hosp) %>%
        arrange (state)
      
      result #print results
    }
  }
 
#process if selected pneumonia   
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
    
#create list of states with max hospital number   
    max_rank_state <- all_names %>%
      group_by(State)%>%
      mutate(state=State)%>%
      summarise(maxno = max(rank)) 
    
#process num input depends on value "best"
    if(num == "best"){
      selected <- all_names %>%
        group_by(state)%>%
        filter(rank == 1) %>%
        select(hospital, state)
      #print table
      selected
    }

#process num input depends on value "worst"    
    else if (num == "worst"){
      selected <- all_names %>%
        group_by(state)%>%
        filter(rank == max(rank)) %>%
        select(hospital, state)
      #print table
      selected
    }

#process num input depends on numeric value     
    else{
      selected <- all_names %>%
        group_by(state)%>%
        filter(rank == {{num}}) %>%
        select(hospital,state)
      
#create data frame with cases per state less then input num
      na_hosp <- max_rank_state %>% 
        mutate( cutoff = (num > max_rank_state$maxno)) %>%
        filter (cutoff == TRUE) %>%
        rename(state = State) %>%
        mutate (hospital = NA)%>%
        select(hospital, state)   
      
#bind and print
      result <- rbind(selected, na_hosp) %>%
        arrange (state)
      
      result
    }
  }

#process if selected heart failure
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
    
#create list of states with max hospital number   
    max_rank_state <- all_names %>%
      group_by(State)%>%
      mutate(state=State)%>%
      summarise(maxno = max(rank)) 
    
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
      
      #create data frame with cases less then input num
      na_hosp <- max_rank_state %>% 
        mutate( cutoff = (num > max_rank_state$maxno)) %>%
        filter (cutoff == TRUE) %>%
        rename(state = State) %>%
        mutate (hospital = NA)%>%
        select(hospital, state)   
      
      #bind and print
      result <- rbind(selected, na_hosp) %>%
        arrange (state)
      
      result
    }
  }
  
  else{
    stop("invalid outcome")
  }
}