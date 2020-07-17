rankall <- function(outcome = "heart attack", num = "best"){
  
  #read outcome data
  input <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
  

#check input outcome and save column names accordingly OR stop
  if(outcome == "heart attack") {a <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
  x <- 11}
  else if (outcome == "heart failure"){a <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  x <- 17}
  else if (outcome == "pneumonia"){a <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  x <- 23}
else {stop ("invalid outcome")}
  
#convert character to numbers selected column
  input[,x] <- suppressWarnings(as.numeric(input[,x])) 
#rename target column for arrange ()
  input <- rename(input, dmr = a )
#save and rank all hospital within each state
  all_names <- input %>%
    select(all_of(c(2,7,x)))%>%
    na.omit() %>%
    group_by(State) %>%
    arrange(dmr,Hospital.Name,.by_group = TRUE) %>%
    mutate(rank = row_number(), hospital = Hospital.Name, state=State)
  
#create list of states with max hospital number   
  max_rank_state <- all_names %>%
    group_by(State)%>%
    mutate(state=State)%>%
    summarise(maxno = max(rank)) 
  
#process num variable input when value is "best"
  if(num == "best"){
    selected <- all_names %>%
      group_by(state)%>%
      filter(rank == 1) %>%
      select(hospital, state)
    #print table
    selected
  }
  
  #process num variable input when  value is "worst"    
  else if (num == "worst"){
    selected <- all_names %>%
      group_by(state)%>%
      filter(rank == max(rank)) %>%
      select(hospital, state)
    #print table
    selected
  }
  
  #process num input variable when numeric value     
  else{
    selected <- all_names %>%
      group_by(state)%>%
      filter(rank == {{num}}) %>%
      select(hospital,state)
    
#create data frame with cases per state less then input num when numeric
    na_hosp <- max_rank_state %>% 
      mutate( cutoff = (num > max_rank_state$maxno)) %>%
      filter (cutoff == TRUE) %>%
      rename(state = State) %>%
      mutate (hospital = NA)%>%
      select(hospital, state)   
    
#bind NA states and valid cases and print
    result <- rbind(selected, na_hosp) %>%
      arrange (state)
    
    result
  }
}