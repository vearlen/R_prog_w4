rankhospital <- function(state,outcome = "heart attack",num = "best"){
  
  #read outcome data
  input <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")

  
  #create variable with state names
  st_names <- unique(input$State)
  
  if(state %in% st_names){ #check if state is correctly entered

    if(outcome == "heart attack") {a <- "Hospital.Name, State,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
    x <- 11}
    else if (outcome == "heart failure"){a <- "Hospital.Name, State,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    x <- 17}
    else if (outcome == "pneumonia"){a <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    x <- 23}
    else {stop ("invalid outcome")}
  
    
    #convert character to numbers selected column
    input[,x] <- suppressWarnings(as.numeric(input[,x])) 
    
      
      #save data frame with names in selected state with heart attack
      all_names <- input %>%
        select(c(2,7,x))%>%
        filter(State == {{state}}) %>%
        na.omit %>%
        arrange(a,Hospital.Name)%>%
        select(Hospital.Name)
        
      #check number of rows
        nrows <- nrow(all_names)
  
        #compare num with number of rows
        if(num <= nrows){
          best_name <- slice(all_names,num)
          best_name[[1]]
        }
        else if(num == "best"){
          best_name <- slice_head(all_names)
          best_name[[1]]
        }
        else if (num == "worst"){
          best_name <- slice_tail(all_names)
          best_name[[1]]
        }
        else  if (num > nrows){
          NA
        }
  }

  else{stop ("invalid state")}
    
}