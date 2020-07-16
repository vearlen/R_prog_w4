rankhospital <- function(state,outcome = "heart attack",num = "best"){
  
  #read outcome data
  input <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")

  
  #create variable with state names
  st_names <- unique(input$State)
  
  if(state %in% st_names){ #check if state is correctly entered
    #process futher if input is heart attack
    if(outcome == "heart attack")
    {
      #convert character to numbers for heart attack
      input[,11] <- suppressWarnings(as.numeric(input[,11]))
      
      #save data frame with names in selected state with heart attack
      all_names <- input %>%
        select(c(2,7,11))%>%
        filter(State == {{state}}) %>%
        na.omit %>%
        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                Hospital.Name)%>%
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
    
    
    else if ( outcome == "pneumonia"){
      #convert character to numbers for pneumomia
      input[,23] <- suppressWarnings(as.numeric(input[,23]))
      
      #save data frame with names in selected state with pneumonia
      all_names <- input %>%
        select(c(2,7,23))%>%
        filter(State == {{state}}) %>%
        na.omit %>%
        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
                Hospital.Name)%>%
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
    
    
    else if ( outcome == "heart failure"){
      #convert character to numbers for pneumomia for heart failure
      input[,17] <- suppressWarnings(as.numeric(input[,17]))
      
      #save data frame with names in selected state with heart failure
     all_names <- input %>%
        select(c(2,7,17))%>%
        filter(State == {{state}}) %>%
        na.omit %>%
        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                Hospital.Name)%>%
        select(Hospital.Name) 
      
     #check number of rows
      nrows <- nrow(all_names)
      
      #compare input num with number of rows
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
    
    else{
      paste("Error in best(",state,",",outcome,") : invalid outcome")}
  }
  else{
    paste("Error in best(",state,",",outcome,") : invalid state")
  }
}