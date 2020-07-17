best <- function(state,outcome = "heart attack"){
  
  #read outcome data
  input <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
  
  #create variable with state names
  st_names <- unique(input$State)
  
  if(state %in% st_names){ #check if state is correctly entered
    
    if(outcome == "heart attack") {a <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
    x <- 11}
    else if (outcome == "heart failure"){a <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    x <- 17}
    else if (outcome == "pneumonia"){a <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    x <- 23}
    else {stop ("invalid outcome")}
  }
  else{stop ("invalid state")}
  
  #convert character to numbers selected column
  input[,x] <- suppressWarnings(as.numeric(input[,x])) 
  #rename target column for arrange ()
  input <- rename(input, dmr = a )

  best_name <- input %>%
    select(c(2,7,x))%>%
    filter(State == {{state}}) %>%
    arrange(dmr,
            Hospital.Name)%>%
    select(Hospital.Name) %>%
    slice_head() 
  best_name[[1]]
}