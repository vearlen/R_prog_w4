outcome <- read.csv ("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
outcome[,11] <- as.numeric(outcome[,11])
str(outcome[,11])
colnames(outcome)

hist(outcome[,11])


x <- rnorm(100,15,10)
hist(x)

ggplot(data=outcome,aes(x=outcome[,11]))

outcome %>%
ggplot()+
geom_histogram(aes(outcome[,11]),
               stat="count")

outcome %>%
  ggplot()+
  geom_histogram(aes(outcome[,11]),binwidth = .5)

# rankhospital ------------------------------------------------------------
a <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
input[,17] <- suppressWarnings(as.numeric(input[,17]))
all_names <- input %>%
  select(c(2,7,17))%>%
  filter(State == "TX") %>%
  na.omit %>%
  group_by (State) %>%
  arrange(a,Hospital.Name, .by_group = TRUE)
  select(Hospital.Name,a)


best_name <- outcome %>%
  select(c(2,7,11))%>%
  filter(State == "TX") %>%
  arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
          Hospital.Name) %>%
  na.omit() %>%
  select(Hospital.Name) 
#store num of rows
nrows <- nrow(best_name)

case_when(
  num == "best" ~ slice_head(best_name),
  num == "worst" ~ slice_tail(best_name)) 

if (num == "best"){
  slice_head(best_name)
}
else if(num == "worst"){
  slice_tail(best_name)
}

slice_tail(best_name)

x <- 1:10
case_when(
  num =="worst" ~ x[1],
  num == "best" ~ slice_tail(x),
  print("wrong input")
)


# rank all prototype ------------------------------------------------------
num <- 20

input <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
a <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")

input <- rename(input, dmr = a )
x <- 11
input[,x] <- suppressWarnings(as.numeric(input[,x]))
b <- c(a, "Hospital.Name","State")
a

all_names <- input %>%
  select(c(2,7,x))%>%
  na.omit() %>%
  group_by(State) %>%
  arrange(dmr, Hospital.Name, .by_group = TRUE) %>%
  mutate(rank = row_number(), hospital = Hospital.Name, state=State)

max_rank_state <- all_names %>%
  group_by(State)%>%
  mutate(state=State)%>%
  summarise(maxno = max(rank)) 

selected <- all_names %>%
  group_by(state)%>%
  filter(rank == 20) %>%
  select(hospital,state)


na_hosp <- max_rank_state %>% mutate( cutoff = (num > max_rank_state$maxno)) %>%
  filter (cutoff == TRUE) %>%
  rename(state = State) %>%
  mutate (hospital = NA)%>%
  select(hospital, state)

rbind(selected, na_hosp) %>% arrange(state)


# View(max_rank_state)

all_names %>%
  filter(state== "FL")%>%
  # filter(rank == 20) %>%
  # select(hospital, state, rank)%>%
  View()  
head(10)

