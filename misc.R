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
