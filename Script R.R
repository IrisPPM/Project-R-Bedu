library(readr)
Smokers <- read_csv("data/Smokers.csv")
View(Smokers)
dim(Smokers)

tail(Smokers)

summary(Smokers)

str(Smokers)

names(Smokers)

nrow(Smokers)
ncol(Smokers)

hist(Smokers$age)

plot(Smokers$age, Smokers$smoking)
