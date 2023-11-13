install.packages("AER")
library(AER)

library(readr)
Smokers <- read_csv("data/Smokers.csv")
View(Smokers)

#Realizando un analisis exploratorio de datos

str(Smokers)

head(Smokers)

summary(Smokers) #muestra mediana, media, 1erqtil

attach(Smokers)

mean(age)
median(age)
min(age)
max(age)


mean(`weight(kg)`)
var(`weight(kg)`)
sd(`weight(kg)`)

hist(log(age),freq = FALSE)

Summary(`hearing(left)`)
table(`hearing(left)`)

xtabs(~age + `weight(kg)`, data=Smokers)
plot(age ~ `weight(kg)`, data=Smokers)

cor(age,`weight(kg)`)
#correlacion de Pearson
cor(log(age),`weight(kg)`)
#Grafica
plot(log(age)~`weight(kg)`)


