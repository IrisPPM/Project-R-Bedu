## 14-NOV
##AED - ALEJANDRO FLORES C


#cargar paquetes
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

#cargar dataset
smokers <- read.csv("data/Smokers.csv")
smokers
view(smokers)


#exploración inicial
head(smokers)
summary(smokers)
str(smokers)
dim(smokers)

#datos faltantes
colSums(is.na(smokers))
#NO HAY DATOS FALTANTES


#gráficas
hist(smokers$age)

plot(smokers$age, smokers$Cholesterol)

boxplot(smokers$age)

barplot(table(smokers$Cholesterol))

heatmap(cor(smokers))

median(age)
