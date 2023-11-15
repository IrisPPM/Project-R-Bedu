## 14-NOV
##AED - ALEJANDRO FLORES C


#cargar paquetes
if (!require("tidyverse")) install.packages("tidyverse")
install.packages("corrplot")
library(tidyverse)
library(readr)
library(corrplot)

#cargar dataset
smokers <- read.csv("data/Smokers.csv")
smokers
view(smokers)


#exploración inicial
head(smokers)
summary(smokers)
str(smokers)
dim(smokers)

nrow(smokers)
ncol(smokers)

#datos faltantes
colSums(is.na(smokers))
#NO HAY DATOS FALTANTES

head(smokers)
tail(smokers)

#gráficas
hist(smokers$age)

plot(smokers$age, smokers$Cholesterol)

boxplot(smokers$age)

barplot(table(smokers$Cholesterol))

heatmap(cor(smokers))


"""
correlación especial
"""
correlation_matrix <- cor(smokers[, -1])
correlation_matrix
par(mar = c(1, 1, 1, 1))
corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(100),
         tl.col = "black", tl.cex = 0.7)
