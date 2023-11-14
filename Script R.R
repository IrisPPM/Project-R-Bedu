library(readr)
Smokers <- read_csv("data/Smokers.csv")
View(Smokers)
dim(Smokers)

tail(Smokers)

# Mínimo, media, promedio y quantiles de cada columna.
summary(Smokers)

str(Smokers)

names(Smokers)

nrow(Smokers)
ncol(Smokers)

# Histograma de la edad
hist(Smokers$age, main = "Histograma de Edad", xlab = "Edad", ylab = "Frecuencia")

# Scatterplot de peso vs altura
plot(Smokers$`height(cm)`, Smokers$`weight(kg)`, 
     main = "Scatterplot de Peso vs Altura", 
     xlab = "Altura (cm)", ylab = "Peso (kg)")

# Correlación entre variables numéricas
correlation_matrix <- cor(Smokers[, c("age", "height(cm)", "weight(kg)", "Cholesterol", "HDL", "LDL")])
print("Matriz de correlación:")
print(correlation_matrix)

# Contar valores faltantes por columna
missing_values <- colSums(is.na(Smokers))
# Mostrar el número de valores faltantes por columna
print("Valores faltantes por columna:")
print(missing_values)

