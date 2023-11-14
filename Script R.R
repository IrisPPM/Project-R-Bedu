library(readr)
library(corrplot)
Smokers <- read_csv("data/Smokers.csv")
View(Smokers)
dim(Smokers)

"""
head(Smokers)
tail(Smokers)
"""

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




# Contar valores faltantes por columna
missing_values <- colSums(is.na(Smokers))
# Mostrar el número de valores faltantes por columna
print("Valores faltantes por columna:")
print(missing_values)


# Nombres de las columnas
column_names <- names(Smokers)
# Configurar la cuadrícula
par(mfrow = c(5, 5), mar = c(2, 2, 2, 2), cex.main = 0.8)
# Bucle para generar histogramas
for (col in column_names) {
  hist(Smokers[[col]], main = paste("Histograma de", col), xlab = col, ylab = "Frecuencia", col = "skyblue")
}


"""
Correlación de las columnas con smoking
"""

# Seleccionar solo las variables numéricas para el cálculo de correlación
numeric_variables <- sapply(Smokers, is.numeric)
# Incluir la columna 'smoking' en el cálculo de la matriz de correlación
correlation_matrix_smoking <- cor(Smokers[, numeric_variables, drop = FALSE], Smokers$smoking)
# Imprimir la matriz de correlación
print("Matriz de correlación con respecto a 'smoking':")
print(correlation_matrix_smoking)

"""
Variables Positivamente Correlacionadas: Variables como 'height(cm)', 'weight(kg)', 'triglyceride', 
'hemoglobin', y 'Gtp' muestran una correlación positiva con 'smoking'. Esto significa que, en general, 
a medida que aumenta el valor de 'smoking', también tienden a aumentar estas variables, y viceversa.

Variables Negativamente Correlacionadas: Variables como 'age', 'HDL', y 'LDL' muestran una correlación 
negativa con 'smoking'. Esto indica que a medida que aumenta el valor de 'smoking', estas variables 
tienden a disminuir, y viceversa.
"""

# GRÁFICA
# Calcular la matriz de correlación 
correlation_matrix <- cor(Smokers[, -1])
# Configurar la cuadrícula de gráficos
par(mar = c(1, 1, 1, 1))
# Crear el gráfico de matriz de correlación
corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(100),
         tl.col = "black", tl.cex = 0.7)

