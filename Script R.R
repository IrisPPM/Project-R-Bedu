library(readr)
library(corrplot)
library(Hmisc) # al final no la usé
library(polycor)

Smokers <- read_csv("data/Smokers.csv")

# Ver dataset
View(Smokers)

# Número de columnas y número de filas
dim(Smokers)

'''
head(Smokers)
tail(Smokers)
'''

# Mínimo, media, promedio y quantiles de cada columna.
summary(Smokers)
# El primer cuartil de edad coincide con la mediana. La mitad de los datos está entre 20-40,
# y una buena parte ronda los 40

str(Smokers)

# Nombres de columnas
names(Smokers)






# Scatterplot de peso vs altura. Nomás por curiosidad
plot(Smokers$`height(cm)`, Smokers$`weight(kg)`, 
     main = "Scatterplot de Peso vs Altura", 
     xlab = "Altura (cm)", ylab = "Peso (kg)")

# Función para cuantificar personas con sobrepeso y obesidad
calculate_overweight_obesity <- function(data) {
  # Calcular el índice de masa corporal (IMC)
  bmi <- data$"weight(kg)" / ((data$"height(cm)" / 100)^2)
  # Clasificar personas según el IMC
  categories <- cut(bmi, breaks = c(0, 18.5, 24.9, 29.9, Inf), labels = c("Bajo peso", "Normal", "Sobrepeso", "Obesidad"))
  # Contar personas en cada categoría
  count_by_category <- table(categories)
  # Imprimir resultados
  cat("Personas con bajo peso:", count_by_category["Bajo peso"], "\n")
  cat("Personas con peso normal:", count_by_category["Normal"], "\n")
  cat("Personas con sobrepeso:", count_by_category["Sobrepeso"], "\n")
  cat("Personas con obesidad:", count_by_category["Obesidad"], "\n")
  # Devolver el resultado como un vector
  return(count_by_category)
}
# Utilizar la función con el conjunto de datos Smokers
result <- calculate_overweight_obesity(Smokers)


# Graficando los resultados
# Histograma de personas con bajo peso, peso normal, sobrepeso y obesidad
barplot(result, main = "Distribución del IMC", 
        ylab = "Número de Personas", col = "skyblue", 
        names.arg = c("Bajo peso", "Normal", "Sobrepeso", "Obesidad"))

# Buscando correlación entre IMC y fumar
# Calcular el índice de masa corporal (IMC)
bmi <- Smokers$"weight(kg)" / ((Smokers$"height(cm)" / 100)^2)

# Crear un nuevo data frame con IMC y la columna "smoking"
data_for_correlation <- data.frame(IMC = bmi, Smoking = Smokers$smoking)

# Calcular la correlación de punto biserial
correlation <- cor.test(data_for_correlation$IMC, data_for_correlation$Smoking, method = "biserial")

# Imprimir la correlación
print(paste("Correlación entre IMC y Smoking (punto biserial):", correlation$estimate))







# Contar valores faltantes por columna
missing_values <- colSums(is.na(Smokers))
# Mostrar el número de valores faltantes por columna
print("Valores faltantes por columna:")
print(missing_values)


# HISTOGRAMA DE TODO
# Nombres de las columnas
column_names <- names(Smokers)
# Configurar la cuadrícula
par(mfrow = c(5, 5), mar = c(2, 2, 2, 2), cex.main = 0.8)
# Bucle para generar histogramas
for (col in column_names) {
  hist(Smokers[[col]], main = paste("Histograma de", col), xlab = col, ylab = "Frecuencia", col = "skyblue")
}
# Hay gente de entre 1.40 y 1.50 a pesar de que todos son mayores de 20 años.


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

