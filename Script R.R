library(readr)
library(corrplot)
library(Hmisc) # al final no la usé
library(polycor)
library(ggplot2)
library(dplyr)


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



# Duplicando el dataframe para agregar imc
# Crear una copia del dataframe Smokers
smokers_2 <- Smokers
# Calcular el índice de masa corporal (IMC) y agregarlo como una nueva columna
smokers_2$BMI <- smokers_2$"weight(kg)" / ((smokers_2$"height(cm)" / 100)^2)
# Verificar la adición de la nueva columna
head(smokers_2)


"""
Correlación de las columnas con smoking
"""

# Incluir la columna 'smoking' en el cálculo de la matriz de correlación
correlation_matrix_smoking <- cor(smokers_2, Smokers$smoking)
# Imprimir la matriz de correlación
print("Matriz de correlación con respecto a 'smoking':")
print(correlation_matrix_smoking)

"
Variables Positivamente Correlacionadas: Variables como 'height(cm)', 'weight(kg)', 'triglyceride', 
'hemoglobin', y 'Gtp' muestran una correlación positiva con 'smoking'. Esto significa que, en general, 
a medida que aumenta el valor de 'smoking', también tienden a aumentar estas variables, y viceversa.

Variables Negativamente Correlacionadas: Variables como 'age', 'HDL', y 'LDL' muestran una correlación 
negativa con 'smoking'. Esto indica que a medida que aumenta el valor de 'smoking', estas variables 
tienden a disminuir, y viceversa.
"

# GRÁFICA
# Calcular la matriz de correlación 
correlation_matrix <- cor(smokers_2[, -1])
# Configurar la cuadrícula de gráficos
par(mar = c(1, 1, 1, 1))
# Crear el gráfico de matriz de correlación
corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(100),
         tl.col = "black", tl.cex = 0.7)


# Buscando índices que suelen estar relacionados con fumar, perfil lipídico, hipertensión, función renal
# Calcular índices
smokers_2$lipid_ratio <- (smokers_2$HDL + smokers_2$LDL) / smokers_2$Cholesterol
smokers_2$systolic_relaxation_ratio <- smokers_2$systolic / smokers_2$relaxation
smokers_2$renal_function <- smokers_2$"serum creatinine" / smokers_2$"Urine protein"

# Calcular la correlación con la columna "smoking"
cor_with_smoking <- sapply(smokers_2[, c("lipid_ratio", "systolic_relaxation_ratio", "renal_function", "BMI", "smoking")], function(x) cor(x, smokers_2$smoking))
# Mostrar la correlación
cor_with_smoking

# En mapa de calor
# Calcular la matriz de correlación
cor_matrix2 <- cor(smokers_2[, c("lipid_ratio", "systolic_relaxation_ratio", "renal_function", "BMI", "smoking")])

# Crear el mapa de calor
heatmap(cor_matrix2, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Correlación con Smoking",
        xlab = "Índices",
        ylab = "Índices",
        cex.main = 1.5,
        cex.axis = 1.2,
        margins = c(10, 10),
        symm = TRUE)
# No parece haber mucha correlación entre los índices, si a caso con renal_function y con
# lipid ratio, pero el resto sale casi neutral


