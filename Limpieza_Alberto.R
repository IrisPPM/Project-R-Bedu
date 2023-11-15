# 1. Importación de librerías y del dataset

# Instalar paqueterías
# install.packages('tidyverse')
# install.packages('reshape')
# install.packages('GGally')

# Importar librerías
library(tidyverse)
library(reshape)
library(GGally)


# Importar dataset
setwd('/Users/albertomartinezgarcia/Downloads')
df <- read.csv('Smokers.csv')

View(df)

# 2. Preprocesamiento de data

## 2.1 Exploración y limpieza de datos

# Tipos de datos
glimpse(df)

'''
Se puede cambiar a V/F las columnas de caries y smoking
'''

df$smoking <- as.logical(df$smoking)
df$dental.caries <- as.logical(df$dental.caries)

glimpse(df)

'''
Se cambiaron a V/F las columnas de caries y smoking
'''


# Valores nulos o faltantes
sum(is.na(df))

'''
No hay valores nulos o faltantes
'''

# Filas duplicadas
nrow(df[duplicated(df), ])

'''
No hay valores filas duplicadas
'''

# Cambiar nombres de columnas para tener formatos consistentes
colnames(df) <- c("id", "age", "height_cm", "weight_cm", "waist_cm", "eyesight_l", 
                  "eyesight_r", "hearing_l", "hearing_r", "systolic", "relaxation",
                  "blood_sugar", "cholesterol", "triglyceride", "hdl", "ldl",
                  "hemoglobin", "urine_protein", "serum_creatinine", "ast", 
                  "alt", "gtp", "caries", "smoking")

colnames(df)

'''
Se cambiaron los nombres de columnas. Se acortaron nombres, 
se utilizan "_" y todo está escrito en minúsculas.
'''


## 2.2 Exploración del dataset

# Valores únicos
unique(df$age)
unique(df$height)
unique(df$weight_cm)
unique(df$waist_cm)
'''
En cuanto a las variables con medidas, waist_cm tiene mucha más variabilidad que
height y weight_cm.
'''

unique(df$eyesight_l)
unique(df$eyesight_r)
unique(df$systolic)
unique(df$relaxation)
unique(df$blood_sugar)
unique(df$cholesterol)
unique(df$triglyceride)

'''
Triglyceride tiene muchos más valores únicos que las demás columnas (392) 
cholesterol, blood_sugar también tienen muchos valores (227 y 229)
'''

unique(df$hdl)
unique(df$ldl)

'''
ldl tiene aproximadamente el doble de valores que hdl (222 vs 108)
'''


unique(df$hemoglobin)
unique(df$urine_protein)
unique(df$serum_creatinine)
unique(df$ast)
unique(df$alt)
unique(df$gtp)
unique(df$caries)
unique(df$smoking)

'''
gtp es la columna con más valores (362). Le sigue alt (188) y ast (140).
'''
