rm(list = ls())

library(treemap)
library(tidyverse)

# Indicamos la ruta completa del archivo
datos <- read.csv("bol_pib_dpto2022.csv")

# Verificamos la estructura del dataframe
str(datos)

# Visualizamos las primeras filas
head(datos)

# Seleccionamos las variables de interés
dpto <- datos$dpto
pib2022 <- datos$pib2022

# Abrimos un dispositivo gráfico PNG
png("bolivia_treemap_pib2022_x_dpto.png", width = 800, height = 600)

# Creamos el treemap
treemap(
  dtf = datos,
  index = "dpto",
  vSize = "pib2022",
  fontface.labels = 12,
  border.col = "black",
  labels = TRUE,
  title = "BOLIVIA - PIB 2022 por departamento"
)

# Cerramos el dispositivo gráfico
dev.off()
