## https://github.com/koenderks/rcityviews
## An R package and Shiny application to render minimalistic aerial city views using data from OpenStreetMap.

# install.packages("devtools")   # Uncomment if you do not have the 'devtools' package installed
devtools::install_github("koenderks/rcityviews")

library(rcityviews)

# Buscar un nombre de ciudad en la base de datos
list_cities(match = "Samaipata")

# Ver la ciudad
p <- cityview(name = "Samaipata", crop = "circle")

# Guardar la imagen resultante
ggplot2::ggsave(filename = "samaipata.png", plot = p, height = 500, width = 500, units = "mm", dpi = 100)
