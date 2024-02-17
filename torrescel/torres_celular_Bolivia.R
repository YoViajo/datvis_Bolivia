## MAPA DE DISTRIBUCIÓN DE TORRES DE CELULAR EN BOLIVIA
## Basado en un script por Rafael Lopes (@rafalpx): https://github.com/rafalopespx/cell_towers_br


library(dplyr)

library(readr)
library(ggplot2)
library(ggtext)
library(sf)


library(rnaturalearth)
library(rnaturalearthdata)

# Filtrado del conjunto de datos Torres de Celular

## Definición de SRC para el conjunto de datos de torres de celular para Bolivia (opencellid.org)
celltowers <- read_csv("736.csv.gz")|> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)


## Obtener la extensión de Bolivia
bolivia <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf') |>  filter(name == 'Bolivia')

## Filtrado de torres por tipo
### 3G
UMTS <- celltowers |> 
  filter(radio == "UMTS") |>
  st_intersection(bolivia)

### 4G
LTE <- celltowers |> 
  filter(radio == "LTE") |>
  st_intersection(bolivia)

### Gráficos

## Representación rápida de la extensión
ggplot() + 
  geom_sf(data = bolivia)

## Gráfico de las torres de celular en el mapa
ggplot() + 
  geom_sf(data = bolivia, fill = "black", color = "white", size = 0.3) + 
  geom_sf(data = UMTS, shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE, shape = ".", color = "#cc0000", alpha = 0.5)

## Guardar gráfico
ggsave("my_plot_bo.png", height = 7, width = 7) 
