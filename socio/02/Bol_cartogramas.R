## Ref: https://www.unigis.es/cartograma-r/
## ¿Cómo diseñar un cartograma con R?

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())

## Paso #1: La instalación y activación de los paquetes

# instalar los paquetes necesarios
#install.packages("sf")
#install.packages("cartogram")
#install.packages("readODS")
#install.package("dplyr")
#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("tmap")

# activar las librerías instaladas
library(cartogram)
library(sf)
library(readODS)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tmap)

## Paso #2: La lectura de los datos

# importar las geometrías y el fichero excel
bolivia <- st_read("/home/yoviajo/Documentos/lab/datvis/94/dat/bo_limite_departamental_ed.geojson")
poblacion <- read_ods("/home/yoviajo/Documentos/lab/datvis/94/dat/bolivia_dptos_pob2012.ods")

class(bolivia)
class(poblacion)
names(bolivia)
names(poblacion)
st_crs(bolivia)

plot(bolivia$geometry)


## Paso #3: La reproyección de un objeto de tipo sf

boliviaLmb <- st_transform(bolivia, "+proj=lcc +lat_1=-11.5 +lat_2=-21.5 +lat_0=-24 +lon_0=-64 +x_0=1000000 +y_0=0 +datum=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
plot(boliviaLmb$geometry, main = "Mapa de Bolivia (Lambert)")


## Paso #5: La vinculación de los datos recopilados

bolivia_poblacion <- left_join(boliviaLmb, poblacion, by=c('NOM_DPTO'='dpto_nombre'))
names(bolivia_poblacion)

## Paso #7: Ahora sí. Vamos a por el cartograma…

poblacion_cartograma <- cartogram_cont(bolivia_poblacion, "dpto_pob2012", itermax = 3)


## Paso #9: Diseñando el mapa definitivo

# a) El paquete tmap
tm_shape(poblacion_cartograma) +
  tm_style("gray") +
  tm_layout(main.title = "Población de Bolivia por Departamentos",
            main.title.size = 1,
            title = "INE (2012) Censo de población", title.size = 0.8)+
  tm_polygons(col = "dpto_pob2012", style = "cont",
              breaks= c(110436,746598,1382760,2018922,2655084), palette = "YlOrBr",
              title = "Población") +
  tm_scale_bar(position = c("left", "bottom"), width = 0.2) +
  tm_compass(position = c("0.001", "0.07"), size = 1.5)+
  tm_credits("E.Armijo, 2022")

# b) El paquete ggplot2
ggplot(data = poblacion_cartograma) +
  theme_set(theme_gray()) +
  theme(legend.position = "bottom") +
  xlab("Longitud") + ylab("Latitud") +
  scale_fill_distiller(name = "Población", palette = "Spectral",
                       limits = c(0,2655084), breaks = c(110436,746598,1382760,2018922,2655084)) +
  geom_sf(aes(fill = dpto_pob2012)) +
  ggtitle(label = "Población de Bolivia por Departamentos",
          subtitle = "INE (2012) Censo de población")


## Paso #10: ¿Qué mas puede ofrecer el paquete cartogram?

cartograma_dorling <- cartogram_dorling(poblacion_cartograma, "dpto_pob2012", itermax = 3)

ggplot(data = cartograma_dorling) +
  theme_set(theme_gray()) +
  theme(legend.position = "bottom") +
  xlab("Longitud") + ylab("Latitud") +
  scale_fill_distiller(name = "Población", palette = "Spectral",
                       limits = c(0,2655084), breaks = c(110436,746598,1382760,2018922,2655084)) +
  geom_sf(aes(fill = dpto_pob2012)) +
  ggtitle(label = "Población de Bolivia por Departamentos",
          subtitle = "INE (2012) Censo de población")


cartograma_ncont <- cartogram_ncont(poblacion_cartograma, weight = "dpto_pob2012", k = 6)

ggplot(data = cartograma_ncont) +
  theme_set(theme_gray()) +
  theme(legend.position = "bottom") +
  xlab("Longitud") + ylab("Latitud") +
  scale_fill_distiller(name = "Población", palette = "Spectral",
                       limits = c(0,2655084), breaks = c(110436,746598,1382760,2018922,2655084)) +
  geom_sf(aes(fill = dpto_pob2012)) +
  ggtitle(label = "Población de Bolivia por Departamentos",
          subtitle = "INE (2012) Censo de población")
