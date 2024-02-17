## Ref: https://www.unigis.es/cartograma-r/
## ¿Cómo diseñar un cartograma con R?


## Paso #1: La instalación y activación de los paquetes

# instalar los paquetes necesarios
install.packages("sf")
install.packages("cartogram")
install.packages("readxl")
install.package("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("tmap")

# activar las librerías instaladas
library(cartogram)
library(sf)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tmap)

## Paso #2: La lectura de los datos

# importar las geometrías y el fichero excel
mundo <- st_read("/home/yoviajo/Documentos/lab/datvis/94/dat/countries.geojson")
articulos <- read_excel("/home/yoviajo/Documentos/lab/datvis/94/dat/QGIS_ARQUEOLOGIA_ARTICULOS.xlsx")

class(mundo)
class(articulos)
names(mundo)
names(articulos)
st_crs(mundo)

plot(mundo$geometry)


## Paso #3: La reproyección de un objeto de tipo sf

mundo3857 <- st_transform(mundo, 3857)
plot(mundo3857$geometry, main = "Mapa del mundo (EPSG:3857")

## Paso #4: El filtrado de observaciones

mundo3857 <- mundo3857 %>%
  filter(!name %in% 'Antarctica')
plot(mundo3857$geometry)


## Paso #5: La vinculación de los datos recopilados

mundo_articulos <- left_join(mundo3857, articulos, by=c('name'='PAIS'))


## Paso #6: La gestión de los valores NA (missing values)

mundo_articulos_noNA <- mundo_articulos %>% replace_na(list(INVESTIGADORES = 0.1))
View(mundo_articulos_noNA)


## Paso #7: Ahora sí. Vamos a por el cartograma…

articulos_cartograma <- cartogram_cont(mundo_articulos_noNA, "INVESTIGADORES", itermax = 3)


## Paso #8: Desandando el camino de los missing values

dtemp<-replace(articulos_cartograma$INVESTIGADORES, articulos_cartograma$INVESTIGADORES == 0.1, 0)
mapa_final <- articulos_cartograma %>% mutate(INVESTIGADORES = dtemp)

## Paso #9: Diseñando el mapa definitivo

# a) El paquete tmap
tm_shape(mapa_final) +
  tm_style("gray") +
  tm_layout(main.title = "Artículos: Uso de QGIS en arqueología",
            main.title.size = 1,
            title = "Recuento basado en Google Scholar (2022)", title.size = 0.8)+
  tm_polygons(col = "INVESTIGADORES", style = "cont",
              breaks= c(1,5,10,20,40), palette = "YlOrBr",
              title = "Artículos según origen") +
  tm_scale_bar(position = c("left", "bottom"), width = 0.2) +
  tm_compass(position = c("0.001", "0.07"), size = 1.5)+
  tm_credits("UNIGIS Girona, 2022")

# b) El paquete ggplot2
ggplot(data = mapa_final) +
  theme_set(theme_gray()) +
  theme(legend.position = "bottom") +
  xlab("Longitud") + ylab("Latitud") +
  scale_fill_distiller(name = "Artículos según origen", palette = "Spectral",
                       limits = c(0,45), breaks = c(1,5,10,20,40)) +
  geom_sf(aes(fill = INVESTIGADORES)) +
  ggtitle(label = "Artículos: QGIS aplicados en arqueología",
          subtitle = "Según Google Scholar (2022)")


## Paso #10: ¿Qué mas puede ofrecer el paquete cartogram?

cartograma_dorling <- cartogram_dorling(mundo_articulos_noNA, "INVESTIGADORES", itermax = 3)

ggplot(data = cartograma_dorling) +
  theme_set(theme_gray()) +
  theme(legend.position = "bottom") +
  xlab("Longitud") + ylab("Latitud") +
  scale_fill_distiller(name = "Artículos según origen", palette = "Spectral",
                       limits = c(0,45), breaks = c(1,5,10,20,40)) +
  geom_sf(aes(fill = INVESTIGADORES)) +
  ggtitle(label = "Artículos: QGIS aplicados en arqueología",
          subtitle = "Según Google Scholar (2022)")


cartograma_ncont <- cartogram_ncont(mundo_articulos_noNA, weight = "INVESTIGADORES", k = 6)

ggplot(data = cartograma_ncont) +
  theme_set(theme_gray()) +
  theme(legend.position = "bottom") +
  xlab("Longitud") + ylab("Latitud") +
  scale_fill_distiller(name = "Artículos según origen", palette = "Spectral",
                       limits = c(0,45), breaks = c(1,5,10,20,40)) +
  geom_sf(aes(fill = INVESTIGADORES)) +
  ggtitle(label = "Artículos: QGIS aplicados en arqueología",
          subtitle = "Según Google Scholar (2022)")
