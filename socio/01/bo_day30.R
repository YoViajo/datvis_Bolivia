#30DayChartChallenge | Day30 | 3D

## Densidad de población 2020 en Bolivia

## Adaptado de:
# Ref: https://github.com/andybridger/30DayChartChallenge/tree/main/day30

# Load/install libraries
install.packages("devtools")
devtools::install_github("tylermorganwall/rayshader")
devtools::install_github("dmurdoch/rgl")

library(tidyverse)
library(extrafont)
library(ggmap)
library(ggthemes)
library(viridis)
library(mapproj)
library(scales)
library(ggplot2)
library(rgl)
library(rayshader)

#data from worldpop.org
#read in the data and mutate (log) density data
pop_density <- read.csv("/home/yoviajo/Documentos/lab/datvis/95/dat/ppp_BOL_2020_1km_Aggregated.csv",
                        header = T) %>%
  mutate(Country = "Bolivia") %>%
  mutate(log10_density = log10(Z))

#plot
p <- ggplot(pop_density, aes(x = X, y = Y)) +
  geom_point(aes(colour = Z), 
             size = 0.1, alpha = 1,
             show.legend = TRUE) +
  scale_color_gradientn(colours = rev(rainbow(7)),
                                            breaks = c(0, 1, 2, 10, 100, 1000),
                                            trans = "log10") +
  coord_map() +
  labs(
       caption = "Fuente: worldpop.org") +
  theme_map() %+replace%
  theme(
    text = element_text(family = "Arial", colour = "#36413d"),
    plot.title = element_text(hjust = 0, size = 16, lineheight = 1.0, colour = "#36413d"),
    plot.subtitle = element_text(hjust = 0, size = 12),
    plot.caption = element_text(hjust = 1, size = 8, colour = "#36413d"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_blank())

p
# Export plot
ggsave(plot = p,'/home/yoviajo/Documentos/lab/datvis/95/salida/bol_denspob_Bolivia.png', 
       dpi = 400, height = 4, width = 6)

###########
#Generate 3D map with rayshader
###########
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

plot_gg(p, multicore = TRUE, width = 6 ,height=4, windowsize = c(800, 600),
        zoom = 0.55, phi = 50, theta = 45, sunangle = 105, soliddepth = -100,
        triangulate = TRUE, ground_material = rayrender::diffuse(color = "grey40"))
Sys.sleep(0.2)
render_snapshot(clear = TRUE)

#save as video
render_movie("/home/yoviajo/Documentos/lab/datvis/95/salida/bol_denspob_Bolivia.mp4")



