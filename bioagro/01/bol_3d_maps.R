# Importar librerías
#install dev version of terra package
#install.packages('terra', repos='https://rspatial.r-universe.dev')

# libraries we need
libs <- c("rayshader", "tidyverse", "sf", 
          "classInt", "giscoR", "terra", "exactextractr")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# define longlat CRS
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

# Crear mapa hexagonal de Bolivia
# 1. OBTENER DATOS SF DE BOLIVIA
#---------
get_bolivia_sf <- function(bolivia, bolivia_hex, bolivia_sf) {
  
  bolivia <- giscoR::gisco_get_countries(
    year = "2016",
    epsg = "4326",
    resolution = "10",
    country = "Bolivia")  %>% 
    st_transform(3575)
  
  # Make grid of circa 30,000 m2
  bolivia_hex <- st_make_grid(bolivia, 
                             cellsize = (3 * sqrt(3) * 107^2)/2, 
                             what = "polygons", 
                             square = F) %>%
    st_intersection(bolivia) %>%
    st_sf() %>%
    mutate(id = row_number()) %>% filter(
      st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% 
    st_cast("MULTIPOLYGON") #transform to multipolygons
  
  bolivia_sf <- st_transform(bolivia_hex, crs = crsLONGLAT) #transform back to longlat
  
  return(bolivia_sf)
}


# Importar datos altura árboles
# 2. LOAD FOREST RASTER
#---------

get_forests <- function(url, forests) {
  
  url <- c("/vsicurl/https://glad.geog.umd.edu/Potapov/Forest_height_2019/Forest_height_2019_SAM.tif")
  forests <- rast(url)
  
  return(forests)
}

# Agregar datos ráster
# 3. AGGREGATE RASTER VALUES
#---------

get_bolivia_forest_agg <- function(bolivia_sf, forests, bolivia_forest, bolivia_ext) {
  
  forests <- get_forests()
  bolivia_sf <- get_bolivia_sf()
  
  bolivia_forest <-  forests
  bolivia_ext <- st_bbox(bolivia_sf) %>% as.vector()
  ext(bolivia_forest) <- bolivia_ext
  bo_forest <- terra::aggregate(bolivia_forest, fact=5)
  
  return(bo_forest)
}

# Estadísticas zonales
# 4. ZONAL STATISTICS
#---------

get_bolivia_zonal_stats <- function(bo_forest, bolivia_sf, bolivia_df, bo_df) {
  
  bo_forest <- get_bolivia_forest_agg()
  bolivia_sf <- get_bolivia_sf()
  
  bo_forest_clamp <- ifel(bo_forest > 100, 0, bo_forest)
  bolivia_df  <- exact_extract(bo_forest_clamp, bolivia_sf, "mean")
  bo_df <- as.data.frame(bolivia_df)
  bo_df$id <- 1:max(nrow(bo_df))
  
  return(bo_df)
}

# Unir altura promedio y objeto sf
# 5. MERGE NEW DF WITH HEX OBJECT
#---------

get_bolivia_final <- function(f, bolivia_sf, bo_df) {
  
  bo_df <- get_bolivia_zonal_stats()
  bolivia_sf <- get_bolivia_sf()
  
  f <- left_join(bo_df, bolivia_sf, by="id") %>% # join with transformed sf object
    st_as_sf() %>%
    filter(!st_is_empty(.)) #remove null features
  
  names(f)[1] <- "val"
  f$val[is.na(f$val)] <- 0
  
  return(f)
}

f <- get_bolivia_final()


# Cuadro de contorno
# 6. GENERATE BREAKS AND COLORS
#---------

get_breaks <- function(vmin, vmax, brk, breaks, all_breaks) {
  
  vmin <- min(f$val, na.rm=T)
  vmax <- max(f$val, na.rm=T)
  
  brk <- round(classIntervals(f$val, 
                              n = 6, 
                              style = 'fisher')$brks, 1) %>%
    head(-1) %>%
    tail(-1) %>%
    append(vmax)
  
  breaks <- c(vmin, brk)
  all_breaks <- list(vmin, vmax, breaks)
  return(all_breaks)
}

get_colors <- function(cols, newcol, ncols, cols2) {
  
  cols = rev(c("#1b3104", "#386c2e", 
               "#498c44", "#5bad5d", "#8dc97f", "#c4e4a7"))
  newcol <- colorRampPalette(cols)
  ncols <- 6
  cols2 <- newcol(ncols)
  
  return(cols2)
}


# Mapa 3D de altura de árboles de Bolivia
# 7. MAP
#---------

get_forest_map <- function(breaks, vmin, vmax, cols2, p) {
  
  vmin <- get_breaks()[[1]]
  vmax <- get_breaks()[[2]]
  breaks <- get_breaks()[[3]]
  cols2 <- get_colors()
  
  p <- ggplot(f) +
    geom_sf(aes(fill = val), color = NA, size=0) +
    scale_fill_gradientn(name="En metros",
                         colours=cols2,
                         breaks=breaks,
                         labels=round(breaks, 1),
                         limits=c(vmin, vmax))+   
    guides(fill=guide_legend(
      direction = "horizontal",
      keyheight = unit(2.5, units = "mm"),
      keywidth = unit(2.55, units = "mm"),
      title.position = 'top',
      title.hjust = .5,
      label.hjust = .5,
      nrow = 7,
      byrow = T,
      reverse = F,
      label.position = "left")) +
    coord_sf(crs = crsLONGLAT)+
    theme_minimal() +
    theme(text = element_text(family = "georg", color = "#22211d"),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = c(0.8, .1),
          legend.text = element_text(size=6, color="white"),
          legend.title = element_text(size=8, color="white"),
          panel.grid.major = element_line(color = "grey60", size = 0.2),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=14, color="#498c44", hjust=1, face="bold", vjust=-1),
          plot.caption = element_text(size=6, color="white", hjust=.15, vjust=20),
          plot.subtitle = element_text(size=12, color="#498c44", hjust=1),
          plot.margin = unit(c(t=0, r=0, b=0, l=0),"lines"),
          plot.background = element_rect(fill = "grey60", color = NA), 
          panel.background = element_rect(fill = "grey60", color = NA), 
          legend.background = element_rect(fill = "grey60", color = NA),
          panel.border = element_blank())+
    labs(x = "", 
         y = NULL, 
         title = "Altura promedio de cobertura de bosque en Bolivia", 
         subtitle = expression(paste("por 30.000", m^{2}, "de área de tierra")), 
         caption = "©2022 Eric Armijo (@rcrmj)\n Datos: https://glad.umd.edu/dataset")
  
  return(p)
}


p <- get_forest_map()

plot_gg(p,
        multicore = T,
        width=5,
        height=5,
        scale=150,
        shadow_intensity = .75,
        sunangle = 360,
        offset_edges=T,
        windowsize=c(1400,866),
        zoom = .4, 
        phi = 30, 
        theta = -30)

render_snapshot("bolivia_forest_height_2019.png", clear=T)
