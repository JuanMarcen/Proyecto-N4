library(dplyr)
library(sf)
library(sp)
library(rnaturalearth)
library(ggplot2)
library(viridis)
# calcular background, limits
limits <- st_transform(
  as(
    SpatialPointsDataFrame(
      coords = data.frame(X = c(-10, 4), Y = c(35.5, 44)), 
      data = data.frame(X = c(-10, 4), Y = c(35.5, 44)),
      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")),
    'sf'
  ),
  2062
)

#background
world_map <- ne_countries(scale = "large", returnclass = 'sf')
european_union <- c("Algeria", "Andorra", "France", "Gibraltar", "Morocco", "Portugal", "Spain")
european_union_map <- 
  world_map %>% 
  filter(name %in% european_union)
background <- st_transform(european_union_map, 2062)

# mapa España con puntos
spain_points <- function(values, stations, title, title_legend){
  
  spain <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(admin == "Spain")
  
  coords <- data.frame(
    Longitude = stations$LON, Latitude = stations$LAT)
  
  data <- cbind(coords, values)
  
  data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
  
  p <- ggplot(data = background) +
    geom_sf(fill = "antiquewhite") +
    geom_sf(data = data_sf, aes(color = values), size = 2) +
    xlab("Longitud (º)") + 
    ylab("Latitud (º)") + 
    ggtitle(title) +
    coord_sf(xlim = st_coordinates(limits)[,1], ylim = st_coordinates(limits)[,2]) +
    theme(panel.background = element_rect(fill = "aliceblue"),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6, angle = 90),
          axis.title = element_text(size = 10, face = "bold"))  +
    scale_color_gradientn(
      colors = c("#2c0072", "#ff6ec7", "#ffe680", "#fdae61", "#f0f0f0"),
      values = scales::rescale(c(0, 0.05, 0.10, 0.20, 0.30)),
      name = "p-valor",
      limits = c(0, 0.30),
      breaks = seq(0, 0.30, 0.05)
    ) + theme(
      legend.position = "bottom",          # Mueve la leyenda abajo
      legend.key.width = unit(2, "cm"),    # Ajusta el ancho de los bloques
      legend.key.height = unit(0.4, "cm"), # Ajusta la altura
      legend.title = element_text(size = 10, face = "bold"),
      legend.text  = element_text(size = 8)
    ) +
    guides(color = guide_colorbar(
      title.position = "top",              # Título arriba de la barra
      title.hjust = 0.5,                   # Centra el título
      barwidth = 15,                       # Ancho total de la barra
      barheight = 0.8                      # Altura de la barra
    ))
    
  return(p)
}


