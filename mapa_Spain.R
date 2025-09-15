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
      coords = data.frame(X = c(-10.5, 6), Y = c(34, 46)), 
      data = data.frame(X = c(-10.5, 6), Y = c(34, 46)),
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
spain_points <- function(values, stations, lim_value = c(0,1), jump = 0.1, threshold,
                         title, title_legend, dif = FALSE){
  
  spain <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(admin == "Spain")
  
  coords <- data.frame(Longitude = stations$LON, Latitude = stations$LAT)
  data <- cbind(coords, values)
  data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
  
  # Extraer puntos que superan el umbral
  highlight_df <- subset(data, values > threshold)
  highlight_sf <- st_as_sf(highlight_df, coords = c("Longitude", "Latitude"), crs = 4326)
  highlight_sf <- st_transform(highlight_sf, 2062)
  highlight_coords <- st_coordinates(highlight_sf)
  highlight_df_proj <- cbind(highlight_df, X = highlight_coords[,1], Y = highlight_coords[,2])
  
  # Capa base
  p <- ggplot(data = background) +
    geom_sf(fill = "antiquewhite") +
    geom_sf(data = data_sf, aes(color = values), size = 2) +
    geom_point(data = highlight_df_proj,
               aes(x = X, y = Y),
               shape = 21,
               color = "red",
               fill = NA,
               size = 3,
               stroke = 1.2) +
    xlab("Longitude (º)") + 
    ylab("Latitude (º)") + 
    ggtitle(title) +
    coord_sf(xlim = st_coordinates(limits)[,1], ylim = st_coordinates(limits)[,2]) +
    theme(panel.background = element_rect(fill = "aliceblue"),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6, angle = 90),
          axis.title = element_text(size = 10, face = "bold")) +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.4, "cm"),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text  = element_text(size = 8)
    ) +
    guides(color = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 15,
      barheight = 0.8
    ))
  
  # Elegir escala según dif
  if(dif){
    # escala divergente centrada en 0
    p <- p + scale_color_gradient2(low = "blue", mid = "white", high = "red",
                                   midpoint = 0,
                                   limits = c(lim_value[1], lim_value[2]),
                                   breaks = seq(lim_value[1], lim_value[2], jump),
                                   name = title_legend)
  } else {
    # escala continua como antes
    p <- p + scale_color_viridis_c(
      option = "plasma",
      name = title_legend,
      limits = c(lim_value[1], lim_value[2]),
      breaks = seq(lim_value[1], lim_value[2], jump)
    )
  }
  
  return(p)
}


