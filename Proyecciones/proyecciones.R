# carga de datos
rm(list=ls())

stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")
rel_stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/rel_stations.rds")

# obtener estaciones más cercanas del MPI al ERA5
# código de Elsa modifcado
stations$Grid <- paste0(round(stations$LAT),"N.",abs(round(stations$LON)),ifelse(stations$LON>-0.5,"E","W"))

#corners
cpoints <- c("45N.10W","45N.5E","35N.10W","35N.5E")
#Near station-points
tpoints <- c(cpoints,unique(stations$Grid))

# Reference period (para más adelante, cuando haga las anomalías)
ref_period <- c("1981-06-01","2010-08-31")

# Read data (ERA5 already done)
# CMIP 6: MPI
cmip6_g500 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/Github/CMIP6_Iberian/Data/CMIP6/MPI-ESM1-2-HR_r1i1p1f1_zg/historical/Grid/iberian_MPI-ESM1-2-HR_r1i1p1f1_zg_500hPa_1960-2014.rds")
cmip6_g700 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/Github/CMIP6_Iberian/Data/CMIP6/MPI-ESM1-2-HR_r1i1p1f1_zg/historical/Grid/iberian_MPI-ESM1-2-HR_r1i1p1f1_zg_700hPa_1960-2014.rds")

# lo primero es hacer la relación de estaciones del MPI
# Extract GCM's grid coordinates
plev_cool_g500 <- 500
plev_cool_g700 <- 700
cmip6_vars <- grep("zg",names(cmip6_g500))
cmip6_coor <- unlist(
  strsplit(x = names(cmip6_g500)[cmip6_vars],
           split = paste0("zg",plev_cool_g500,"[.]")))[seq(2,2*length(cmip6_vars),2)]
cmip6_coor <- data.frame(lab = cmip6_coor,
                         lat = as.numeric(NA),
                         lon = as.numeric(NA))
for(ii in 1:nrow(cmip6_coor)){
  aux <- unlist(strsplit(cmip6_coor$lab[ii],"N"))
  cmip6_coor$lat[ii] <- as.numeric(aux[1])
  lon_sign <- substr(aux[2],nchar(aux[2]),nchar(aux[2]))
  cmip6_coor$lon[ii] <- as.numeric(substr(aux[2],1,nchar(aux[2])-1))
  if(substr(aux[2],nchar(aux[2]),nchar(aux[2])) == "W"){
    cmip6_coor$lon[ii] <- cmip6_coor$lon[ii] * (-1)
  }
}

# Create output dataframe
outdf <- data.frame(ERA5 = paste0("zg",tpoints),
                    GCM = 'MPI',
                    lat = as.numeric(NA),
                    lon = as.numeric(NA),
                    near_g500 = rep("", length(tpoints)),
                    near_g700 = rep("", length(tpoints)))

# Compute scores
for(pp in 1:length(tpoints)){
  
  # Get ERA5 coordinates
  aux <- unlist(strsplit(tpoints[pp],"[.]"))
  outdf$lat[pp] <- as.numeric(as.numeric(substr(aux[1],1,nchar(aux[1])-1)))
  outdf$lon[pp] <- as.numeric(substr(aux[2],1,nchar(aux[2])-1))
  if(substr(aux[2],nchar(aux[2]),nchar(aux[2])) == "W"){
    outdf$lon[pp] <- outdf$lon[pp] * (-1)
  }
  
  # Find nearest grid point in GMC
  eudist <- sqrt((cmip6_coor$lat-outdf$lat[pp])**2 + (cmip6_coor$lon-outdf$lon[pp])**2)
  near_idx <- which.min(eudist)[1]
  outdf$near_g500[pp] <- names(cmip6_g500)[cmip6_vars[near_idx]]
  outdf$near_g700[pp] <- names(cmip6_g700)[cmip6_vars[near_idx]]
  
}

