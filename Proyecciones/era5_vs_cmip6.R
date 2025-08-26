rm(list=ls())

# comparison of densities of era5 and cmip6 in the whole grid

#----Creation of dataframes----
library(lubridate)
#coordinates of grid
coord <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coord.rds")
colnames(coord) <- c('LAT', 'LON', 'station')

# ERA5 (already done)
g500_era5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/g500_jja.rds")
g500_era5[ , 2:ncol(g500_era5)] <- g500_era5[ , 2:ncol(g500_era5)] / 1000
g700_era5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/g700_jja.rds")
g700_era5[ , 2:ncol(g700_era5)] <- g700_era5[ , 2:ncol(g700_era5)] / 1000

# CMIP6 (closest to original grid of ERA5)
coord$Grid <- paste0(round(coord$LAT),"N.",abs(round(coord$LON)),ifelse(coord$LON>-0.5,"E","W"))
tpoints <-coord$Grid

g500_cmip6 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/Github/CMIP6_Iberian/Data/CMIP6/MPI-ESM1-2-HR_r1i1p1f1_zg/historical/Grid/iberian_MPI-ESM1-2-HR_r1i1p1f1_zg_500hPa_1960-2014.rds")
g700_cmip6 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/Github/CMIP6_Iberian/Data/CMIP6/MPI-ESM1-2-HR_r1i1p1f1_zg/historical/Grid/iberian_MPI-ESM1-2-HR_r1i1p1f1_zg_700hPa_1960-2014.rds")

plev_cool_g500 <- 500
plev_cool_g700 <- 700
cmip6_vars <- grep("zg",names(g500_cmip6))
cmip6_coor <- unlist(
  strsplit(x = names(g500_cmip6)[cmip6_vars],
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

coord_id <- coord$station
coord_names <- coord$station
outdf <- data.frame(id = coord_id,
                    name = coord_names,
                    ERA5 = paste0("zg",tpoints),
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
  outdf$near_g500[pp] <- names(g500_cmip6)[cmip6_vars[near_idx]]
  outdf$near_g700[pp] <- names(g700_cmip6)[cmip6_vars[near_idx]]
  
}

# cambio de unidades para que sea equivalente
g500_cmip6[, cmip6_vars] <- g500_cmip6[, cmip6_vars] * 9.80665 / 1000
g700_cmip6[, cmip6_vars] <- g700_cmip6[, cmip6_vars] * 9.80665 / 1000

# dataframe of values CMIP
g500_cmip6 <- g500_cmip6[, c('date', outdf$near_g500)]
g700_cmip6 <- g700_cmip6[, c('date', outdf$near_g700)]

#Reference period
fecha_ref <- c('1981-06-01', '2010-08-31')

g500_era5 <- g500_era5[which(g500_era5$Date >= fecha_ref[1] &
                             g500_era5$Date <= fecha_ref[2] & 
                             month(g500_era5$Date) != 5), ]
colnames(g500_era5)[-1] <- paste0('g500.', colnames(g500_era5)[-1])
g700_era5 <- g700_era5[which(g700_era5$Date >= fecha_ref[1] &
                             g700_era5$Date <= fecha_ref[2] & 
                             month(g700_era5$Date) != 5), ]
colnames(g700_era5)[-1] <- paste0('g700.', colnames(g700_era5)[-1])

g500_cmip6 <- g500_cmip6[which(g500_cmip6$date >= fecha_ref[1] &
                               g500_cmip6$date <= fecha_ref[2] & 
                               month(g500_cmip6$date) != 5), ]
g700_cmip6 <- g700_cmip6[which(g700_cmip6$date >= fecha_ref[1] &
                               g700_cmip6$date <= fecha_ref[2] & 
                               month(g700_cmip6$date) != 5), ]


#----ANOMALIES-----
# harmonics (in order to calculate the anomalies)
cs <- function(t,harmonics=1) {
  # if(min(t) <0 | max(t) > 1){ stop(" t must be in [0,1] range")}
  if(min(harmonics) <1){stop("harmonics > = 1")}
  ret <- numeric(0)
  for ( i in harmonics) {
    ret <- cbind( ret, cos(2*pi*i*t/365), sin(2*pi*i*t/365))
  }
  if (missing(harmonics)) cnames <- c('c','s')
  else {
    cnames <- paste( c("c","s"), rep(harmonics, each = 2),sep=".")
  }
  colnames(ret) <- cnames
  ret
}

l <- 152:243 # days
harm <- cs(l, 1) # same for all

l <- rep(l,times=30)
harm <- matrix(rep(harm, times = 30), ncol = ncol(harm), byrow = FALSE)
colnames(harm) <- c('c.1', 's.1')
t <- year(g500_era5$Date) - 1960 + 1

g500_era5 <- cbind(g500_era5, l, t, harm)
g700_era5 <- cbind(g700_era5, l, t, harm)
g500_cmip6 <- cbind(g500_cmip6, l, t, harm)
g700_cmip6 <- cbind(g700_cmip6, l, t, harm)

rm(list = setdiff(ls(), c('coord', 'g500_era5', 'g700_era5', 'g500_cmip6', 'g700_cmip6')))
anomalies <- function(df){
  for (i in 2:(ncol(df) - 4)){
    var <- names(df)[i]
    formula <- as.formula(paste(paste0('`', var, '`'), "~ s.1 + c.1"))
    mod <- lm(formula, data = df)
    
    df[, var] <- mod$residuals
  }
  
  return(df)
}

g500_era5 <- anomalies(g500_era5)
g700_era5 <- anomalies(g700_era5)
g500_cmip6 <- anomalies(g500_cmip6)
g700_cmip6 <- anomalies(g700_cmip6)


#----KS TEST----
ks_test_df <- function(df_era5, df_cmip6){
  ks_df <- data.frame(
    station = coord$Grid
  )
  
  for(i in 2:(ncol(df_era5)-4)){
    suppressWarnings(
      ks <- ks.test(df_era5[, i], df_cmip6[, i])
    )
    ks_df[i - 1, 'KS'] <- ks$statistic
    ks_df[i - 1, 'KSp'] <- ks$p.value
  }
  
  return(ks_df)
}

ks_df_g500 <- ks_test_df(g500_era5, g500_cmip6)
ks_df_g700 <- ks_test_df(g700_era5, g700_cmip6)

source('mapa_Spain.R')
ks_g500 <- spain_points(ks_df_g500$KSp, coord, 'KS Test g500 (crudo) 1981-2010', 'p-values')
ks_g700 <- spain_points(ks_df_g700$KSp, coord, 'KS Test g700 (crudo) 1981-2010', 'p-values')

## Estandarización y comparación
library(dplyr)
g500_era5_sc <- g500_era5 %>%
  mutate(across(2:(ncol(.)-4), scale))
g500_cmip6_sc <- g500_cmip6 %>%
  mutate(across(2:(ncol(.)-4), scale))
g700_era5_sc <- g700_era5 %>%
  mutate(across(2:(ncol(.)-4), scale))
g700_cmip6_sc <- g700_cmip6 %>%
  mutate(across(2:(ncol(.)-4), scale))

ks_df_g500_sc <- ks_test_df(g500_era5_sc, g500_cmip6_sc)
ks_df_g700_sc <- ks_test_df(g700_era5_sc, g700_cmip6_sc)

ks_g500_sc <- spain_points(ks_df_g500_sc$KSp, coord, 'KS Test g500 (est) 1981-2010', 'p-values')
ks_g700_sc <- spain_points(ks_df_g700_sc$KSp, coord, 'KS Test g700 (est) 1981-2010', 'p-values')

p_values_ref <- ggpubr::ggarrange(ks_g500, ks_g700, ks_g500_sc, ks_g700_sc, 
                                  nrow = 2, ncol = 2,
                                        common.legend = T, legend = 'bottom')
ggsave(
  filename = "Proyecciones/era5_vs_cmip6/p_values_ref.png", 
  plot = p_values_ref, 
  width = 8,   
  height = 5,     
  dpi = 300
)