# carga de datos
rm(list=ls())

stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")
stations_dist <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations_dist.rds")

# obtener estaciones más cercanas del MPI al ERA5
# código de Elsa modifcado
stations$Grid <- paste0(round(stations$LAT),"N.",abs(round(stations$LON)),ifelse(stations$LON>-0.5,"E","W"))

#corners
cpoints <- c("45N.10W","45N.5E","35N.10W","35N.5E")
#Near station-points
tpoints <- c(stations$Grid, cpoints)

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
stations_id <- c(stations$STAID, cpoints)
stations_names <- c(stations$NAME2, cpoints)
outdf <- data.frame(id = stations_id,
                    name = stations_names,
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
  outdf$near_g500[pp] <- names(cmip6_g500)[cmip6_vars[near_idx]]
  outdf$near_g700[pp] <- names(cmip6_g700)[cmip6_vars[near_idx]]
  
}

# cambio de unidades para que sea equivalente
cmip6_g500[, cmip6_vars] <- cmip6_g500[, cmip6_vars] * 9.80665 / 1000
cmip6_g700[, cmip6_vars] <- cmip6_g700[, cmip6_vars] * 9.80665 / 1000

# dataframe of values CMIP
cmip6_g500 <- cmip6_g500[, c('date', outdf$near_g500)]
cmip6_g700 <- cmip6_g700[, c('date', outdf$near_g700)]

# dataframe en formato deseado para mis funciones ya desarrolladas
# primero no esquinas
# g500
n_estaciones <- ncol(cmip6_g500) - 5  

# Repetir las fechas para cada estación
fechas_ordenadas <- rep(cmip6_g500$date, times = n_estaciones)

# Extraer los valores por columna (aplanar por columnas)
valores_ordenados <- unlist(cmip6_g500[ , -c(1,(ncol(cmip6_g500)-3):ncol(cmip6_g500))])

# Crear data frame
X_g500 <- data.frame(
  date = fechas_ordenadas,
  g500 = valores_ordenados,
  g500_45_.10 = rep(cmip6_g500$zg500.45.4N10.3W, times = n_estaciones),
  g500_45_5 = rep(cmip6_g500$zg500.45.4N4.7E, times = n_estaciones),
  g500_35_.10 = rep(cmip6_g500$zg500.35.1N10.3W, times = n_estaciones),
  g500_35_5 = rep(cmip6_g500$zg500.35.1N4.7E, times = n_estaciones)
)


# g700
n_estaciones <- ncol(cmip6_g700) - 5  

# Repetir las fechas para cada estación
fechas_ordenadas <- rep(cmip6_g700$date, times = n_estaciones)

# Extraer los valores por columna (aplanar por columnas)
valores_ordenados <- unlist(cmip6_g700[ , -c(1,(ncol(cmip6_g700)-3):ncol(cmip6_g700))])

# Crear data frame
X_g700 <- data.frame(
  date = fechas_ordenadas,
  g700 = valores_ordenados,
  g700_45_.10 = rep(cmip6_g700$zg700.45.4N10.3W, times = n_estaciones),
  g700_45_5 = rep(cmip6_g700$zg700.45.4N4.7E, times = n_estaciones),
  g700_35_.10 = rep(cmip6_g700$zg700.35.1N10.3W, times = n_estaciones),
  g700_35_5 = rep(cmip6_g700$zg700.35.1N4.7E, times = n_estaciones)
)

X_proy <- cbind(X_g500, X_g700[, -1])

# juntar con los valores de Y para calcular armónicos 
Y <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/ProyectoN4/Y_2.rds")

fechas_cmip6 <- c(min(cmip6_g500$date), max(cmip6_g500$date))

Y <- Y[which(Y$Date >= fechas_cmip6[1] & Y$Date <= fechas_cmip6[2]),]

df_proy <- cbind(Y, X_proy[, -1])
names(df_proy)[3] <- 'Y'
df_proy$Y <- df_proy$Y/10

# retoques finales: añadir elevaciones, distancias, armónicos, t y l 
library(zoo)

# relleno nulos en la parte de Y
which(is.na(df_proy))
for (col_name in names(df_proy)[3:ncol(df_proy)]){
  df_proy[[col_name]] <- na.approx(df_proy[[col_name]],rule=2)
}
which(is.na(df_proy))

# elevación y distancia a costa
elev_sc <- scale(stations_dist$HGHT)
dist_sc <- scale(stations_dist$DIST)

# días l
l <- 151:243 # 31 mayo - 31 agosto
l <- rep(l, times = 40 * 55)

# año t
library(lubridate)
t <- year(Y$Date) - 1960 + 1

# juntado 
df_proy <- cbind(df_proy, l, t, rep(elev_sc,each=93*55), rep(dist_sc,each=93*55))
colnames(df_proy)[c(14, 15, 16, 17)]<-c('l', 't', 'elev', 'dist')

# armónicos
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

df_proy <- cbind(df_proy, cs(df_proy$l, 1))

# anomalías
# cálculo local de anomalías. Ref 1981-2010
for (i in 1:dim(stations)[1]){
  ind <- which(df_proy$station == stations$STAID[i])
  ind_jja <- which(df_proy$t[ind] >= 22 & df_proy$t[ind] <= 51 & df_proy$l[ind] >= 152)
  
  for (j in 4:13){
    var <- names(df_proy)[j]
    formula <- as.formula(paste(var, "~ s.1 + c.1"))
    mod <- lm(formula, data = df_proy[ind,], subset = ind_jja)
    preds <- predict(mod, newdata = data.frame(
      c.1 = df_proy$c.1[ind],
      s.1 = df_proy$s.1[ind]
    ))
    
    res <- df_proy[ind, var] - preds
    #print(sum(preds[ind_jja] - mod$fitted.values <= 1e-10))
    
    df_proy[ind,var] <- res 
  }
  
}

# cuadrado de anomalias
for (j in 4:13){
  var <- names(df_proy)[j]
  df_proy[,paste0('I(',var,'^2)')] <- df_proy[,var]^2
}

# lags
library(dplyr)
df_final_proy <- df_proy %>% 
  group_by(station,t) %>% 
  mutate(across(c(3:12),
                .fns = ~lag(.),
                .names = '{.col}_lag')) %>%
  as.data.frame() %>% na.omit()

# cuadrado lags
for (j in 30:39){
  var <- names(df_final_proy)[j]
  df_final_proy[,paste0('I(',var,'^2)')] <- df_final_proy[,var]^2
}


