# carga de datos
rm(list=ls())

stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")
stations_dist <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations_dist.rds")

#----Dataframe CMIP6----
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

cmip6_g500_fut<- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/ProyectoN4/Github/Proyecto-N4/datos_Elsa/iberian_MPI-ESM1-2-HR_r1i1p1f1_zg_500hPa_2015-2064.rds")
cmip6_g700_fut <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/ProyectoN4/Github/Proyecto-N4/datos_Elsa/iberian_MPI-ESM1-2-HR_r1i1p1f1_zg_700hPa_2015-2064.rds")

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
df_cmip6 <- df_proy

# anomalías
# cálculo local de anomalías. Ref 1981-2010
# no estandarizadas
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

# estandarizadas segun los lm 
# df_era5 <- readRDS('df_era5.rds')
# df_era5 <- df_era5[which(df_era5$Date >= fechas_cmip6[1] & df_era5$Date <= fechas_cmip6[2]),]
# 
# for (i in 1:dim(stations)[1]){
#   ind <- which(df_proy$station == stations$STAID[i])
#   ind_jja <- which(df_proy$t[ind] >= 22 & df_proy$t[ind] <= 51 & df_proy$l[ind] >= 152)
#   
#   for (j in 4:13){
#     var <- names(df_proy)[j]
#     formula <- as.formula(paste(var, "~ s.1 + c.1"))
#     mod <- lm(formula, data = df_proy[ind,], subset = ind_jja)
#     preds <- predict(mod, newdata = data.frame(
#       c.1 = df_proy$c.1[ind],
#       s.1 = df_proy$s.1[ind]
#     ))
#     
#     res <- df_proy[ind, var] - preds
#     #print(sum(preds[ind_jja] - mod$fitted.values <= 1e-10))
#     
#     mod2<- lm(formula, data = df_era5[ind,], subset = ind_jja)
#     #guardado de estandarizado
#     df_proy[ind,var] <- res * summary(mod2)$sigma / summary(mod)$sigma
#   }
#   
# }


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


#----CMIP6 FUTURO----
#IGUAL QUE ANTES PERO CAMBIANDO LOS DATOS 
cmip6_g500_fut<- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/ProyectoN4/Github/Proyecto-N4/datos_Elsa/iberian_MPI-ESM1-2-HR_r1i1p1f1_zg_500hPa_2015-2064.rds")
cmip6_g700_fut <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/ProyectoN4/Github/Proyecto-N4/datos_Elsa/iberian_MPI-ESM1-2-HR_r1i1p1f1_zg_700hPa_2015-2064.rds")

# lo primero es hacer la relación de estaciones del MPI
# Extract GCM's grid coordinates

plev_cool_g500 <- 500
plev_cool_g700 <- 700
cmip6_vars <- grep("zg",names(cmip6_g500_fut))
cmip6_coor <- unlist(
  strsplit(x = names(cmip6_g500_fut)[cmip6_vars],
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
  outdf$near_g500[pp] <- names(cmip6_g500_fut)[cmip6_vars[near_idx]]
  outdf$near_g700[pp] <- names(cmip6_g700_fut)[cmip6_vars[near_idx]]
  
}

# cambio de unidades para que sea equivalente
cmip6_g500_fut[, cmip6_vars] <- cmip6_g500_fut[, cmip6_vars] * 9.80665 / 1000
cmip6_g700_fut[, cmip6_vars] <- cmip6_g700_fut[, cmip6_vars] * 9.80665 / 1000

# dataframe of values CMIP
cmip6_g500_fut <- cmip6_g500_fut[, c('date', outdf$near_g500)]
cmip6_g700_fut <- cmip6_g700_fut[, c('date', outdf$near_g700)]

# dataframe en formato deseado para mis funciones ya desarrolladas
# primero no esquinas
# g500
n_estaciones <- ncol(cmip6_g500_fut) - 5  

# Repetir las fechas para cada estación
fechas_ordenadas <- rep(cmip6_g500_fut$date, times = n_estaciones)

# Extraer los valores por columna (aplanar por columnas)
valores_ordenados <- unlist(cmip6_g500_fut[ , -c(1,(ncol(cmip6_g500_fut)-3):ncol(cmip6_g500_fut))])

# Crear data frame
X_g500_fut <- data.frame(
  date = fechas_ordenadas,
  g500 = valores_ordenados,
  g500_45_.10 = rep(cmip6_g500_fut$zg500.45.4N10.3W, times = n_estaciones),
  g500_45_5 = rep(cmip6_g500_fut$zg500.45.4N4.7E, times = n_estaciones),
  g500_35_.10 = rep(cmip6_g500_fut$zg500.35.1N10.3W, times = n_estaciones),
  g500_35_5 = rep(cmip6_g500_fut$zg500.35.1N4.7E, times = n_estaciones)
)


# g700
n_estaciones <- ncol(cmip6_g700_fut) - 5  

# Repetir las fechas para cada estación
fechas_ordenadas <- rep(cmip6_g700_fut$date, times = n_estaciones)

# Extraer los valores por columna (aplanar por columnas)
valores_ordenados <- unlist(cmip6_g700_fut[ , -c(1,(ncol(cmip6_g700_fut)-3):ncol(cmip6_g700_fut))])

# Crear data frame
X_g700_fut <- data.frame(
  date = fechas_ordenadas,
  g700 = valores_ordenados,
  g700_45_.10 = rep(cmip6_g700_fut$zg700.45.4N10.3W, times = n_estaciones),
  g700_45_5 = rep(cmip6_g700_fut$zg700.45.4N4.7E, times = n_estaciones),
  g700_35_.10 = rep(cmip6_g700_fut$zg700.35.1N10.3W, times = n_estaciones),
  g700_35_5 = rep(cmip6_g700_fut$zg700.35.1N4.7E, times = n_estaciones)
)

X_proy_fut <- cbind(rep(stations$STAID, each = 50* 92),X_g500_fut, X_g700_fut[, -1])
df_proy_fut <- X_proy_fut
colnames(df_proy_fut)[1] <- 'station'
# retoques finales: añadir elevaciones, distancias, armónicos, t y l 
library(zoo)

which(is.na(df_proy_fut))

# elevación y distancia a costa
elev_sc <- scale(stations_dist$HGHT)
dist_sc <- scale(stations_dist$DIST)

# días l
l <- 152:243 # 1 junio - 31 agosto
l <- rep(l, times = 40 * 50)

# año t
library(lubridate)
t <- year(X_proy_fut$date) - 2015 + 1

# juntado 
df_proy_fut <- cbind(df_proy_fut, l, t, rep(elev_sc,each=92*50), rep(dist_sc,each=92*50))
colnames(df_proy_fut)[c(13, 14, 15, 16)]<-c('l', 't', 'elev', 'dist')

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

df_proy_fut <- cbind(df_proy_fut, cs(df_proy_fut$l, 1))
df_cmip6_fut <- df_proy_fut

# anomalías
# cálculo local de anomalías. Ref 1981-2010
# no estandarizadas
for (i in 1:dim(stations)[1]){
  ind <- which(df_proy$station == stations$STAID[i])
  ind_fut <- which(df_proy_fut$station == stations$STAID[i])
  ind_jja <- which(df_proy$t[ind] >= 22 & df_proy$t[ind] <= 51 & df_proy$l[ind] >= 152)
  
  for (j in 4:13){
    var <- names(df_proy)[j]
    formula <- as.formula(paste(var, "~ s.1 + c.1"))
    mod <- lm(formula, data = df_proy[ind,], subset = ind_jja)
    preds <- predict(mod, newdata = data.frame(
      c.1 = df_proy_fut$c.1[ind_fut], #prediccion anomalias segun el periodo de referencia
      s.1 = df_proy_fut$s.1[ind_fut]
    ))
    
    res <- df_proy_fut[ind_fut, var] - preds
    #print(sum(preds[ind_jja] - mod$fitted.values <= 1e-10))
    
    df_proy_fut[ind_fut, var] <- res 
  }
  
}

# estandarizadas segun los lm 
# df_era5 <- readRDS('df_era5.rds')
# df_era5 <- df_era5[which(df_era5$Date >= fechas_cmip6[1] & df_era5$Date <= fechas_cmip6[2]),]
# 
# for (i in 1:dim(stations)[1]){
#   ind <- which(df_proy$station == stations$STAID[i])
#   ind_jja <- which(df_proy$t[ind] >= 22 & df_proy$t[ind] <= 51 & df_proy$l[ind] >= 152)
#   
#   for (j in 4:13){
#     var <- names(df_proy)[j]
#     formula <- as.formula(paste(var, "~ s.1 + c.1"))
#     mod <- lm(formula, data = df_proy[ind,], subset = ind_jja)
#     preds <- predict(mod, newdata = data.frame(
#       c.1 = df_proy$c.1[ind],
#       s.1 = df_proy$s.1[ind]
#     ))
#     
#     res <- df_proy[ind, var] - preds
#     #print(sum(preds[ind_jja] - mod$fitted.values <= 1e-10))
#     
#     mod2<- lm(formula, data = df_era5[ind,], subset = ind_jja)
#     #guardado de estandarizado
#     df_proy[ind,var] <- res * summary(mod2)$sigma / summary(mod)$sigma
#   }
#   
# }


# cuadrado de anomalias
for (j in 3:12){
  var <- names(df_proy_fut)[j]
  df_proy_fut[,paste0('I(',var,'^2)')] <- df_proy_fut[,var]^2
}

# lags
library(dplyr)
df_final_proy_fut <- df_proy_fut %>% 
  group_by(station,t) %>% 
  mutate(across(c(2:11),
                .fns = ~lag(.),
                .names = '{.col}_lag')) %>%
  as.data.frame() %>% na.omit()

# cuadrado lags
for (j in 29:38){
  var <- names(df_final_proy_fut)[j]
  df_final_proy_fut[,paste0('I(',var,'^2)')] <- df_final_proy_fut[,var]^2
}


#----Dataframes finales----
# guardado de solo las variables que se ajustan en el modelo
escalado_info <- readRDS("data_q0.95/escalado_info.rds")
df_era5 <- readRDS('data_q0.95/df_era5.rds')
df_era5 <- df_era5[which(df_era5$Date >= fechas_cmip6[1] & df_era5$Date <= fechas_cmip6[2]),]
vars <- readRDS('data_q0.95/vars.rds')
load('data_q0.90/data.RData')
load('data_q0.75/data.RData')

rm(list = setdiff(ls(), c('stations',
                        'stations_dist',
                        'df_final_proy',
                        'df_final_proy_fut',
                        'outdf',
                        'Y',
                        'vars',
                        'vars_q0.90',
                        'vars_q0.75',
                        'fechas_cmip6',
                        'df_cmip6',
                        'df_cmip6_fut',
                        'df_era5',
                        'escalado_info',
                        'escalado_info_q0.90',
                        'escalado_info_q0.75')))

# Creación de fórmula. Si aparece al cuadrado y normal como polinomio ortogonal
generar_formula_poly <- function(vars) {
  # variables cuadráticas
  quad_vars <- gsub("^I\\((.*)\\^2\\)$", "\\1", grep("^I\\(.*\\^2\\)$", vars, value = TRUE))
  
  # variables lineales
  lin_vars <- setdiff(vars, grep("^I\\(.*\\^2\\)$", vars, value = TRUE))
  
  # Lista de variables finales para la fórmula
  vars_finales <- c()
  
  for (var in lin_vars) {
    if (var %in% quad_vars) {
      # poly() si hay término lineal y cudrático
      vars_finales <- c(vars_finales, paste0("poly(", var, ", 2)"))
    } else {
      vars_finales <- c(vars_finales, var)
    }
  }
  
  # cuadráticas que sin parte lineal
  quad_solo <- setdiff(quad_vars, lin_vars)
  for (var in quad_solo) {
    vars_finales <- c(vars_finales, paste0("I(", var, "^2)"))
  }
  
  # fórmula final
  return(vars_finales)
}

vars_finales <- generar_formula_poly(vars)
vars_finales_q0.90 <- generar_formula_poly(vars_q0.90)
vars_finales_q0.75 <- generar_formula_poly(vars_q0.75)
# esta fórmula permite observar cuales tienen valor poly
# (aunque luego no se usa, porque se mantienen los nombres originales)
colnames(df_final_proy_fut)[2] <- 'Date'
v_proy <- function(vars_finales, vars, df, escalado_info){
  # Crear una lista para ir almacenando las columnas
  ## AQUI ME HE QUEDADO
  columnas <- list()
  
  # ESCALAR SEGÚN EL ESCALADO UTILIZADO EN EL MODELO AJUSTADO
  # escalado_info <- readRDS("data_q0.95/escalado_info.rds")
  # df_era5 <- readRDS('data_q0.95/df_era5.rds')
  # df_era5 <- df_era5[which(df_era5$Date >= fechas_cmip6[1] & df_era5$Date <= fechas_cmip6[2]),]
  
  for (var in vars_finales) {
    cat('Variable: ', var, '\n')
    if (grepl("^poly\\(", var)) {
      # Extraer el nombre real de la variable dentro del poly()
      nombre_var <- sub("^poly\\(([^,]+),.*", "\\1", var)
      
      # Calcular los términos polinómicos SIN escalar
      poly_cols <- poly(df[[nombre_var]], degree = 2)
      
      # Recuperar los parámetros de escalado guardados
      info <- escalado_info[[nombre_var]]
      # info$center y info$scale son vectores con 2 elementos (uno por columna del poly)
      
      # Escalar con la misma media y sd que en el dataset original
      poly_scaled <- sweep(poly_cols, 2, info$center, "-")
      poly_scaled <- sweep(poly_scaled, 2, info$scale, "/")
      
      # Asignar nombres
      colnames(poly_scaled) <- c(nombre_var, paste0("I(", nombre_var, "^2)"))
      
      # Añadir al listado
      columnas[[length(columnas) + 1]] <- as.data.frame(poly_scaled)
      
    } else {
      # Recuperar info de escalado para la variable
      info <- escalado_info[[var]]
      
      # Escalar manualmente
      var_scaled <- (df[[var]] - info$center) / info$scale
      
      columnas[[length(columnas) + 1]] <- data.frame(var_scaled)
      colnames(columnas[[length(columnas)]]) <- var
    }
  }
  
  
  # Combinar todas las columnas en un único dataframe
  v <- do.call(cbind, columnas)
  v <- v[, vars]
  v$elev <- as.numeric(df$elev)
  v$dist <- as.numeric(df$dist)
  v$Date <- df$Date
  v$station <- as.integer(df$station)
  v$Y <- df$Y
  v <- v[, c((ncol(v)-2):ncol(v), 1:(ncol(v)-3))]
  
  return(v)
}

v_q0.95_proy <- v_proy(vars_finales, vars, df_final_proy, escalado_info)
v_q0.90_proy <- v_proy(vars_finales_q0.90, vars_q0.90, df_final_proy, escalado_info_q0.90)
v_q0.75_proy <- v_proy(vars_finales_q0.75, vars_q0.75, df_final_proy, escalado_info_q0.75)

v_q0.95_proy_fut <- v_proy(vars_finales, vars, df_final_proy_fut, escalado_info)
v_q0.90_proy_fut <- v_proy(vars_finales_q0.90, vars_q0.90, df_final_proy_fut, escalado_info_q0.90)
v_q0.75_proy_fut <- v_proy(vars_finales_q0.75, vars_q0.75, df_final_proy_fut, escalado_info_q0.75)

# extra escalado (Jorge) #faltan para las futuro
colnames(df_cmip6_fut)[2] <- 'Date'
v_proy_est <- function(vars, v_proy, df_cmip6, df.mod){
  
  sigmas <- data.frame(
    matrix(NA, nrow = dim(df_cmip6)[1], ncol = 2 * length(vars))
  )
  colnames(sigmas) <- c(paste0('sd_era5_', vars), paste0('sd_cmip6_', vars))
  sigmas$Date <- df_cmip6$Date
  
  for(var in vars){
    cat('Variable: ', var, '\n')
    es_cuadrado <- grepl("^I\\(.*\\^2\\)$", var)
    var_clean <- gsub('_lag', '', var)
    var_clean <- gsub("^I\\((.*)\\^2\\)$", "\\1", var_clean)
    cat('Transformada a ', var_clean, '\n')
    
    if (var == 's.1' || var == 'c.1'){
      cat('Seno o coseno\n')
      sigmas[, paste0('sd_cmip6_', var)] <- 1
      sigmas[, paste0('sd_era5_', var)] <- 1
      next
    }
    
    for (i in 1:dim(stations)[1]){
      ind <- which(df.mod$station == stations$STAID[i])
      ind_fut <- which(df_cmip6$station == stations$STAID[i])
      ind_jja <- which(df.mod$t[ind] >= 22 & df.mod$t[ind] <= 51 & df.mod$l[ind] >= 152)
  
      formula <- as.formula(paste(var_clean, "~ s.1 + c.1"))
      mod <- lm(formula, data = df.mod[ind,], subset = ind_jja)
      mod2<- lm(formula, data = df.mod[ind,], subset = ind_jja)
  
      if (es_cuadrado == FALSE){
        sigmas[ind_fut, paste0('sd_cmip6_', var)] <- summary(mod)$sigma
        sigmas[ind_fut, paste0('sd_era5_', var)] <- summary(mod2)$sigma
      }else{
        sigmas[ind_fut, paste0('sd_cmip6_', var)] <- (summary(mod)$sigma)^2
        sigmas[ind_fut, paste0('sd_era5_', var)] <- (summary(mod2)$sigma)^2
      }
      
    }
  }
  
  sigmas <- sigmas[which(format(sigmas$Date, '%m-%d') != '06-01'), ]
  
  v <- v_proy
  for (var in vars){
    v[, var] <- v[, var] * sigmas[, paste0('sd_era5_', var)] / sigmas[, paste0('sd_cmip6_', var)]
  }

  return(v)
}

v_q0.95_proy_est <- v_proy_est(vars, v_q0.95_proy, df_cmip6, df_cmip6)
v_q0.90_proy_est <- v_proy_est(vars_q0.90, v_q0.90_proy, df_cmip6, df_cmip6)
v_q0.75_proy_est <- v_proy_est(vars_q0.75, v_q0.75_proy, df_cmip6, df_cmip6)

v_q0.95_proy_est_fut <- v_proy_est(vars, v_q0.95_proy_fut, df_cmip6_fut, df_cmip6)

save(stations,
     stations_dist,
     df_final_proy,
     df_final_proy_fut,
     outdf,
     v_q0.95_proy,
     v_q0.95_proy_fut,
     v_q0.95_proy_est,
     Y,
     vars,
     file = 'data_q0.95/proyecciones.RData')

save(stations,
     stations_dist,
     df_final_proy,
     df_final_proy_fut,
     outdf,
     v_q0.90_proy,
     v_q0.90_proy_fut,
     v_q0.90_proy_est,
     Y,
     vars_q0.90,
     file = 'data_q0.90/proyecciones.RData')

save(stations,
     stations_dist,
     df_final_proy,
     df_final_proy_fut,
     outdf,
     v_q0.75_proy,
     v_q0.75_proy_fut,
     v_q0.75_proy_est,
     Y,
     vars_q0.75,
     file = 'data_q0.75/proyecciones.RData')
