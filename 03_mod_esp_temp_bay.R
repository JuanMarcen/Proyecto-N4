# TODO YA GENERALIZADO MENOS EL TEMA GRÁFICAS

rm(list = setdiff(ls(), c('df_final',
                          'stations',
                          'stations_dist',
                          'v_q0.95',
                          'formula',
                          'vars',
                          'modelos_finales_q0.95')))

load('data_q0.95/data.RData')
load('data_q0.90/data.RData')
load('data_q0.75/data.RData')

library(spTReg)
library(quantreg)
library(sf)
library(sp)
library(coda)

#---Preparativos----
# coordenadas en km
stations <- st_transform(
  as(
    SpatialPointsDataFrame(
      coords = stations[c("LON", "LAT")], 
      data = stations[c("STAID", "STANAME", "LON", "LAT", "HGHT","color",'NAME1','NAME2')],
      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")),
    'sf'
  ),
  2062
)

coords_km <- st_coordinates(stations) / 1000

# fórmula, añadir elevación y distancia
formula <- update(formula, .~. + elev + dist)
formula_q0.90 <- update(formula_q0.90, .~. + elev + dist)
formula_q0.75 <- update(formula_q0.75, .~. + elev + dist)

#----Starting points----
#obtencion de parametros de inicio
starting_values <- function(modelos_finales){
  start_beta <- apply(
    modelos_finales[,3:(ncol(modelos_finales) - 1)],
    MARGIN = 2, FUN = mean)
  
  elev_sc <- scale(stations_dist$HGHT)
  dist_sc <- scale(stations_dist$DIST)
  mod <- lm(modelos_finales$intercept ~ elev_sc + dist_sc)
  
  elev_inic <- coef(mod)[2]
  dist_inic <- coef(mod)[3]
  
  inic_procesos <- as.matrix(
    cbind(
      mod$residuals,
      sweep(modelos_finales[,4:(ncol(modelos_finales) - 1)],
            2, start_beta[-1])))
  inic_procesos <- inic_procesos + rnorm(40 * (ncol(modelos_finales)-3), 0, 0.1)
  
  start_beta <- c(start_beta, elev_inic, dist_inic)
  
  return(list(
    start_beta = start_beta,
    inic_procesos = inic_procesos
  ))
}

start_beta_q0.95 <- starting_values(modelos_finales_q0.95)[['start_beta']]
inic_procesos_q0.95 <- starting_values(modelos_finales_q0.95)[['inic_procesos']]

start_beta_q0.90 <- starting_values(modelos_finales_q0.90)[['start_beta']]
inic_procesos_q0.90 <- starting_values(modelos_finales_q0.90)[['inic_procesos']]

start_beta_q0.75 <- starting_values(modelos_finales_q0.75)[['start_beta']]
inic_procesos_q0.75 <- starting_values(modelos_finales_q0.75)[['inic_procesos']]

# start_beta_q0.95 <- apply(
#   modelos_finales_q0.95[,3:(ncol(modelos_finales_q0.95) - 1)],
#   MARGIN = 2, FUN = mean)
# 
# elev_sc <- scale(stations_dist$HGHT)
# dist_sc <- scale(stations_dist$DIST)
# mod_q0.95 <- lm(modelos_finales_q0.95$intercept ~ elev_sc + dist_sc)
# 
# elev_inic_q0.95 <- coef(mod_q0.95)[2]
# dist_inic_q0.95 <- coef(mod_q0.95)[3]
# 
# inic_procesos_q0.95 <- as.matrix(
#   cbind(
#     mod_q0.95$residuals,
#     sweep(modelos_finales_q0.95[,4:(ncol(modelos_finales_q0.95) - 1)],
#           2, start_beta_q0.95[-1])))
# inic_procesos_q0.95 <- inic_procesos_q0.95 + rnorm(40*18, 0, 0.1)


#----Modelos----
mod_bay <- function(formula, data, tau, vars, 
                    coords, start_beta, inic_procesos,
                    n.samples, n.burnin, n.thin, n.report){
  
  ind <- length(vars) + 3
  
  mod<-spTm(formula,
            data = data,
            method = 'q',
            quantile = tau,
            coords = coords,
            v = as.matrix(cbind(1,data[4:ind])),
            priors = list(
              "beta" = list(M = rep(0, ind), P = 0.0001 * 
                              diag(ind)),
              "sigma" = c(0.1, 0.1),
              "phi" = c(38, 7400),
              "mu" = c(0, 0.0001)),
            starting = list(
              "beta" = start_beta,
              "sigma" = 1,
              "alpha" = inic_procesos,
              "hp" = c("mu" = 0, "sigma" = 1, "phi" = 3 / 600)),
            n.samples = n.samples,
            n.burnin = n.burnin,
            n.thin = n.thin,
            n.report = n.report
  )
  
  return(mod)
}

mod_q0.95_bay <- mod_bay(
  formula = formula, 
  data = v_q0.95, 
  tau = 0.95, 
  vars = vars, 
  coords = coords_km, 
  start_beta = start_beta_q0.95, 
  inic_procesos = inic_procesos_q0.95, 
  n.samples = 100000, 
  n.burnin = 100000, 
  n.thin = 1000, 
  n.report = 1000)

mod_q0.90_bay <- mod_bay(
  formula = formula_q0.90, 
  data = v_q0.90, 
  tau = 0.90, 
  vars = vars_q0.90, 
  coords = coords_km, 
  start_beta = start_beta_q0.90, 
  inic_procesos = inic_procesos_q0.90, 
  n.samples = 100000, 
  n.burnin = 100000, 
  n.thin = 1000, 
  n.report = 1000)

mod_q0.75_bay <- mod_bay(
  formula = formula_q0.75, 
  data = v_q0.75, 
  tau = 0.95, 
  vars = vars_q0.75, 
  coords = coords_km, 
  start_beta = start_beta_q0.75, 
  inic_procesos = inic_procesos_q0.75, 
  n.samples = 100000, 
  n.burnin = 100000, 
  n.thin = 1000, 
  n.report = 1000)


# mod_q0.95_bay<-spTm(formula,
#                        data = v_q0.95,
#                        method = 'q',
#                        quantile = 0.95,
#                        coords = coords_km,
#                        v = as.matrix(cbind(1,v_q0.95[4:20])),
#                        priors = list(
#                          "beta" = list(M = rep(0, 20), P = 0.0001 * 
#                                          diag(20)),
#                          "sigma" = c(0.1, 0.1),
#                          "phi" = c(38, 7400),
#                          "mu" = c(0, 0.0001)),
#                        starting = list(
#                          "beta" = start_beta_q0.95,
#                          "sigma" = 1,
#                          "alpha" = inic_procesos_q0.95,
#                          "hp" = c("mu" = 0, "sigma" = 1, "phi" = 3 / 600)),
#                        n.samples = 100000,
#                        n.burnin = 100000,
#                        n.thin = 1000,
#                        n.report = 1000
# )


# save(Y, df_final, stations, stations_dist, 
#      modelos_proyecto_q0.95, modelos_proyecto_q0.95_AIC, 
#      vars, v_q0.95, 
#      modelos_finales_q0.95,
#      formula, mod_q0.95_bay, elev_sc, dist_sc,
#      file = 'data.RData')

save(Y, df_final, stations, stations_dist,
     modelos_proyecto_q0.90,
     vars_q0.90, v_q0.90,
     modelos_finales_q0.90,
     formula_q0.90, mod_q0.90_bay, elev_sc, dist_sc,
     file = 'data_q0.95/data.RData')

save(Y, df_final, stations, stations_dist,
     modelos_proyecto_q0.75, 
     vars_q0.75, v_q0.75,
     modelos_finales_q0.75,
     formula_q0.75, mod_q0.75_bay, elev_sc, dist_sc,
     file = 'data_q0.75/data.RData')

# nombres de columna a poly
# esto es util en el caso de realizar proyecciones en un futuro (IGNORAR DE MOMENTO)
quad_vars <- gsub("^I\\((.*)\\^2\\)$", "\\1", 
                  grep("^I\\(.*\\^2\\)$", vars, value = TRUE))

new_vars <- vars
for (i in 1:length(new_vars)){
  if(new_vars[i] %in% quad_vars){
    new_vars[i] <- paste0('poly(',new_vars[i],', 2)1')
  }
}

poly_vars <- gsub("^poly\\((.*), 2\\)1$", "\\1", 
                  grep("^poly\\((.*), 2\\)1$", new_vars, value = TRUE))

for (i in 1:length(new_vars)){
  aux <- gsub("^I\\((.*)\\^2\\)$", "\\1", new_vars[i])
  if(aux %in% poly_vars){
    new_vars[i] <- paste0('poly(', aux,', 2)2')
  }
}

colnames(mod_q0.95_bay$p.params.samples) <- gsub(
  '`','', colnames(mod_q0.95_bay$p.params.samples)
)

colnames(mod_q0.95_bay$p.params.samples)[4:18] <- new_vars[3: length(new_vars)]

#----medias de las betas predichas----
#mod_q0.95_bay <- readRDS('mod_q0.95_bay.rds')
traducir_nombres_coef <- function(nombres_coef) { 
  traducidos <- character(length(nombres_coef))
  
  for (i in seq_along(nombres_coef)) {
    nombre <- nombres_coef[i]
    
    if (grepl("^poly\\((.+), 2\\)1$", nombre)) {
      base <- sub("^poly\\((.+), 2\\)1$", "\\1", nombre)
      traducidos[i] <- base
    } else if (grepl("^poly\\((.+), 2\\)2$", nombre)) {
      base <- sub("^poly\\((.+), 2\\)2$", "\\1", nombre)
      traducidos[i] <- paste0("I(", base, "^2)")
    } else {
      traducidos[i] <- nombre
    }
  }
  
  return(traducidos)
}

betas <- function(vars, mod, cuantil){
  ind <- length(vars) + 1
  params <- as.data.frame(mod$p.params.samples)
  
  tr <- traducir_nombres_coef(colnames(params)[2:ind])
  colnames(params)[2:ind] <- gsub('`', '', tr)
  #intercepto
  int <- mean(params[['(Intercept)']])
  int <- rep(int, length=dim(stations)[1])
  
  #beta_fija
  betas_fijas <- apply(params[, vars], 2, mean)
  betas_fijas <- matrix(rep(betas_fijas, each = dim(stations)[1]), nrow = dim(stations)[1])

  elev <- mean(params[,'elev'])
  elev <- matrix(rep(elev, each = dim(stations)[1]), nrow = dim(stations)[1])
  dist <- mean(params[,'dist'])
  dist <- matrix(rep(dist, each = dim(stations)[1]), nrow = dim(stations)[1])

  #betas espaciales (beta1,...,beta16)
  cols <- grep('beta', names(params), value=T)
  mu <- apply(params[, cols], 2, mean)
  betas_esp <- matrix(mu, nrow = dim(stations)[1])
  
  #juntar en data frame
  betas <- cbind(int, elev, dist, betas_fijas, betas_esp)
  betas <- as.data.frame(betas, row.names = stations$NAME2)
  colnames(betas) <- c('intercept', 'elev', 'dist',
                       vars, paste0('beta',1:(length(vars) + 1)))

  
  return(betas)
}


betas_q0.95 <- betas(vars, mod_q0.95_bay, cuantil = 0.95)
betas_q0.90 <- betas(vars_q0.90, mod_q0.90_bay, cuantil = 0.90)
betas_q0.75 <- betas(vars_q0.75, mod_q0.75_bay, cuantil = 0.75)


#----Predicciones----
predictions <- function(vars, betas, df, cuantil){
  pred <- numeric(nrow(df))
  for (i in 1:dim(stations)[1]){
    ind <- which(df$station == stations$STAID[i])
    for (j in ind){
      #inicializar en interceptos
      comp_esp <- betas[i, 'beta1'] #intercepto espacial para la estacion i
      
      comp_fija <- betas[i, 'intercept'] #intercepto fijo para la estacion i
      
      for (k in 1:(length(vars))){ #beta 1 es la componente espacial del intercepto
        comp_esp <- comp_esp + betas[i, paste0('beta', k + 1)] * df[j, vars[k]]
        comp_fija <- comp_fija + betas[i, vars[k]] * df[j, vars[k]]
      }
      
      # if (cuantil==0.5){
      #   pred[j]<-comp_esp+comp_fija + betas[i,'elev']*elev_sc[i] + betas[i,'dist']*dist_sc[i]
      # }
      
      if (cuantil == 0.95){
        pred[j] <- comp_esp + comp_fija + betas[i, 'elev'] * elev_sc[i] + betas[i, 'dist'] * dist_sc[i]
      }
      
    }
  }
  
  return(pred)
}

pred_q0.95 <- predictions(vars, mod_q0.95_bay, betas_q0.95, v_q0.95, cuantil = 0.95)
pred_q0.95 <- cbind(v_q0.95[,c('Date', 'station', 'Y')], pred_q0.95)

pred_q0.90 <- predictions(vars_q0.90, mod_q0.90_bay, betas_q0.90, v_q0.90, cuantil = 0.90)
pred_q0.90 <- cbind(v_q0.90[,c('Date', 'station', 'Y')], pred_q0.90)

pred_q0.75 <- predictions(vars_q0.75, mod_q0.75_bay, betas_q0.75, v_q0.75, cuantil = 0.75)
pred_q0.75 <- cbind(v_q0.75[,c('Date', 'station', 'Y')], pred_q0.75)

# save(Y, df_final, stations, stations_dist, 
#      modelos_proyecto_q0.95, modelos_proyecto_q0.95_AIC, 
#      vars, v_q0.95, 
#      modelos_finales_q0.95,
#      formula, mod_q0.95_bay, elev_sc, dist_sc,
#      pred_q0.95, 
#      file = 'data.RData')

save(Y, df_final, stations, stations_dist,
     modelos_proyecto_q0.90, 
     vars_q0.90, v_q0.90,
     modelos_finales_q0.90,
     formula_q0.90, mod_q0.90_bay, elev_sc, dist_sc,
     pred_q0.90,
     file = 'data_q0.90/data.RData')
save(Y, df_final, stations, stations_dist,
     modelos_proyecto_q0.75, 
     vars_q0.75, v_q0.75,
     modelos_finales_q0.75,
     formula_q0.75, mod_q0.75_bay, elev_sc, dist_sc,
     pred_q0.75,
     file = 'data_q0.75/data.RData')


#----R1----
check <- function(u, tau) {
  return(u * (tau - (u < 0)))  # Implements the quantile loss function
}

R1_bay <- function(pred, tau, data){
  pred_clean <- na.omit(pred)
  
  #dataframe para global
  df <- matrix(NA, nrow=1, ncol=1)
  df <- as.data.frame(df)
  colnames(df) <- c('R1_bay_global')
  
  #dataframe para locales
  df_local <- matrix(NA, nrow=dim(stations)[1], ncol=1)
  df_local <- as.data.frame(df_local, row.names = stations$NAME2)
  colnames(df_local) <- c('R1_bay_local')
  
  #modelos nulos, son para todas variables igual
  mod_nulo_f <- rq(Y ~ as.factor(station), data = data, tau = tau)
  
  rho_estacion <- rep(NA, dim(stations)[1])
  R1_nulo_est <- rep(NA, dim(stations)[1])
  for (j in 1:length(rho_estacion)){
    ind <- which(pred_clean$station == stations$STAID[j])
    rho_estacion[j] <- sum(check(pred_clean$Y[ind] - pred_clean[ind,paste0('pred_q',tau)],tau = tau))
    R1_nulo_est[j] <- sum(check(mod_nulo_f$residuals[ind], tau = tau))
  }
  
  df['R1_bay_global'] <- 1 - sum(rho_estacion) / mod_nulo_f$rho
  df_local['R1_bay_local'] <- 1 - rho_estacion / R1_nulo_est
  
  return(list(R1_globales = df, R1_locales = df_local))
}

R1_bay_q0.95 <- R1_bay(pred_q0.95, 0.95, v_q0.95)
R1_bay_q0.90 <- R1_bay(pred_q0.90, 0.90, v_q0.90)
R1_bay_q0.75 <- R1_bay(pred_q0.75, 0.75, v_q0.75)


#----RHO----
library(lubridate)
rho_bay <- function(predicciones, tau){
  
  #global
  #dataframe para global
  df <- matrix(NA, nrow=1, ncol=1)
  df <- as.data.frame(df)
  colnames(df) <- c('rho_bay_global')
  pred <- predicciones[[paste0('pred_q',tau)]]
  dif<- predicciones$Y - pred
  df['rho_bay_global'] <- sum(dif < 0, na.rm = T) / ( 40 * 64 * 92)
  
  #estaciones
  df_est <- matrix(NA, nrow=dim(stations)[1], ncol=1)
  df_est <- as.data.frame(df_est, row.names = stations$NAME2)
  colnames(df_est) <- c('rho_bay_est')
  for (i in 1:dim(stations)[1]){
    ind <- which(predicciones$station == stations$STAID[i])
    dif <- predicciones$Y[ind]-pred[ind]
    df_est[i,] <- sum(dif < 0, na.rm = T) / (64 * 92)
  }
  
  #dias
  df_dia_list <- list() #lista para dias por estacion
  
  day_month <- unique(format(predicciones$Date, "%d-%m"))
  df_dia <- matrix(NA, nrow = length(day_month), ncol=1)
  df_dia <- as.data.frame(df_dia, row.names = day_month)
  colnames(df_dia) <- c('rho_bay_dia')
  
  for (i in 1:length(day_month)){
    ind <- which(format(predicciones$Date, "%d-%m") == day_month[i])
    dif <- predicciones$Y[ind] - pred[ind]
    df_dia[i,] <- sum(dif < 0, na.rm = T) / (64 * 40)
    
    #por estaciones
    for (j in 1:dim(stations)[1]){
      nombre <- stations$NAME2[j]
      
      # Si la estación aún no está en la lista, inicialízala
      if (!(nombre %in% names(df_dia_list))) {
        df_dia_list[[nombre]] <- data.frame(rho_bay_dia = rep(NA, length(day_month)), row.names = day_month)
      }
      df_temp <- df_dia_list[[nombre]]
      
      ind_2 <- which(predicciones$station == stations$STAID[j])
      ind_2 <- ind_2[which(ind_2 %in% ind)]
      dif <- predicciones$Y[ind_2] - pred[ind_2]
      
      #guardado
      df_temp[i, 1] <- sum(dif < 0, na.rm = T) / 64
      df_dia_list[[nombre]] <- df_temp
      
      
    }
    
    
  }
  
  #años
  df_year_list <- list()#lista para años por estacion
  
  year <- unique(year(predicciones$Date))
  df_year <- matrix(NA, nrow = length(year), ncol = 1)
  df_year <- as.data.frame(df_year, row.names = year)
  colnames(df_year) <- c('rho_bay_year')
  for (i in 1:length(year)){
    ind <- which(year(predicciones$Date) == year[i])
    dif <- predicciones$Y[ind] - pred[ind]
    df_year[i,] <- sum(dif < 0, na.rm = T) / (40 * 92)
    
    for (j in 1:dim(stations)[1]){
      nombre <- stations$NAME2[j]
      
      # Si la estación aún no está en la lista, inicialízala
      if (!(nombre %in% names(df_year_list))) {
        df_year_list[[nombre]] <- data.frame(rho_bay_year = rep(NA, length(year)), row.names = year)
      }
      df_temp <- df_year_list[[nombre]]
      
      ind_2 <- which(predicciones$station == stations$STAID[j])
      ind_2 <- ind_2[which(ind_2%in%ind)]
      dif <- predicciones$Y[ind_2] - pred[ind_2]
      
      #guardado
      df_temp[i, 1] <- sum(dif < 0, na.rm = T) / 92
      df_year_list[[nombre]] <- df_temp
    }
    
  }
  
  return(list(rho_globales=df,
              rho_estaciones=df_est,
              rho_años=df_year,
              rho_dias=df_dia,
              rho_dias_est=df_dia_list,
              rho_años_est=df_year_list))
}

rho_q0.95 <- rho_bay(pred_q0.95, 0.95)
rho_q0.90 <- rho_bay(pred_q0.90, 0.90)
rho_q0.75 <- rho_bay(pred_q0.75, 0.75)

# save(Y, df_final, stations, stations_dist,
#      modelos_proyecto_q0.95, modelos_proyecto_q0.95_AIC,
#      vars, v_q0.95,
#      modelos_finales_q0.95,
#      formula, mod_q0.95_bay, elev_sc, dist_sc,
#      pred_q0.95, R1_bay_q0.95, rho_q0.95,
#      file = 'data.RData')
save(Y, df_final, stations, stations_dist,
     modelos_proyecto_q0.90, 
     vars_q0.90, v_q0.90,
     modelos_finales_q0.90,
     formula_q0.90, mod_q0.90_bay, elev_sc, dist_sc,
     pred_q0.90, R1_bay_q0.90, rho_q0.90,
     file = 'data_q0.90/data.RData')
save(Y, df_final, stations, stations_dist,
     modelos_proyecto_q0.75, 
     vars_q0.75, v_q0.75,
     modelos_finales_q0.75,
     formula_q0.75, mod_q0.75_bay, elev_sc, dist_sc,
     pred_q0.75, R1_bay_q0.75, rho_q0.75,
     file = 'data_q0.70/data.RData')

# para sacar predicciones de las proyecciones
save(betas_q0.95, betas_q0.75, betas_q0.90,
     pred_q0.95, pred_q0.90, pred_q0.75,
     betas, check, predictions, 
     R1_bay, rho_bay, traducir_nombres_coef,
     file = 'predictions.RData')

# Gráficas de desagregación
par(mfrow = c(1, 2))
plot(1:length(rho_q0.95$rho_dias$rho_bay_dia), 
     rho_q0.95$rho_dias$rho_bay_dia, type = "l", xlab = "l", 
     ylab = expression(rho[l](0.95)),
     ylim=c(0.85,1),
     main = 'Desagregación por días')
abline(h=0.95,col='red')

plot(1:length(rho_q0.95$rho_años$rho_bay_year), 
     rho_q0.95$rho_años$rho_bay_year, type = "l", xlab = "t", 
     ylab = expression(rho[t](0.95)),
     ylim=c(0.85,1),
     main = 'Desagregación por años')
abline(h=0.95,col='red')


png('rho_def.png', width = 2000*3/2, height = 2200*3/2, res = 150)
par(mfrow=c(10,8))
for (i in 1:dim(stations)[1]){
  est <- stations$NAME2[i]
  #dias
  plot(1:92, rho_q0.95$rho_dias_est[[i]][, 1], type = 'l',
       xlab = 'l', ylim = c(0.85, 1), ylab = expression(rho[l](tau)),
       main = paste(est, '(días)'))
  abline(h = 0.95,col = 'red')

  # años
  plot(1:64, rho_q0.95$rho_años_est[[i]][, 1], type = 'l',
       xlab = 't', ylim = c(0.85, 1), ylab = expression(rho[l](tau)),
       main = paste(est, '(años)'))
  abline(h = 0.95, col = 'red')
  
}
dev.off()




#----Extra: Estudio de los residuos----
library(lubridate)
dev.off()

png('res_mod.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
for (i in 1:dim(stations)[1]){
  ind <- which(pred_q0.95$station == stations$STAID[i])
  name <- stations$NAME2[i]
  
  # model residuals
  aux <- data.frame(
    Date = pred_q0.95$Date[ind],
    res_mod_bay = pred_q0.95$Y[ind] - pred_q0.95$pred_q0.95[ind]
  )
  
  res_mod_bay <- tapply(aux$res_mod_bay, year(aux$Date), mean)
  
  # residuals of model with only harmonics (scaled)
  mod_harm <- rq(Y ~ c.1 + s.1, data = v_q0.95, subset = ind, tau = 0.95)
  res_mod_harm <- tapply(mod_harm$residuals, year(aux$Date), mean)
  
  # graph
  t <- 1:64
  min <- min(c(res_mod_bay, res_mod_harm))
  max <- max(c(res_mod_bay, res_mod_harm))
  
  plot(res_mod_bay, ylim = c(min, max), type = 'l',
       main = name)
  abline(lm(res_mod_bay ~ t))
  lines(res_mod_harm, col = 'red')
  abline(lm(res_mod_harm ~ t), col = 'red')
  
}
dev.off()




