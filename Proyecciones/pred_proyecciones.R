# ajuste del modelo a los datos de proyecciones
rm(list = ls())
load('predictions.RData')
load('data_q0.95/proyecciones.RData')
load('data_q0.90/proyecciones.RData')
load('data_q0.75/proyecciones.RData')
library(lubridate)
library(dplyr)
# anomalias estandarizadas según la regla utilizada para ajustar el modelo
# (MPI - mean(ERA5)) / sd(ERA5)

elev_sc <- scale(stations_dist$HGHT)
dist_sc <- scale(stations_dist$DIST)

pred_q0.95_proy <- predictions(vars, betas_q0.95, v_q0.95_proy, cuantil = 0.95)
pred_q0.95_proy_est <- predictions(vars, betas_q0.95, v_q0.95_proy_est, cuantil = 0.95)

pred_q0.95_proy_fut <- predictions(vars, betas_q0.95, v_q0.95_proy_fut, cuantil = 0.95)
pred_q0.95_proy_est_fut <- predictions(vars, betas_q0.95, v_q0.95_proy_est_fut, cuantil = 0.95)

pred_q0.90_proy <- predictions(vars_q0.90, betas_q0.90, v_q0.90_proy, cuantil = 0.90)
pred_q0.90_proy_est <- predictions(vars_q0.90, betas_q0.90, v_q0.90_proy_est, cuantil = 0.90)

pred_q0.90_proy_fut <- predictions(vars_q0.90, betas_q0.90, v_q0.90_proy_fut, cuantil = 0.90)
pred_q0.90_proy_est_fut <- predictions(vars_q0.90, betas_q0.90, v_q0.90_proy_est_fut, cuantil = 0.90)

pred_q0.75_proy <- predictions(vars_q0.75, betas_q0.75, v_q0.75_proy, cuantil = 0.75)
pred_q0.75_proy_est <- predictions(vars_q0.75, betas_q0.75, v_q0.75_proy_est, cuantil = 0.75)

pred_q0.75_proy_fut <- predictions(vars_q0.75, betas_q0.75, v_q0.75_proy_fut, cuantil = 0.75)
pred_q0.75_proy_est_fut <- predictions(vars_q0.75, betas_q0.75, v_q0.75_proy_est_fut, cuantil = 0.75)

fechas_cmip6 <- c(min(df_final_proy$Date), max(df_final_proy$Date))

# load('data.RData')
# IR DIRECTAMENTE A LOS READRDS
pred_q0.95_era5 <- pred_q0.95[which(pred_q0.95$Date >= fechas_cmip6[1] & pred_q0.95$Date <= fechas_cmip6[2]), ]
pred_q0.95_comp <- cbind(pred_q0.95_era5, pred_q0.95_proy, pred_q0.95_proy_est)
# meses moviles. Indicadores de nuevos meses 16 jun- 15 jul. 16 jul-15 ag
# pred_q0.95_comp <- pred_q0.95_comp %>%
#   mutate(
#     day = day(Date),
#     month = month(Date),
#     
#     month.aux = case_when(
#       (month == 6 & day >= 15) | (month == 7 & day <= 15) ~ 1,
#       (month == 7 & day >= 16) | (month == 8 & day <= 15) ~ 2,
#       TRUE ~ 0
#     )
#   )
# saveRDS(pred_q0.95_comp, 'data_q0.95/pred_q0.95_comp.rds')
pred_q0.95_comp <- readRDS('data_q0.95/pred_q0.95_comp.rds')
pred_q0.95_comp_ref <- pred_q0.95_comp[which(pred_q0.95_comp$Date >= '1981-06-01' &
                                               pred_q0.95_comp$Date <= '2010-08-31'), ]
#futuro
pred_q0.95_comp_fut <- df_final_proy_fut[, c(2,1)]
pred_q0.95_comp_fut$pred_q0.95_proy_fut <- pred_q0.95_proy_fut
pred_q0.95_comp_fut$pred_q0.95_proy_est_fut <- pred_q0.95_proy_est_fut

pred_q0.95_comp_fut <- pred_q0.95_comp_fut[which(pred_q0.95_comp_fut$Date >= '2031-06-01' &
                                                   pred_q0.95_comp_fut$Date <= '2060-08-31'), ]

aux <- pred_q0.95_comp_ref %>%
  filter(format(Date, '%m-%d') != '06-01')

pred_q0.95_comp_fut$pred_q0.95_proy_ref <- aux$pred_q0.95_proy
#saveRDS(pred_q0.95_comp_fut, 'data_q0.95/pred_q0.95_comp_fut.rds')
pred_q0.95_comp_fut <- readRDS('data_q0.95/pred_q0.95_comp_fut.rds')


pred_q0.90_era5 <- pred_q0.90[which(pred_q0.90$Date >= fechas_cmip6[1] & pred_q0.90$Date <= fechas_cmip6[2]), ]
pred_q0.90_comp <- cbind(pred_q0.90_era5, pred_q0.90_proy, pred_q0.90_proy_est)
# pred_q0.90_comp$month.aux <- pred_q0.95_comp$month.aux
# saveRDS(pred_q0.90_comp, 'data_q0.90/pred_q0.90_comp.rds')
pred_q0.90_comp <- readRDS('data_q0.90/pred_q0.90_comp.rds')
pred_q0.90_comp_ref <- pred_q0.90_comp[which(pred_q0.90_comp$Date >= '1981-06-01' &
                                               pred_q0.90_comp$Date <= '2010-08-31'), ]
#futuro
pred_q0.90_comp_fut <- df_final_proy_fut[, c(2,1)]
pred_q0.90_comp_fut$pred_q0.90_proy_fut <- pred_q0.90_proy_fut
pred_q0.90_comp_fut$pred_q0.90_proy_est_fut <- pred_q0.90_proy_est_fut

pred_q0.90_comp_fut <- pred_q0.90_comp_fut[which(pred_q0.90_comp_fut$Date >= '2031-06-01' &
                                                   pred_q0.90_comp_fut$Date <= '2060-08-31'), ]

aux <- pred_q0.90_comp_ref %>%
  filter(format(Date, '%m-%d') != '06-01')

pred_q0.90_comp_fut$pred_q0.90_proy_ref <- aux$pred_q0.90_proy
#saveRDS(pred_q0.90_comp_fut, 'data_q0.90/pred_q0.90_comp_fut.rds')
pred_q0.90_comp_fut <- readRDS('data_q0.90/pred_q0.90_comp_fut.rds')


pred_q0.75_era5 <- pred_q0.75[which(pred_q0.75$Date >= fechas_cmip6[1] & pred_q0.75$Date <= fechas_cmip6[2]), ]
pred_q0.75_comp <- cbind(pred_q0.75_era5, pred_q0.75_proy, pred_q0.75_proy_est)
# pred_q0.75_comp$month.aux <- pred_q0.95_comp$month.aux
# saveRDS(pred_q0.75_comp, 'data_q0.75/pred_q0.75_comp.rds')
pred_q0.75_comp <- readRDS('data_q0.75/pred_q0.75_comp.rds')
pred_q0.75_comp_ref <- pred_q0.75_comp[which(pred_q0.75_comp$Date >= '1981-06-01' &
                                               pred_q0.75_comp$Date <= '2010-08-31'), ]
#futuro
pred_q0.75_comp_fut <- df_final_proy_fut[, c(2,1)]
pred_q0.75_comp_fut$pred_q0.75_proy_fut <- pred_q0.75_proy_fut
pred_q0.75_comp_fut$pred_q0.75_proy_est_fut <- pred_q0.75_proy_est_fut

pred_q0.75_comp_fut <- pred_q0.75_comp_fut[which(pred_q0.75_comp_fut$Date >= '2031-06-01' &
                                                   pred_q0.75_comp_fut$Date <= '2060-08-31'), ]

aux <- pred_q0.75_comp_ref %>%
  filter(format(Date, '%m-%d') != '06-01')

pred_q0.75_comp_fut$pred_q0.75_proy_ref <- aux$pred_q0.75_proy
#saveRDS(pred_q0.75_comp_fut, 'data_q0.75/pred_q0.75_comp_fut.rds')
pred_q0.75_comp_fut <- readRDS('data_q0.75/pred_q0.75_comp_fut.rds')


# solo para q0.95. Para resto cuantiles solo lo hacemos en el periodo de referencia
p1 <- which(year(pred_q0.95_comp$Date) >= '1960'
            & year(pred_q0.95_comp$Date) <= '1977')
p2 <- which(year(pred_q0.95_comp$Date) >= '1978'
            & year(pred_q0.95_comp$Date) <= '1995')
p3 <- which(year(pred_q0.95_comp$Date) >= '1996')

p_ref_ant <- which(year(pred_q0.95_comp$Date) <= '1980')
p_ref <- which(year(pred_q0.95_comp$Date) >= '1981'
               & year(pred_q0.95_comp$Date) <= '2010')
p_ref_post <- which(year(pred_q0.95_comp$Date) >= '2011')

# hacer qqplots para periodos antes y depsues del periodo de referencia

#----DENSIDADES----
density_plots <- function(data, col1, col2, col3, 
                          type = NULL, month = NULL, period = NULL){
  if (is.null(type)){
    for (i in 1:dim(stations)[1]){
      ind <- which(data$station == stations$STAID[i])
      name <- stations$NAME2[i]
      
      dens1 <- density(data[ind, col1], 
                       from = min(data[ind, col1]),
                       to   = max(data[ind, col1]))
      
      
      dens2 <- density(data[ind, col2], 
                       from = min(data[ind, col2]),
                       to   = max(data[ind, col2]))
      
      dens3 <- density(data[ind, col3], 
                       from = min(data[ind, col3]),
                       to   = max(data[ind, col3]))
      
      plot(dens1, col = "blue", lwd = 2, 
           main = paste0('Dens. ERA5 vs CMIP6 (', name, ')'),
           xlab = 'ºC',
           ylim = c(0,max(dens1$y, dens2$y, dens3$y)))
      lines(dens2, col = "red", lwd = 2)
      lines(dens3, col = "darkgreen", lwd = 2)
      
      legend("topleft", legend = c("ERA5", "CMIP6", 'CMIP6 (est)'),
             col = c("blue", "red", 'darkgreen'), lwd = 2)
      
    }
  }else if (type == 'months' & !is.null(month)){
    for (i in 1:dim(stations)[1]){
      ind <- which(data$station == stations$STAID[i]
                   & format(data$Date, '%m') == month)
      
      name <- stations$NAME2[i]
      
      dens1 <- density(data[ind, col1], 
                       from = min(data[ind, col1]),
                       to   = max(data[ind, col1]))
      
      dens2 <- density(data[ind, col2], 
                       from = min(data[ind, col2]),
                       to   = max(data[ind, col2]))
      
      dens3 <- density(data[ind, col3], 
                       from = min(data[ind, col3]),
                       to   = max(data[ind, col3]))
      
      mm.aux <- month.abb[as.integer(month)]
      plot(dens1, col = "blue", lwd = 2, 
           main = paste0('Dens. ', mm.aux, ' ERA5 vs CMIP6 (', name, ')'),
           xlab = 'ºC',
           ylim = c(0,max(dens1$y, dens2$y, dens3$y)))
      lines(dens2, col = "red", lwd = 2)
      lines(dens3, col = "darkgreen", lwd = 2)
      
      legend("topleft", legend = c("ERA5", "CMIP6", 'CMIP6 (est)'),
             col = c("blue", "red", 'darkgreen'), lwd = 2)
      
    }
  }else if (type == 'period' & !is.null(period)){
    for (i in 1:dim(stations)[1]){
      data_period <- data[period, ]
      ind <- which(data_period$station == stations$STAID[i])
      
      name <- stations$NAME2[i]
      
      dens1 <- density(data_period[ind, col1], 
                       from = min(data_period[ind, col1]),
                       to   = max(data_period[ind, col1]))
      
      dens2 <- density(data_period[ind, col2], 
                       from = min(data_period[ind, col2]),
                       to   = max(data_period[ind, col2]))
      
      dens3 <- density(data_period[ind, col3], 
                       from = min(data_period[ind, col3]),
                       to   = max(data_period[ind, col3]))
      
      period.aux <- c(min(year(data_period$Date)), max(year(data_period$Date)))
      plot(dens1, col = "blue", lwd = 2, 
           main = paste0('Dens. ', period.aux[1], '-', period.aux[2], ' . ERA5 vs CMIP6 (', name, ')'),
           xlab = 'ºC',
           ylim = c(0,max(dens1$y, dens2$y, dens3$y)))
      lines(dens2, col = "red", lwd = 2)
      lines(dens3, col = "darkgreen", lwd = 2)
      
      legend("topleft", legend = c("ERA5", "CMIP6", 'CMIP6 (est)'),
             col = c("blue", "red", 'darkgreen'), lwd = 2)
      
    }
  }
}

png('Proyecciones/density_plots/comp_dens.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est')
dev.off()

png('Proyecciones/density_plots/comp_dens_ref.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp_ref, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est')
dev.off()

#por meses 
png('Proyecciones/density_plots/dens_jun.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'months',
              month = '06')
dev.off()

png('Proyecciones/density_plots/dens_jun_ref.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp_ref, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'months',
              month = '06')
dev.off()

png('Proyecciones/density_plots/dens_jul.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'months',
              month = '07')
dev.off()

png('Proyecciones/density_plots/dens_jul_ref.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp_ref, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'months',
              month = '07')
dev.off()

png('Proyecciones/density_plots/dens_aug.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'months',
              month = '08')
dev.off()

png('Proyecciones/density_plots/dens_aug_ref.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp_ref, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'months',
              month = '08')
dev.off()

#por periodos
png('Proyecciones/density_plots/dens_p1.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'period',
              period = p1)
dev.off()

png('Proyecciones/density_plots/dens_p2.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'period',
              period = p2)
dev.off()

png('Proyecciones/density_plots/dens_p3.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'period',
              period = p3)
dev.off()

png('Proyecciones/density_plots/dens_p_ref_ant.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'period',
              period = p_ref_ant)
dev.off()

png('Proyecciones/density_plots/dens_p_ref_post.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'period',
              period = p_ref_post)
dev.off()

# CUANTIL 0.90
png('Proyecciones/density_plots/q0.90/comp_dens_ref_q0.90.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.90_comp_ref, 
              'pred_q0.90', 
              'pred_q0.90_proy',
              'pred_q0.90_proy_est')
dev.off()

png('Proyecciones/density_plots/q0.90/dens_jun_ref_q0.90.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.90_comp_ref, 
              'pred_q0.90', 
              'pred_q0.90_proy',
              'pred_q0.90_proy_est',
              type = 'months',
              month = '06')
dev.off()

png('Proyecciones/density_plots/q0.90/dens_jul_ref_q0.90.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.90_comp_ref, 
              'pred_q0.90', 
              'pred_q0.90_proy',
              'pred_q0.90_proy_est',
              type = 'months',
              month = '07')
dev.off()

png('Proyecciones/density_plots/q0.90/dens_aug_ref_q0.90.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.90_comp_ref, 
              'pred_q0.90', 
              'pred_q0.90_proy',
              'pred_q0.90_proy_est',
              type = 'months',
              month = '08')
dev.off()

# CUANTIL 0.75
png('Proyecciones/density_plots/q0.75/comp_dens_ref_q0.75.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.75_comp_ref, 
              'pred_q0.75', 
              'pred_q0.75_proy',
              'pred_q0.75_proy_est')
dev.off()

png('Proyecciones/density_plots/q0.75/dens_jun_ref_q0.75.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.75_comp_ref, 
              'pred_q0.75', 
              'pred_q0.75_proy',
              'pred_q0.75_proy_est',
              type = 'months',
              month = '06')
dev.off()

png('Proyecciones/density_plots/q0.75/dens_jul_ref_q0.75.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.75_comp_ref, 
              'pred_q0.75', 
              'pred_q0.75_proy',
              'pred_q0.75_proy_est',
              type = 'months',
              month = '07')
dev.off()

png('Proyecciones/density_plots/q0.75/dens_aug_ref_q0.75.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.75_comp_ref, 
              'pred_q0.75', 
              'pred_q0.75_proy',
              'pred_q0.75_proy_est',
              type = 'months',
              month = '08')
dev.off()

#----DENSITY OVERLAPPING----
library(overlapping)
basura <- overlap(list(ERA5 = pred_q0.95_comp_ref$pred_q0.95[ind],
                       CMIP6 = pred_q0.95_comp_ref$pred_q0.95_proy[ind],
                       `CMIP6 (est)` = pred_q0.95_comp_ref$pred_q0.95_proy_est[ind]),
                  type = '1',
                  plot = T)
# overlapping function for the reference period
overlap_dens <- function(data, era5, cmip6, cmip6_est){
  
  df <- data.frame(matrix(NA, ncol = 2, nrow = dim(stations)[1]))
  colnames(df) <- c('STAID', 'NAME2')
  df$STAID <- stations$STAID
  df$NAME2 <- stations$NAME2
  
  for (i in 1:dim(stations)[1]){
    # whole period
    ind <- which(data$station == stations$STAID[i])
    name <- stations$NAME2[i]
    id <- stations$STAID[i]
    
    ov <- overlap(
      list(ERA5 = data[ind, era5],
           CMIP6 = data[ind, cmip6],
           `CMIP6 (est)` = data[ind, cmip6_est]),
      type = '1'
    )
    
    df[i, 'ERA5-CMIP6'] <- ov$OVPairs[1]
    df[i, 'ERA5-CMIP6(est)'] <- ov$OVPairs[2]
    df[i, 'dif.CMIP6'] <- ov$OVPairs[1] - ov$OVPairs[2]
    #months
    for (m in c('06', '07', '08')){
      
      data.aux <- data %>%
        filter(station == stations$STAID[i],
               format(Date, "%m") == m)
      
      ov.m <- overlap(
        list(ERA5 = data.aux[[era5]],
             CMIP6 = data.aux[[cmip6]],
             `CMIP6 (est)` = data.aux[[cmip6_est]]),
        type = '1'
      )
      
      mm.aux <- month.abb[as.integer(m)]
      
      df[i, paste0(mm.aux, '.ERA5-CMIP6')] <- ov.m$OVPairs[1]
      df[i, paste0(mm.aux, '.ERA5-CMIP6(est)')] <- ov.m$OVPairs[2]
      df[i, paste0('dif.', mm.aux, '.CMIP6')] <- ov.m$OVPairs[1] - ov.m$OVPairs[2]
  
    }
    
    #auxiliary momths
    for (m in c(1, 2)){
      
      data.aux <- data %>%
        filter(station == stations$STAID[i],
               month.aux == m)
      
      ov.m <- overlap(
        list(ERA5 = data.aux[[era5]],
             CMIP6 = data.aux[[cmip6]],
             `CMIP6 (est)` = data.aux[[cmip6_est]]),
        type = '1'
      )
      
      if (m == 1){
        mm.aux <- '15jun.15jul'
      }else if(m == 2){
        mm.aux <- '16jul.15aug'
      }
      
      
      df[i, paste0(mm.aux, '.ERA5-CMIP6')] <- ov.m$OVPairs[1]
      df[i, paste0(mm.aux, '.ERA5-CMIP6(est)')] <- ov.m$OVPairs[2]
      df[i, paste0('dif.', mm.aux, '.CMIP6')] <- ov.m$OVPairs[1] - ov.m$OVPairs[2]
      
    }
  }
  
  
  return(df)
  
}

ov_dens_q0.95 <- overlap_dens(pred_q0.95_comp_ref, 'pred_q0.95', 
                              'pred_q0.95_proy', 'pred_q0.95_proy_est')
ov_dens_q0.90 <- overlap_dens(pred_q0.90_comp_ref, 'pred_q0.90', 
                              'pred_q0.90_proy', 'pred_q0.90_proy_est')
ov_dens_q0.75 <- overlap_dens(pred_q0.75_comp_ref, 'pred_q0.75', 
                              'pred_q0.75_proy', 'pred_q0.75_proy_est')

source('mapa_Spain.R')
mapa_ov <- function(ov_dens){
  m1 <- spain_points(ov_dens$`ERA5-CMIP6`, stations,  c(0.8, 1), 0.05, 0.95, 'ERA5-CMIP6', 'Overlap')
  m2 <- spain_points(ov_dens$`ERA5-CMIP6(est)`, stations, c(0.8, 1), 0.05, 0.95, 'ERA5-CMIP6(est)', 'Overlap')
  m3 <- spain_points(ov_dens$dif.CMIP6, stations, c(-0.05, 0.05), 0.05, 0, 'dif. CMIP6', 'Diferencia overlap', dif = TRUE)
  
  m_total <- ggpubr::ggarrange(m1, m2, m3, 
                         nrow = 1, ncol = 3,
                         common.legend = F, legend = 'bottom')
  
  m1 <- spain_points(ov_dens$`Jun.ERA5-CMIP6`, stations,  c(0.8, 1), 0.05, 0.95, 'ERA5-CMIP6 Junio', 'Overlap')
  m2 <- spain_points(ov_dens$`Jun.ERA5-CMIP6(est)`, stations, c(0.8, 1), 0.05, 0.95, 'ERA5-CMIP6(est) Junio', 'Overlap')
  m3 <- spain_points(ov_dens$dif.Jun.CMIP6, stations, c(-0.05, 0.05), 0.05, 0, 'dif. CMIP6 Junio', 'Diferencia overlap', dif = TRUE)
  
  m_junio <- ggpubr::ggarrange(m1, m2, m3, 
                               nrow = 1, ncol = 3,
                               common.legend = F, legend = 'bottom')
  
  m1 <- spain_points(ov_dens$`Jul.ERA5-CMIP6`, stations,  c(0.8, 1), 0.05, 0.95, 'ERA5-CMIP6 Julio', 'Overlap')
  m2 <- spain_points(ov_dens$`Jul.ERA5-CMIP6(est)`, stations, c(0.8, 1), 0.05, 0.95, 'ERA5-CMIP6(est) Julio', 'Overlap')
  m3 <- spain_points(ov_dens$dif.Jul.CMIP6, stations, c(-0.05, 0.05), 0.05, 0, 'dif. CMIP6 Julio', 'Diferencia overlap', dif = TRUE)
  
  m_julio <- ggpubr::ggarrange(m1, m2, m3, 
                               nrow = 1, ncol = 3,
                               common.legend = F, legend = 'bottom')
  
  m1 <- spain_points(ov_dens$`Aug.ERA5-CMIP6`, stations,  c(0.8, 1), 0.05, 0.95, 'ERA5-CMIP6 Agosto', 'Overlap')
  m2 <- spain_points(ov_dens$`Aug.ERA5-CMIP6(est)`, stations, c(0.8, 1), 0.05, 0.95, 'ERA5-CMIP6(est) Agosto', 'Overlap')
  m3 <- spain_points(ov_dens$dif.Aug.CMIP6, stations, c(-0.05, 0.05), 0.05, 0, 'dif. CMIP6 Agosto', 'Diferencia overlap', dif = TRUE)
  
  m_agosto <- ggpubr::ggarrange(m1, m2, m3, 
                               nrow = 1, ncol = 3,
                               common.legend = F, legend = 'bottom')
  
  m1 <- spain_points(ov_dens$`15jun.15jul.ERA5-CMIP6`, stations,  c(0.8, 1), 0.05, 0.95, 'ERA5-CMIP6 15 junio - 15 julio', 'Overlap')
  m2 <- spain_points(ov_dens$`15jun.15jul.ERA5-CMIP6(est)`, stations, c(0.8, 1), 0.05, 0.95, 'ERA5-CMIP6(est) 15 junio - 15 julio', 'Overlap')
  m3 <- spain_points(ov_dens$dif.15jun.15jul.CMIP6, stations, c(-0.05, 0.05), 0.05, 0, 'dif. CMIP6 15 junio - 15 julio', 'Diferencia overlap', dif = TRUE)
  
  m_15jun.15jul <- ggpubr::ggarrange(m1, m2, m3, 
                               nrow = 1, ncol = 3,
                               common.legend = F, legend = 'bottom')
  
  m1 <- spain_points(ov_dens$`16jul.15aug.ERA5-CMIP6`, stations,  c(0.8, 1), 0.05, 0.95, 'ERA5-CMIP6 16 julio - 15 agosto', 'Overlap')
  m2 <- spain_points(ov_dens$`16jul.15aug.ERA5-CMIP6(est)`, stations, c(0.8, 1), 0.05, 0.95, 'ERA5-CMIP6(est) 16 julio - 15 agosto', 'Overlap')
  m3 <- spain_points(ov_dens$dif.16jul.15aug.CMIP6, stations, c(-0.05, 0.05), 0.05, 0, 'dif. CMIP6 16 julio - 15 agosto', 'Diferencia overlap', dif = TRUE)
  
  m_16jul.15ag <- ggpubr::ggarrange(m1, m2, m3, 
                               nrow = 1, ncol = 3,
                               common.legend = F, legend = 'bottom')
  
  
  return(list(
    total = m_total,
    junio = m_junio,
    julio = m_julio,
    agosto = m_agosto,
    `15jun.15jul` = m_15jun.15jul,
    `16jul.15aug` = m_16jul.15ag
  ))
}

mapa_ov_q0.95 <- mapa_ov(ov_dens_q0.95)
mapa_ov_q0.90 <- mapa_ov(ov_dens_q0.90)
mapa_ov_q0.75 <- mapa_ov(ov_dens_q0.75)

#saving graphs q0.95
ggsave(
  filename = "Proyecciones/overlaps/dens_ov_total_q0.95.png", 
  plot = mapa_ov_q0.95$total, 
  width = 12,
  height = 5,     
  dpi = 300       
)

ggsave(
  filename = "Proyecciones/overlaps/dens_ov_junio_q0.95.png", 
  plot = mapa_ov_q0.95$junio, 
  width = 12,
  height = 5,     
  dpi = 300       
)

ggsave(
  filename = "Proyecciones/overlaps/dens_ov_julio_q0.95.png", 
  plot = mapa_ov_q0.95$julio, 
  width = 12,
  height = 5,     
  dpi = 300       
)

ggsave(
  filename = "Proyecciones/overlaps/dens_ov_agosto_q0.95.png", 
  plot = mapa_ov_q0.95$agosto, 
  width = 12,
  height = 5,     
  dpi = 300       
)

ggsave(
  filename = "Proyecciones/overlaps/dens_ov_15jun.15jul_q0.95.png", 
  plot = mapa_ov_q0.95$`15jun.15jul`, 
  width = 12,
  height = 5,     
  dpi = 300       
)

ggsave(
  filename = "Proyecciones/overlaps/dens_ov_16jul.15aug_q0.95.png", 
  plot = mapa_ov_q0.95$`16jul.15aug`, 
  width = 12,
  height = 5,     
  dpi = 300       
)

# 0.90
ggsave(
  filename = "Proyecciones/overlaps/dens_ov_total_q0.90.png", 
  plot = mapa_ov_q0.90$total, 
  width = 12,
  height = 5,     
  dpi = 300       
)

ggsave(
  filename = "Proyecciones/overlaps/dens_ov_junio_q0.90.png", 
  plot = mapa_ov_q0.90$junio, 
  width = 12,
  height = 5,     
  dpi = 300       
)

ggsave(
  filename = "Proyecciones/overlaps/dens_ov_julio_q0.90.png", 
  plot = mapa_ov_q0.90$julio, 
  width = 12,
  height = 5,     
  dpi = 300       
)

ggsave(
  filename = "Proyecciones/overlaps/dens_ov_agosto_q0.90.png", 
  plot = mapa_ov_q0.90$agosto, 
  width = 12,
  height = 5,     
  dpi = 300       
)

ggsave(
  filename = "Proyecciones/overlaps/dens_ov_15jun.15jul_q0.90.png", 
  plot = mapa_ov_q0.90$`15jun.15jul`, 
  width = 12,
  height = 5,     
  dpi = 300       
)

ggsave(
  filename = "Proyecciones/overlaps/dens_ov_16jul.15aug_q0.90.png", 
  plot = mapa_ov_q0.90$`16jul.15aug`, 
  width = 12,
  height = 5,     
  dpi = 300       
)

# 0.75
ggsave(
  filename = "Proyecciones/overlaps/dens_ov_total_q0.75.png", 
  plot = mapa_ov_q0.75$total, 
  width = 12,
  height = 5,     
  dpi = 300       
)

ggsave(
  filename = "Proyecciones/overlaps/dens_ov_junio_q0.75.png", 
  plot = mapa_ov_q0.75$junio, 
  width = 12,
  height = 5,     
  dpi = 300       
)

ggsave(
  filename = "Proyecciones/overlaps/dens_ov_julio_q0.75.png", 
  plot = mapa_ov_q0.75$julio, 
  width = 12,
  height = 5,     
  dpi = 300       
)

ggsave(
  filename = "Proyecciones/overlaps/dens_ov_agosto_q0.75.png", 
  plot = mapa_ov_q0.75$agosto, 
  width = 12,
  height = 5,     
  dpi = 300       
)

ggsave(
  filename = "Proyecciones/overlaps/dens_ov_15jun.15jul_q0.75.png", 
  plot = mapa_ov_q0.75$`15jun.15jul`, 
  width = 12,
  height = 5,     
  dpi = 300       
)

ggsave(
  filename = "Proyecciones/overlaps/dens_ov_16jul.15aug_q0.75.png", 
  plot = mapa_ov_q0.75$`16jul.15aug`, 
  width = 12,
  height = 5,     
  dpi = 300       
)

#----QQPLOTS----
#convertir a FUNCION 
qqplots <- function(data, col1, col2, 
                          type = NULL, month = NULL, period = NULL){
  if (is.null(type)){
    for (i in 1:dim(stations)[1]){
      ind <- which(data$station == stations$STAID[i])
      name <- stations$NAME2[i]
      
      qqplot(data[ind, col1], data[ind, col2],
             main = paste0("QQ Plot: ERA5 vs CMIP6 (", name,")"),
             xlab = "ERA5 (ºC)",
             ylab = "CMIP6 (ºC)")
      abline(0, 1, col = "red")
      
    }
  }else if (type == 'months' & !is.null(month)){
    for (i in 1:dim(stations)[1]){
      ind <- which(data$station == stations$STAID[i]
                   & format(data$Date, '%m') == month)
      
      name <- stations$NAME2[i]
      
      mm.aux <- month.abb[as.integer(month)]
      qqplot(data[ind, col1], data[ind, col2],
             main = paste0("QQ Plot ", mm.aux, ": ERA5 vs CMIP6 (", name,")"),
             xlab = "ERA5 (ºC)",
             ylab = "CMIP6 (ºC)")
      abline(0, 1, col = "red")
      
    }
  }else if (type == 'period' & !is.null(period)){
    for (i in 1:dim(stations)[1]){
      data_period <- data[period, ]
      ind <- which(data_period$station == stations$STAID[i])
      
      name <- stations$NAME2[i]
      
      period.aux <- c(min(year(data_period$Date)), max(year(data_period$Date)))
      
      qqplot(data_period[ind, col1], data_period[ind, col2],
             main = paste0("QQ Plot", period.aux[1], '-', period.aux[2], ": ERA5 vs CMIP6 (", name,")"),
             xlab = "ERA5 (ºC)",
             ylab = "CMIP6 (ºC)")
      abline(0, 1, col = "red")
      
    }
  }
}

## CON RESIDUOS SIN TRANSFORMAR
png('Proyecciones/qqplots/qqplot.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy')
dev.off()

png('Proyecciones/qqplots/qqplot_ref.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp_ref, 
              'pred_q0.95', 
              'pred_q0.95_proy')
dev.off()

#por meses 
png('Proyecciones/qqplots/qqplot_jun.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              type = 'months',
              month = '06')
dev.off()

png('Proyecciones/qqplots/qqplot_jun_ref.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp_ref, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              type = 'months',
              month = '06')
dev.off()

png('Proyecciones/qqplots/qqplot_jul.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              type = 'months',
              month = '07')
dev.off()

png('Proyecciones/qqplots/qqplot_jul_ref.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp_ref, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              type = 'months',
              month = '07')
dev.off()

png('Proyecciones/qqplots/qqplot_aug.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              type = 'months',
              month = '08')
dev.off()

png('Proyecciones/qqplots/qqplot_aug_ref.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp_ref, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              type = 'months',
              month = '08')
dev.off()

#por periodos
png('Proyecciones/qqplots/qqplot_p1.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              type = 'period',
              period = p1)
dev.off()

png('Proyecciones/qqplots/qqplot_p2.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              type = 'period',
              period = p2)
dev.off()

png('Proyecciones/qqplots/qqplot_p3.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              type = 'period',
              period = p3)
dev.off()

png('Proyecciones/qqplots/qqplot_p_ref_ant.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              type = 'period',
              period = p_ref_ant)
dev.off()

png('Proyecciones/qqplots/qqplot_p_ref_post.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
        'pred_q0.95', 
        'pred_q0.95_proy',
        type = 'period',
        period = p_ref_post)
dev.off()

## CON RESIDUOS ESTANDARIZADOS
png('Proyecciones/qqplots/qqplot_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
        'pred_q0.95', 
        'pred_q0.95_proy_est')
dev.off()

png('Proyecciones/qqplots/qqplot_ref_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp_ref, 
        'pred_q0.95', 
        'pred_q0.95_proy_est')
dev.off()

#por meses 
png('Proyecciones/qqplots/qqplot_jun_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
        'pred_q0.95', 
        'pred_q0.95_proy_est',
        type = 'months',
        month = '06')
dev.off()

png('Proyecciones/qqplots/qqplot_jun_ref_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp_ref, 
        'pred_q0.95', 
        'pred_q0.95_proy_est',
        type = 'months',
        month = '06')
dev.off()

png('Proyecciones/qqplots/qqplot_jul_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
        'pred_q0.95', 
        'pred_q0.95_proy_est',
        type = 'months',
        month = '07')
dev.off()

png('Proyecciones/qqplots/qqplot_jul_ref_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp_ref, 
        'pred_q0.95', 
        'pred_q0.95_proy_est',
        type = 'months',
        month = '07')
dev.off()

png('Proyecciones/qqplots/qqplot_aug_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
        'pred_q0.95', 
        'pred_q0.95_proy_est',
        type = 'months',
        month = '08')
dev.off()

png('Proyecciones/qqplots/qqplot_aug_ref_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp_ref, 
        'pred_q0.95', 
        'pred_q0.95_proy_est',
        type = 'months',
        month = '08')
dev.off()

#por periodos
png('Proyecciones/qqplots/qqplot_p1_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
        'pred_q0.95', 
        'pred_q0.95_proy_est',
        type = 'period',
        period = p1)
dev.off()

png('Proyecciones/qqplots/qqplot_p2_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
        'pred_q0.95', 
        'pred_q0.95_proy_est',
        type = 'period',
        period = p2)
dev.off()

png('Proyecciones/qqplots/qqplot_p3_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
        'pred_q0.95', 
        'pred_q0.95_proy_est',
        type = 'period',
        period = p3)
dev.off()

png('Proyecciones/qqplots/qqplot_p_ref_ant_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
        'pred_q0.95', 
        'pred_q0.95_proy_est',
        type = 'period',
        period = p_ref_ant)
dev.off()

png('Proyecciones/qqplots/qqplot_p_ref_post_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.95_comp, 
        'pred_q0.95', 
        'pred_q0.95_proy_est',
        type = 'period',
        period = p_ref_post)
dev.off()

# CUANTIL 0.90
png('Proyecciones/qqplots/q0.90/qqplot_ref_q0.90.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.90_comp_ref, 
        'pred_q0.90', 
        'pred_q0.90_proy')
dev.off()

png('Proyecciones/qqplots/q0.90/qqplot_ref_est_q0.90.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.90_comp_ref, 
        'pred_q0.90', 
        'pred_q0.90_proy_est')
dev.off()

png('Proyecciones/qqplots/q0.90/qqplot_jun_ref_q0.90.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.90_comp_ref, 
        'pred_q0.90', 
        'pred_q0.90_proy',
        type = 'months',
        month = '06')
dev.off()

png('Proyecciones/qqplots/q0.90/qqplot_jun_ref_est_q0.90.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.90_comp_ref, 
        'pred_q0.90', 
        'pred_q0.90_proy_est',
        type = 'months',
        month = '06')
dev.off()

png('Proyecciones/qqplots/q0.90/qqplot_jul_ref_q0.90.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.90_comp_ref, 
        'pred_q0.90', 
        'pred_q0.90_proy',
        type = 'months',
        month = '07')
dev.off()

png('Proyecciones/qqplots/q0.90/qqplot_jul_ref_est_q0.90.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.90_comp_ref, 
        'pred_q0.90', 
        'pred_q0.90_proy_est',
        type = 'months',
        month = '07')
dev.off()

png('Proyecciones/qqplots/q0.90/qqplot_aug_ref_q0.90.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.90_comp_ref, 
        'pred_q0.90', 
        'pred_q0.90_proy',
        type = 'months',
        month = '08')
dev.off()

png('Proyecciones/qqplots/q0.90/qqplot_aug_ref_est_q0.90.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.90_comp_ref, 
        'pred_q0.90', 
        'pred_q0.90_proy_est',
        type = 'months',
        month = '08')
dev.off()


# CUANTIL 0.75
png('Proyecciones/qqplots/q0.75/qqplot_ref_q0.75.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.75_comp_ref, 
        'pred_q0.75', 
        'pred_q0.75_proy')
dev.off()

png('Proyecciones/qqplots/q0.75/qqplot_ref_est_q0.75.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.75_comp_ref, 
        'pred_q0.75', 
        'pred_q0.75_proy_est')
dev.off()

png('Proyecciones/qqplots/q0.75/qqplot_jun_ref_q0.75.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.75_comp_ref, 
        'pred_q0.75', 
        'pred_q0.75_proy',
        type = 'months',
        month = '06')
dev.off()

png('Proyecciones/qqplots/q0.75/qqplot_jun_ref_est_q0.75.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.75_comp_ref, 
        'pred_q0.75', 
        'pred_q0.75_proy_est',
        type = 'months',
        month = '06')
dev.off()

png('Proyecciones/qqplots/q0.75/qqplot_jul_ref_q0.75.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.75_comp_ref, 
        'pred_q0.75', 
        'pred_q0.75_proy',
        type = 'months',
        month = '07')
dev.off()

png('Proyecciones/qqplots/q0.75/qqplot_jul_ref_est_q0.75.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.75_comp_ref, 
        'pred_q0.75', 
        'pred_q0.75_proy_est',
        type = 'months',
        month = '07')
dev.off()

png('Proyecciones/qqplots/q0.75/qqplot_aug_ref_q0.75.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.75_comp_ref, 
        'pred_q0.75', 
        'pred_q0.75_proy',
        type = 'months',
        month = '08')
dev.off()

png('Proyecciones/qqplots/q0.75/qqplot_aug_ref_est_q0.75.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
qqplots(pred_q0.75_comp_ref, 
        'pred_q0.75', 
        'pred_q0.75_proy_est',
        type = 'months',
        month = '08')
dev.off()


#----KS TEST meses Y PERIODOS----
ks_test_df <- function(data, data_ref, type, pred, pred_proy){
  cat('KS según ', type, '\n')
  
  if (type == 'month'){
    
    ks_df <- data.frame(
      stations = stations$STAID,
      NAME2 = stations$NAME2
    )
    
    cat('Cálculo para periodo completo y para periodo de referencia\n')
    
    for (i in 1:dim(stations)[1]){
      
      for (month in months){
        
        ind <- which(data$station == stations$STAID[i] 
                     & format(data$Date, '%m') == month)
        
        ind_ref <- which(data_ref$station == stations$STAID[i] 
                         & format(data_ref$Date, '%m') == month)
        
        mm_aux <- month.abb[as.integer(month)]
        ks <- ks.test(data[ind, pred], data[ind, pred_proy])
        
        ks_df[i, paste0(mm_aux, '.KS')] <- ks$statistic
        ks_df[i, paste0(mm_aux, '.KSp')] <- ks$p.value
        
        ks_ref <- ks.test(data_ref[ind_ref, pred], data_ref[ind_ref, pred_proy])
        
        ks_df[i, paste0(mm_aux, '.KS_ref')] <- ks_ref$statistic
        ks_df[i, paste0(mm_aux, '.KSp_ref')] <- ks_ref$p.value
      }
    }
    
    for (i in 1:dim(stations)[1]){
      
      for (month in c(1,2)){
        
        ind <- which(data$station == stations$STAID[i] 
                     & data$month.aux == month)
        
        ind_ref <- which(data_ref$station == stations$STAID[i] 
                         & data$month.aux == month)
        
        if (month == 1){
          mm_aux <- '15jun.15jul'
        }else if(month == 2){
          mm_aux <- '16jul.15aug'
        }
        
        ks <- ks.test(data[ind, pred], data[ind, pred_proy])
        
        ks_df[i, paste0(mm_aux, '.KS')] <- ks$statistic
        ks_df[i, paste0(mm_aux, '.KSp')] <- ks$p.value
        
        ks_ref <- ks.test(data_ref[ind_ref, pred], data_ref[ind_ref, pred_proy])
        
        ks_df[i, paste0(mm_aux, '.KS_ref')] <- ks_ref$statistic
        ks_df[i, paste0(mm_aux, '.KSp_ref')] <- ks_ref$p.value
      }
    }
    
  }else if (type == 'period'){ # solo para q0.95
    
    cat('Cálculo según', type, '\n')
    
    ks_df <- data.frame(
      stations = stations$STAID,
      NAME2 = stations$NAME2
    )
    
    for (i in 1:dim(stations)[1]){
      
      for (name in names(periods)){
      
        p <- periods[[name]]
        
        ind_p <- which(data$station[p] == stations$STAID[i])
        ks_p <- ks.test(data[[pred]][p][ind_p], 
                         data[[pred_proy]][p][ind_p])
        
        ks_df[i, paste0(name,'.KS')] <- ks_p$statistic
        ks_df[i, paste0(name,'.KSp')] <- ks_p$p.value
        
      }
      
    }
    
  }
  
  print(head(ks_df))
  return(ks_df)
}

months <- c('06', '07', '08')
ks_months <- ks_test_df(pred_q0.95_comp, pred_q0.95_comp_ref, type = 'month', 'pred_q0.95', 'pred_q0.95_proy')
ks_months_q0.90 <- ks_test_df(pred_q0.90_comp, pred_q0.90_comp_ref, type = 'month', 'pred_q0.90', 'pred_q0.90_proy')
ks_months_q0.75 <- ks_test_df(pred_q0.75_comp, pred_q0.75_comp_ref, type = 'month', 'pred_q0.75', 'pred_q0.75_proy')

# solo cuantil 0.95
periods <- list(`1960.1977` = p1, `1978.1995` = p2, `1996.2014` = p3, 
                `1960.1980` = p_ref_ant, `1981.2010` = p_ref, `2011.2014` = p_ref_post)
ks_periods <- ks_test_df(pred_q0.95_comp, pred_q0.95_comp_ref, type = 'period', 'pred_q0.95', 'pred_q0.95_proy')

periods <- list(`1981.2010` = p_ref)
ks_periods_q0.90 <- ks_test_df(pred_q0.90_comp, pred_q0.90_comp_ref, type = 'period', 'pred_q0.90', 'pred_q0.90_proy')
ks_periods_q0.75 <- ks_test_df(pred_q0.75_comp, pred_q0.75_comp_ref, type = 'period', 'pred_q0.75', 'pred_q0.75_proy')



# ks_months <- data.frame(
#   stations = stations$STAID,
#   NAME2 = stations$NAME2
# )
# 
# for (i in 1:dim(stations)[1]){
#   #cat('Estación ', i, '\n')
#   for (month in c('06', '07', '08')){
#     cat('mes', month, '\n')
#     ind <- which(pred_q0.95_comp$station == stations$STAID[i] 
#                  & format(pred_q0.95_comp$Date, '%m') == month)
#     
#     ind_ref <- which(pred_q0.95_comp_ref$station == stations$STAID[i] 
#                  & format(pred_q0.95_comp_ref$Date, '%m') == month)
#     
#     cat('ind: ', length(ind), '\tind_ref: ', length(ind_ref), '\n')
#     
#     mm_aux <- month.abb[as.integer(month)]
#     ks <- ks.test(pred_q0.95_comp$pred_q0.95[ind], pred_q0.95_comp$pred_q0.95_proy[ind])
#     
#     ks_months[i, paste0(mm_aux, '.KS')] <- ks$statistic
#     ks_months[i, paste0(mm_aux, '.KSp')] <- ks$p.value
#     
#     ks_ref <- ks.test(pred_q0.95_comp_ref$pred_q0.95[ind_ref], pred_q0.95_comp_ref$pred_q0.95_proy[ind_ref])
#     
#     ks_months[i, paste0(mm_aux, '.KS_ref')] <- ks_ref$statistic
#     ks_months[i, paste0(mm_aux, '.KSp_ref')] <- ks_ref$p.value
#   }
# }
# 
source('mapa_Spain.R')
# 
# jun <- spain_points(ks_months$Jun.KSp, stations, 'KS Test June', 'p-value')
# jul <- spain_points(ks_months$Jul.KSp, stations, 'KS Test July', 'p-value')
# aug <- spain_points(ks_months$Aug.KSp, stations, 'KS Test August', 'p-value')
# 
# p_values_month <- ggpubr::ggarrange(jun, jul, aug, nrow = 1, ncol = 3,
#                   common.legend = T, legend = 'bottom')
# 
# ggsave(
#   filename = "Proyecciones/p_values_month.png", 
#   plot = p_values_month, 
#   width = 12,
#   height = 5,     
#   dpi = 300       
# )

#cuantil 0.95
jun_ref1 <- spain_points(ks_months$Jun.KSp_ref, stations, 'KS Test June Q0.95', 'p-values')
jul_ref1 <- spain_points(ks_months$Jul.KSp_ref, stations, 'KS Test July Q0.95', 'p-values')
aug_ref1 <- spain_points(ks_months$Aug.KSp_ref, stations, 'KS Test August Q0.95', 'p-values')

p_values_month_ref <- ggpubr::ggarrange(jun_ref1, jul_ref1, aug_ref1, nrow = 1, ncol = 3,
                                    common.legend = T, legend = 'bottom')

ggsave(
  filename = "Proyecciones/p_values_month_ref.png", 
  plot = p_values_month_ref, 
  width = 12,
  height = 5,     
  dpi = 300       
)

#cuantil 0.90
jun_ref2 <- spain_points(ks_months_q0.90$Jun.KSp_ref, stations, 'KS Test June Q0.90', 'p-values')
jul_ref2 <- spain_points(ks_months_q0.90$Jul.KSp_ref, stations, 'KS Test July Q0.90', 'p-values')
aug_ref2 <- spain_points(ks_months_q0.90$Aug.KSp_ref, stations, 'KS Test August Q0.90', 'p-values')

p_values_month_ref <- ggpubr::ggarrange(jun_ref2, jul_ref2, aug_ref2, nrow = 1, ncol = 3,
                                        common.legend = T, legend = 'bottom')

ggsave(
  filename = "Proyecciones/p_values_month_ref_q0.90.png", 
  plot = p_values_month_ref, 
  width = 12,
  height = 5,     
  dpi = 300       
)

#cuantil 0.75
jun_ref3 <- spain_points(ks_months_q0.75$Jun.KSp_ref, stations, 'KS Test June Q0.75', 'p-values')
jul_ref3 <- spain_points(ks_months_q0.75$Jul.KSp_ref, stations, 'KS Test July Q0.75', 'p-values')
aug_ref3 <- spain_points(ks_months_q0.75$Aug.KSp_ref, stations, 'KS Test August Q0.75', 'p-values')

p_values_month_ref <- ggpubr::ggarrange(jun_ref3, jul_ref3, aug_ref3, nrow = 1, ncol = 3,
                                        common.legend = T, legend = 'bottom')

ggsave(
  filename = "Proyecciones/p_values_month_ref_q0.75.png", 
  plot = p_values_month_ref, 
  width = 12,
  height = 5,     
  dpi = 300       
)


# all together
p_values_month_ref <- ggpubr::ggarrange(jun_ref1, jul_ref1, aug_ref1,
                                        jun_ref2, jul_ref2, aug_ref2,
                                        jun_ref3, jul_ref3, aug_ref3, nrow = 3, ncol = 3,
                                        common.legend = T, legend = 'bottom')

ggsave(
  filename = "Proyecciones/p_values_month_ref_all.png", 
  plot = p_values_month_ref, 
  width = 12,
  height = 5*3,     
  dpi = 300       
)

# KS POR PERIODOS DE AÑOS 1960-2014
# ks_periods <- data.frame(
#   stations = stations$STAID,
#   NAME2 = stations$NAME2
# )
# 
# for (i in 1:dim(stations)[1]){
#   cat('Estación ', i, '\n')
#   ind_p1 <- which(pred_q0.95_comp$station[p1] == stations$STAID[i])
#   ind_p2 <- which(pred_q0.95_comp$station[p2] == stations$STAID[i])
#   ind_p3 <- which(pred_q0.95_comp$station[p3] == stations$STAID[i])
#   
#   ind_p_ref_post <- which(pred_q0.95_comp$station[p_ref_post] == stations$STAID[i])
#   ind_p_ref <- which(pred_q0.95_comp$station[p_ref] == stations$STAID[i])
#   ind_p_ref_ant <- which(pred_q0.95_comp$station[p_ref_ant] == stations$STAID[i])
#   
#   # cat('ind_p1: ', length(ind_p1), '\tind_p2: ', length(ind_p2), 
#   #     '\tind_p3: ', length(ind_p3), '\n')
#   
#   ks_p1 <- ks.test(pred_q0.95_comp$pred_q0.95[p1][ind_p1], 
#                    pred_q0.95_comp$pred_q0.95_proy[p1][ind_p1])
#   
#   ks_periods[i, 'p1.KS'] <- ks_p1$statistic
#   ks_periods[i, 'p1.KSp'] <- ks_p1$p.value
#   
#   ks_p2 <- ks.test(pred_q0.95_comp$pred_q0.95[p2][ind_p2], 
#                    pred_q0.95_comp$pred_q0.95_proy[p2][ind_p2])
#   
#   ks_periods[i, 'p2.KS'] <- ks_p2$statistic
#   ks_periods[i, 'p2.KSp'] <- ks_p2$p.value
#   
#   ks_p3 <- ks.test(pred_q0.95_comp$pred_q0.95[p3][ind_p3], 
#                    pred_q0.95_comp$pred_q0.95_proy[p3][ind_p3])
#   
#   ks_periods[i, 'p3.KS'] <- ks_p3$statistic
#   ks_periods[i, 'p3.KSp'] <- ks_p3$p.value
#   
#   ks_p_ref_post <- ks.test(pred_q0.95_comp$pred_q0.95[p_ref_post][ind_p_ref_post], 
#                    pred_q0.95_comp$pred_q0.95_proy[p_ref_post][ind_p_ref_post])
#   
#   ks_periods[i, 'p_ref_post.KS'] <- ks_p_ref_post$statistic
#   ks_periods[i, 'p_ref_post.KSp'] <- ks_p_ref_post$p.value
#   
#   ks_p_ref <- ks.test(pred_q0.95_comp$pred_q0.95[p_ref][ind_p_ref], 
#                    pred_q0.95_comp$pred_q0.95_proy[p_ref][ind_p_ref])
#   
#   ks_periods[i, 'p_ref.KS'] <- ks_p_ref$statistic
#   ks_periods[i, 'p_ref.KSp'] <- ks_p_ref$p.value
#   
#   ks_p_ref_ant <- ks.test(pred_q0.95_comp$pred_q0.95[p_ref_ant][ind_p_ref_ant], 
#                    pred_q0.95_comp$pred_q0.95_proy[p_ref_ant][ind_p_ref_ant])
#   
#   ks_periods[i, 'p_ref_ant.KS'] <- ks_p_ref_ant$statistic
#   ks_periods[i, 'p_ref_ant.KSp'] <- ks_p_ref_ant$p.value
# 
# }
# 
# source('mapa_Spain.R')
# 
ks_p1 <- spain_points(ks_periods$`1960.1977.KSp`, stations, 'KS Test 1960-1977', 'p-value')
ks_p2 <- spain_points(ks_periods$`1978.1995.KSp`, stations, 'KS Test 1978-1995', 'p-value')
ks_p3 <- spain_points(ks_periods$`1996.2014.KSp`, stations, 'KS Test 1996-2014', 'p-value')
ks_p_ref_ant <- spain_points(ks_periods$`1960.1980.KSp`, stations, 'KS Test 1960-1980', 'p-value')
ks_p_ref <- spain_points(ks_periods$`1981.2010.KSp`, stations, 'KS Test 1981-2010', 'p-value')
ks_p_ref_post <- spain_points(ks_periods$`2011.2014.KSp`, stations, 'KS Test 2011-2014', 'p-value')

p_values_periods <- ggpubr::ggarrange(ks_p1, ks_p2, ks_p3, 
                                      ks_p_ref_ant, ks_p_ref, ks_p_ref_post, 
                                      nrow = 2, ncol = 3,
                                      common.legend = T, legend = 'bottom')

ggsave(
  filename = "Proyecciones/p_values_periods.png", 
  plot = p_values_periods, 
  width = 12,
  height = 5*2,     
  dpi = 300       
)


# 3 cuantiles en periodo de referencia
ks_p_ref_q0.75 <- spain_points(ks_periods_q0.75$`1981.2010.KSp`, stations, 'KS Test 1981-2010 Q0.75', 'p-value')
ks_p_ref_q0.90 <- spain_points(ks_periods_q0.90$`1981.2010.KSp`, stations, 'KS Test 1981-2010 Q0.90', 'p-value')
ks_p_ref <- spain_points(ks_periods$`1981.2010.KSp`, stations, 'KS Test 1981-2010 Q0.95', 'p-value')

p_values_ref <- ggpubr::ggarrange(ks_p_ref_q0.75, ks_p_ref_q0.90, ks_p_ref, 
                                  nrow = 1, ncol = 3,
                                  common.legend = T, legend = 'bottom')

ggsave(
  filename = "Proyecciones/p_values_ref.png", 
  plot = p_values_ref, 
  width = 12,
  height = 5,     
  dpi = 300       
)

#----MEDIAS ANUALES----
medias_anuales <- function(data, station){
  ind <- which(data$station == stations$STAID[station])
  name <- stations$NAME2[station]
  
  aux <- data[ind, ]
  
  mu_era5 <- tapply(aux$pred_q0.95, year(aux$Date), mean)
  mu_cmip6 <- tapply(aux$pred_q0.95_proy, year(aux$Date), mean)
  
  t <- unique(year(aux$Date))
  plot(t, mu_era5, type = 'l',
       main = paste('Medias anuales', name),
       xlab = 't', ylab = 'ºC',
       ylim = c(min(c(mu_era5, mu_cmip6)), max(c(mu_era5, mu_cmip6))))
  lines(t, mu_cmip6, col = 'red')
  abline(lm(mu_era5 ~ t))
  abline(lm(mu_cmip6 ~ t), col = 'red')
  legend("bottomright", legend = c("ERA5", "CMIP6"),
         col = c("black", "red"), lwd = 2)
}

png('Proyecciones/medias_anuales.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
for (i in 1:dim(stations)[1]){
  medias_anuales(pred_q0.95_comp, i)
}
dev.off()

png('Proyecciones/medias_anuales_ref.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
for (i in 1:dim(stations)[1]){
  medias_anuales(pred_q0.95_comp_ref, i)
}
dev.off()

#----PREDICCIONES COMPLETAS (UTILIZAR TODA CADENA)----
rm(list = ls())
library(coda)
library(lubridate)
load('data_q0.95/data.RData')
load('data_q0.95/proyecciones.RData')
orden <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/orden.rds")

source('05_pred.completa.R')

pred.q0.95.23jun <- pred.fecha(v_q0.95,
                              mod_q0.95_bay,
                              vars,
                              1981,
                              2010,
                             paste(1:31),
                              '7')
colnames(pred.q0.95.23jun) <- stations$NAME2

pred.q0.95.23jun.proy <- pred.fecha(v_q0.95_proy,
                              mod_q0.95_bay,
                              vars,
                              1981,
                              2010,
                              paste(1:31),
                              '7')
colnames(pred.q0.95.23jun.proy) <- stations$NAME2

IC.q0.95 <- IC(pred.q0.95.23jun)
IC.q0.95.proy <- IC(pred.q0.95.23jun.proy)

library(ggplot2)
library(reshape2)

fig.IC <- function(IC, IC.proy){
  df1 <- data.frame(t(IC[, orden]))
  df1$estacion <- rownames(df1)
  colnames(df1) <- c("low", "high", "estacion")
  df1$tipo <- "IC ERA5"
  
  df2 <- data.frame(t(IC.proy[, orden]))
  df2$estacion <- rownames(df2)
  colnames(df2) <- c("low", "high", "estacion")
  df2$tipo <- "IC MPI"
  
  # Combino
  df_all <- rbind(df1, df2)
  df_all$estacion <- factor(df_all$estacion, levels = orden)
  # Grafico
  ggplot(df_all, aes(x = estacion, ymin = low, ymax = high, color = tipo)) +
    geom_errorbar(position = position_dodge(width = 0.5), width = 0.3, size = 1) +
    labs(title = "Intervalos de confianza por estación",
         x = "Estación", y = "Valor") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

fig.IC(IC.q0.95, IC.q0.95.proy)
