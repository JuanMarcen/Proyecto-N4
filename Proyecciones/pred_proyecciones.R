# ajuste del modelo a los datos de proyecciones
rm(list = ls())
load('predictions.RData')
load('proyecciones.RData')

# anomalias estandarizadas según la regla utilizada para ajustar el modelo
# (MPI - mean(ERA5)) / sd(ERA5)

elev_sc <- scale(stations_dist$HGHT)
dist_sc <- scale(stations_dist$DIST)
pred_q0.95_proy <- predictions(vars, betas_q0.95, v_q0.95_proy, cuantil = 0.95)
pred_q0.95_proy_est <- predictions(vars, betas_q0.95, v_q0.95_proy_est, cuantil = 0.95)

fechas_cmip6 <- c(min(df_final_proy$Date), max(df_final_proy$Date))

load('data.RData')
pred_q0.95_era5 <- pred_q0.95[which(pred_q0.95$Date >= fechas_cmip6[1] & pred_q0.95$Date <= fechas_cmip6[2]), ]

pred_q0.95_comp <- cbind(pred_q0.95_era5, pred_q0.95_proy, pred_q0.95_proy_est)
pred_q0.95_comp_ref <- pred_q0.95_comp[which(pred_q0.95_comp$Date >= '1981-06-01' &
                                               pred_q0.95_comp$Date <= '2010-08-31'), ]

p1 <- which(year(pred_q0.95_comp$Date) >= '1960'
            & year(pred_q0.95_comp$Date) <= '1977')
p2 <- which(year(pred_q0.95_comp$Date) >= '1978'
            & year(pred_q0.95_comp$Date) <= '1995')
p3 <- which(year(pred_q0.95_comp$Date) >= '1996')
p4 <- which(year(pred_q0.95_comp$Date) >= '2011')

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

png('Proyecciones/comp_dens.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est')
dev.off()

png('Proyecciones/comp_dens_ref.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp_ref, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est')
dev.off()

#por meses 
png('Proyecciones/dens_jun.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'months',
              month = '06')
dev.off()

png('Proyecciones/dens_jun_ref.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp_ref, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'months',
              month = '06')
dev.off()

png('Proyecciones/dens_jul.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'months',
              month = '07')
dev.off()

png('Proyecciones/dens_jul_ref.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp_ref, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'months',
              month = '07')
dev.off()

png('Proyecciones/dens_aug.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'months',
              month = '08')
dev.off()

png('Proyecciones/dens_aug_ref.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp_ref, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'months',
              month = '08')
dev.off()

#por periodos
png('Proyecciones/dens_p1.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'period',
              period = p1)
dev.off()

png('Proyecciones/dens_p2.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'period',
              period = p2)
dev.off()

png('Proyecciones/dens_p3.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
density_plots(pred_q0.95_comp, 
              'pred_q0.95', 
              'pred_q0.95_proy',
              'pred_q0.95_proy_est',
              type = 'period',
              period = p3)
dev.off()

#----QQPLOTS----
#convertir a FUNCION 
png('Proyecciones/comp_qqplot_ref_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
for (i in 1:dim(stations)[1]){
  ind <- which(pred_q0.95_comp_ref$station == stations$STAID[i])
  name <- stations$NAME2[i]
  
  qqplot(pred_q0.95_comp_ref$pred_q0.95[ind], pred_q0.95_comp_ref$pred_q0.95_proy_est[ind],
         main = paste0("QQ Plot: ERA5 vs CMIP6 (", name,")"),
         xlab = "ERA5 (ºC)",
         ylab = "CMIP6 (ºC)")
  abline(0, 1, col = "red")
  
  
}
dev.off()

#----KS TEST meses Y PERIODOS----
ks_months <- data.frame(
  stations = stations$STAID,
  NAME2 = stations$NAME2
)

for (i in 1:dim(stations)[1]){
  cat('Estación ', i, '\n')
  for (month in c('06', '07', '08')){
    cat('mes', month, '\n')
    ind <- which(pred_q0.95_comp$station == stations$STAID[i] 
                 & format(pred_q0.95_comp$Date, '%m') == month)
    
    ind_ref <- which(pred_q0.95_comp_ref$station == stations$STAID[i] 
                 & format(pred_q0.95_comp_ref$Date, '%m') == month)
    
    cat('ind: ', length(ind), '\tind_ref: ', length(ind_ref), '\n')
    
    mm_aux <- month.abb[as.integer(month)]
    ks <- ks.test(pred_q0.95_comp$pred_q0.95[ind], pred_q0.95_comp$pred_q0.95_proy[ind])
    
    ks_months[i, paste0(mm_aux, '.KS')] <- ks$statistic
    ks_months[i, paste0(mm_aux, '.KSp')] <- ks$p.value
    
    ks_ref <- ks.test(pred_q0.95_comp_ref$pred_q0.95[ind_ref], pred_q0.95_comp_ref$pred_q0.95_proy[ind_ref])
    
    ks_months[i, paste0(mm_aux, '.KS_ref')] <- ks_ref$statistic
    ks_months[i, paste0(mm_aux, '.KSp_ref')] <- ks_ref$p.value
  }
}

source('mapa_Spain.R')

jun <- spain_points(ks_months$Jun.KSp, stations, 'KS Test June', 'p-value')
jul <- spain_points(ks_months$Jul.KSp, stations, 'KS Test July', 'p-value')
aug <- spain_points(ks_months$Aug.KSp, stations, 'KS Test July', 'p-value')

p_values_month <- ggpubr::ggarrange(jun, jul, aug, nrow = 1, ncol = 3,
                  common.legend = T, legend = 'bottom')

ggsave(
  filename = "Proyecciones/p_values_month.png", 
  plot = p_values_month, 
  width = 12,
  height = 5,     
  dpi = 300       
)

jun_ref <- spain_points(ks_months$Jun.KSp_ref, stations, 'KS Test June', 'p-values')
jul_ref <- spain_points(ks_months$Jul.KSp_ref, stations, 'KS Test July', 'p-values')
aug_ref <- spain_points(ks_months$Aug.KSp_ref, stations, 'KS Test July', 'p-values')

p_values_month_ref <- ggpubr::ggarrange(jun_ref, jul_ref, aug_ref, nrow = 1, ncol = 3,
                                    common.legend = T, legend = 'bottom')

ggsave(
  filename = "Proyecciones/p_values_month_ref.png", 
  plot = p_values_month_ref, 
  width = 12,
  height = 5,     
  dpi = 300       
)

# KS POR PERIODOS DE AÑOS 1960-2014
ks_periods <- data.frame(
  stations = stations$STAID,
  NAME2 = stations$NAME2
)

for (i in 1:dim(stations)[1]){
  cat('Estación ', i, '\n')
  ind_p1 <- which(pred_q0.95_comp$station[p1] == stations$STAID[i])
  ind_p2 <- which(pred_q0.95_comp$station[p2] == stations$STAID[i])
  ind_p3 <- which(pred_q0.95_comp$station[p3] == stations$STAID[i])
  
  cat('ind_p1: ', length(ind_p1), '\tind_p2: ', length(ind_p2), 
      '\tind_p3: ', length(ind_p3), '\n')
  
  ks_p1 <- ks.test(pred_q0.95_comp$pred_q0.95[p1][ind_p1], 
                   pred_q0.95_comp$pred_q0.95_proy[p1][ind_p1])
  
  ks_periods[i, 'p1.KS'] <- ks_p1$statistic
  ks_periods[i, 'p1.KSp'] <- ks_p1$p.value
  
  ks_p2 <- ks.test(pred_q0.95_comp$pred_q0.95[p2][ind_p2], 
                   pred_q0.95_comp$pred_q0.95_proy[p2][ind_p2])
  
  ks_periods[i, 'p2.KS'] <- ks_p2$statistic
  ks_periods[i, 'p2.KSp'] <- ks_p2$p.value
  
  ks_p3 <- ks.test(pred_q0.95_comp$pred_q0.95[p3][ind_p3], 
                   pred_q0.95_comp$pred_q0.95_proy[p3][ind_p3])
  
  ks_periods[i, 'p3.KS'] <- ks_p3$statistic
  ks_periods[i, 'p3.KSp'] <- ks_p3$p.value

}

source('mapa_Spain.R')

ks_p1 <- spain_points(ks_periods$p1.KSp, stations, 'KS Test 1960-1977', 'p-value')
ks_p2 <- spain_points(ks_periods$p2.KSp, stations, 'KS Test 1979-1995', 'p-value')
ks_p3 <- spain_points(ks_periods$p3.KSp, stations, 'KS Test 1996-2014', 'p-value')

p_values_periods <- ggpubr::ggarrange(ks_p1, ks_p2, ks_p3, nrow = 1, ncol = 3,
                                    common.legend = T, legend = 'bottom')

ggsave(
  filename = "Proyecciones/p_values_periods.png", 
  plot = p_values_periods, 
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
