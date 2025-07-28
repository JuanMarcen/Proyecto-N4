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

png('comp_qqplot_ref_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
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

png('comp_dens_ref_est.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
for (i in 1:dim(stations)[1]){
  ind <- which(pred_q0.95_comp_ref$station == stations$STAID[i])
  name <- stations$NAME2[i]
  
  dens1 <- density(pred_q0.95_comp_ref$pred_q0.95[ind], 
                   from = min(pred_q0.95_comp_ref$pred_q0.95[ind]),
                   to   = max(pred_q0.95_comp_ref$pred_q0.95[ind]))
  
  dens2 <- density(pred_q0.95_comp_ref$pred_q0.95_proy[ind], 
                   from = min(pred_q0.95_comp_ref$pred_q0.95_proy[ind]),
                   to   = max(pred_q0.95_comp_ref$pred_q0.95_proy[ind]))
  
  dens3 <- density(pred_q0.95_comp_ref$pred_q0.95_proy_est[ind], 
                   from = min(pred_q0.95_comp_ref$pred_q0.95_proy_est[ind]),
                   to   = max(pred_q0.95_comp_ref$pred_q0.95_proy_est[ind]))
  
  plot(dens1, col = "blue", lwd = 2, 
       main = paste0('Dens. ERA5 vs CMIP6 (', name, ')'),
       xlab = 'ºC')
  lines(dens2, col = "red", lwd = 2)
  lines(dens3, col = "darkgreen", lwd = 2)
  legend("topleft", legend = c("ERA5", "CMIP6", 'CMIP6 (est)'),
         col = c("blue", "red", 'darkgreen'), lwd = 2)
  
}
dev.off()


# KS TEST
ks_months <- data.frame(
  stations = stations$STAID,
  NAME2 = stations$NAME2
)

for (i in 1:dim(stations)[1]){
  for (month in c('06', '07', '08')){
    ind <- which(pred_q0.95_comp$station == stations$STAID[i] 
                 & format(pred_q0.95_comp$Date, '%m') == month)
    
    mm_aux <- month.abb[as.integer(month)]
    ks <- ks.test(pred_q0.95_comp$pred_q0.95[ind], pred_q0.95_comp$pred_q0.95_proy[ind])
    
    ks_months[i, paste0(mm_aux, '.KS')] <- ks$statistic
    ks_months[i, paste0(mm_aux, '.KSp')] <- ks$p.value
  }
}


