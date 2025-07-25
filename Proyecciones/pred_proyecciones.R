# ajuste del modelo a los datos de proyecciones
rm(list = ls())
load('predictions.RData')
load('proyecciones.RData')

elev_sc <- scale(stations_dist$HGHT)
dist_sc <- scale(stations_dist$DIST)
pred_q0.95_proy <- predictions(vars, betas_q0.95, v_q0.95_proy, cuantil = 0.95)

fechas_cmip6 <- c(min(df_final_proy$Date), max(df_final_proy$Date))

load('data.RData')
pred_q0.95_era5 <- pred_q0.95[which(pred_q0.95$Date >= fechas_cmip6[1] & pred_q0.95$Date <= fechas_cmip6[2]), ]

pred_q0.95_comp <- cbind(pred_q0.95_era5, pred_q0.95_proy)

png('comp_qqplot.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
for (i in 1:dim(stations)[1]){
  ind <- which(pred_q0.95_comp$station == stations$STAID[i])
  name <- stations$NAME2[i]
  
  qqplot(pred_q0.95_comp$pred_q0.95[ind], pred_q0.95_comp$pred_q0.95_proy[ind],
         main = paste("QQ Plot: ERA5 vs CMIP6 (,", name,")"),
         xlab = "ERA5",
         ylab = "CMIP6")
  abline(0, 1, col = "red")
  
  
}
dev.off()

png('comp_dens.png', width = 2000*3/3, height = 2200*3/2, res = 150)
par(mfrow=c(10,4))
for (i in 1:dim(stations)[1]){
  ind <- which(pred_q0.95_comp$station == stations$STAID[i])
  name <- stations$NAME2[i]
  
  dens1 <- density(pred_q0.95_comp$pred_q0.95[ind], 
                   from = min(pred_q0.95_comp$pred_q0.95[ind]),
                   to   = max(pred_q0.95_comp$pred_q0.95[ind]))
  
  dens2 <- density(pred_q0.95_comp$pred_q0.95_proy[ind], 
                   from = min(pred_q0.95_comp$pred_q0.95_proy[ind]),
                   to   = max(pred_q0.95_comp$pred_q0.95_proy[ind]))
  
  plot(dens1, col = "blue", lwd = 2, 
       main = paste('Dens. ERA5 vs CMIP6 (', name, ')'))
  lines(dens2, col = "red", lwd = 2)
  legend("topright", legend = c("ERA5", "CMIP6"),
         col = c("blue", "red"), lwd = 2)
  
}
dev.off()
