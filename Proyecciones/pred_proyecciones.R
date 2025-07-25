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


ind <- which(pred_q0.95_comp$station == 229)

qqplot(pred_q0.95_comp$pred_q0.95[ind], pred_q0.95_comp$pred_q0.95_proy[ind],
       main = "QQ Plot: ERA5 vs CMIP6 (Badajoz)",
       xlab = "ERA5",
       ylab = "CMIP6")
abline(0, 1, col = "red")

plot(density(pred_q0.95_comp$pred_q0.95, na.rm = TRUE), 
     col = "blue", lwd = 2, main = "Density Plot",
     xlab = "Valores")
lines(density(pred_q0.95_comp$pred_q0.95_proy, na.rm = TRUE), col = "red", lwd = 2)
legend("topright", legend = c("ERA5", "CMIP6"),
       col = c("blue", "red"), lwd = 2)
