# FUNCION PARA HACER PREDICCIONES COMPLETAS EN TODAS ESTACIONES EN UN DÍA CONCRETO
# rm(list = ls())

# load('data_q0.95/data.RData')
# load('data_q0.95/proyecciones.RData')
library(coda)
library(lubridate)

#23 de junio

pred.fecha <- function(data, modelo, vars, año1, año2, dia, mes){
  
  df.betas <- as.data.frame(modelo$p.params.samples)
  colnames(df.betas) <- gsub('`', '', colnames(df.betas))
  
  fechas <- unique(data$Date[year(data$Date) >= año1 &
                                  year(data$Date) <= año2 &
                                  day(data$Date) %in% dia &
                                  month(data$Date) == mes])
  
  pred_list <- vector("list", length(fechas))
  for (i in seq_along(fechas)){
    
    # cuad_vars <- gsub("^I\\((.*)\\^2\\)$", "\\1", vars[grepl("^I\\(.*\\^2\\)$", vars)])
    # norm_vars <- vars[!grepl("^I\\(.*\\^2\\)$", vars)]
    
    fecha <- fechas[i]
    ind <- which(data$Date == fecha)
    
    G <- data[ind, -c(1, 2, 3)]
    # ya tengo los datos como deseo
    
    
    # bi = bi + bi(s) --> lista de no. vars, 1000x40
    betas <- list()
    # intercepto
    betas[['intercept']] <- as.matrix(sweep(df.betas[, c(paste0('beta1(s', 1:40, ')'))], 1, df.betas$`(Intercept)`, FUN = '+'))
    betas[['elev']] <- as.matrix(df.betas[['elev']])
    betas[['dist']] <- as.matrix(df.betas[['dist']])
    for (k in 1:length(vars)){
      beta_fija <- df.betas[[vars[k]]]
      
      beta_esp <- df.betas[, c(paste0('beta', k + 1,'(s', 1:40, ')'))]
      
      betas[[vars[k]]] <- as.matrix(sweep(beta_esp, 1, beta_fija, FUN = '+'))
      
    }
    
    #prediccion
    pred <- betas[['intercept']]
    
    for (j in seq_along(vars)) {
      pred <- pred + sweep(betas[[vars[j]]], 2, t(G[[vars[j]]]), FUN = '*')
    }
    
    # elevacion y distancia
    pred <- pred + sweep(matrix(rep(betas[["elev"]], 40), ncol = 40), 2, G[['elev']], FUN = '*')
    pred <- pred + sweep(matrix(rep(betas[["dist"]], 40), ncol = 40), 2, G[['dist']], FUN = '*')
    
    pred_list[[i]] <- pred
    if (i %% 10 == 0 || i == 1) cat("Progreso:", i, "/", length(fechas), "\n")
  }
  pred_todas <- do.call(rbind, pred_list)
  
  
  return(pred_todas)
}

# pred.q0.95.6jun <- pred.fecha(v_q0.95,
#                               mod_q0.95_bay,
#                               vars,
#                               2002,
#                               2011,
#                               c('21', '22', '23', '24', '25'),
#                               '6')
# colnames(pred.q0.95.6jun) <- stations$STAID
# 
# pred.q0.95.6jun.proy <- pred.fecha(v_q0.95_proy,
#                               mod_q0.95_bay,
#                               vars,
#                               2002,
#                               2011,
#                               '23',
#                               '6')
# colnames(pred.q0.95.6jun.proy) <- stations$STAID
# 
# IC.q0.95 <- apply(pred.q0.95.6jun, 2, FUN = quantile, probs = c(0.025, 0.975))
# IC.q0.95.proy <- apply(pred.q0.95.6jun.proy, 2, FUN = quantile, probs = c(0.025, 0.975))
IC <- function(pred){
  index <- rep(1:1000, times = nrow(pred)/1000)
  suma <- rowsum(pred, group = index)
  media <- suma / (nrow(pred)/1000)
  
  IC <- apply(media, 2, FUN = quantile, probs = c(0.025, 0.975))
  return(IC)
}

# IC.q0.95 <- IC(pred.q0.95.6jun)
# IC.q0.95.proy <- IC(pred.q0.95.6jun.proy)
# library(ggplot2)
# library(reshape2)

# Supongo que ya tienes IC.q0.95 e IC.q0.95.proy como data.frames/matrices
# con filas = c("2.5%", "97.5%") y columnas = estaciones.

# Los paso a formato largo
# df1 <- data.frame(t(IC.q0.95))
# df1$estacion <- rownames(df1)
# colnames(df1) <- c("low", "high", "estacion")
# df1$tipo <- "IC.q0.95"
# 
# df2 <- data.frame(t(IC.q0.95.proy))
# df2$estacion <- rownames(df2)
# colnames(df2) <- c("low", "high", "estacion")
# df2$tipo <- "IC.q0.95.proy"
# 
# # Combino
# df_all <- rbind(df1, df2)
# 
# # Grafico
# ggplot(df_all, aes(x = estacion, ymin = low, ymax = high, color = tipo)) +
#   geom_errorbar(position = position_dodge(width = 0.5), width = 0.3, size = 1) +
#   labs(title = "Intervalos de confianza por estación",
#        x = "Estación", y = "Valor") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

