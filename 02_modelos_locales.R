# Ajuste de modelos locales, para comparar R1
# Estos modelos también sirven como punto de partida 
# de los valores iniciales de los modelos bayesianos

rm(list = setdiff(ls(), c('df_final',
                          'errores_s_q0.95',
                          'errores_p_q0.95',
                          'stations',
                          'stations_dist',
                          'modelos_proyecto_q0.95')))

# load('data.RData')
library(quantreg)

# variables escogidas tras realizar comparación
vars <- errores_s_q0.95[[2]]$vars
vars <- c(vars[c(1, 2)], sort(vars[3:length(vars)]))

#cuantiles distintos al 95
vars_q0.90 <- unlist(errores_q0.90$vars_pearson[1])
vars_q0.90 <- c(vars_q0.90[c(1, 2)], sort(vars_q0.90[3:length(vars_q0.90)]))
vars_q0.75 <- unlist(errores_q0.75$vars_spearman[1])
vars_q0.75 <- c(vars_q0.75[c(1, 2)], sort(vars_q0.75[3:length(vars_q0.75)]))

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

#----CONJUNTO DE DATOS PARA JUSTAR TODOS MODELOS----

# Crear una lista para ir almacenando las columnas
columnas_df_sc <- function(vars_finales){
  columnas <- list()
  escalado_info <- list() 
  
  for (var in vars_finales) {
    if (grepl("^poly\\(", var)) {
      # Extraer el nombre real de la variable dentro del poly()
      nombre_var <- sub("^poly\\(([^,]+),.*", "\\1", var)
      
      # Calcular los términos polinómicos (lineal y cuadrático)
      poly_cols <- scale(poly(df_final[[nombre_var]], degree = 2))
      
      # Asignar nombres a las columnas resultantes
      colnames(poly_cols) <- c(nombre_var, paste0("I(", nombre_var, "^2)"))
      
      # Añadir al listado de columnas
      columnas[[length(columnas) + 1]] <- as.data.frame(poly_cols)
      
      escalado_info[[nombre_var]] <- list(
        center = attr(poly_cols, "scaled:center"),
        scale = attr(poly_cols, "scaled:scale")
      )
      
    } else {
      if (var == 's.1' || var == 'c.1'){
        scaled_var <- scale(df_final[var])
        columnas[[length(columnas) + 1]] <- scaled_var
        
        escalado_info[[var]] <- list(
          center = attr(scaled_var, "scaled:center"),
          scale = attr(scaled_var, "scaled:scale")
        )
      }else{
        # Añadir la variable directamente desde df_final
        scaled_var <- scale(df_final[var])
        columnas[[length(columnas) + 1]] <- scaled_var
        
        escalado_info[[var]] <- list(
          center = attr(scaled_var, "scaled:center"),
          scale = attr(scaled_var, "scaled:scale")
        )
      }
    }
  }
  
  return(list(
    columnas = columnas,
    escalado_info = escalado_info
    ))
}

columnas <- columnas_df_sc(vars_finales) # q0.95
columnas_q0.90 <- columnas_df_sc(vars_finales_q0.90)
columnas_q0.75 <- columnas_df_sc(vars_finales_q0.75)

escalado_info <- columnas_df_sc(vars_finales)[['escalado_info']]
escalado_info_q0.90 <- columnas_df_sc(vars_finales_q0.90)[['escalado_info']]
escalado_info_q0.75 <- columnas_df_sc(vars_finales_q0.75)[['escalado_info']]

# Combinar todas las columnas en un único dataframe
df_ajuste <- function(columnas, vars, df_base){
  v <- do.call(cbind, columnas)
  v <- v[, vars]
  v$elev <- as.numeric(df_base$elev)
  v$dist <- as.numeric(df_base$dist)
  v$Date <- df_base$Date
  v$station <- as.integer(df_base$station)
  v$Y <- df_base$Y
  v <- v[, c((ncol(v)-2):ncol(v), 1:(ncol(v)-3))]
  
  return(v)
}

v_q0.95 <- df_ajuste(columnas$columnas, vars, df_final)
v_q0.90 <- df_ajuste(columnas_q0.90$columnas, vars_q0.90, df_final)
v_q0.75 <- df_ajuste(columnas_q0.75$columnas, vars_q0.75, df_final)

# save(Y, df_final, stations, stations_dist, 
#      modelos_proyecto_q0.95, modelos_proyecto_q0.95_AIC, 
#      vars, v_q0.95, file = 'data.RData')

save(Y, df_final, stations, stations_dist,
     modelos_proyecto_q0.90, escalado_info_q0.90, 
     vars_q0.90, v_q0.90, file = 'data_q0.90/data.RData')
save(Y, df_final, stations, stations_dist,
     modelos_proyecto_q0.75, escalado_info_q0.75, 
     vars_q0.75, v_q0.75, file = 'data_q0.75/data.RData')

#----Modelos finales----

modelos_finales <- function(vars, tau, v){
  vars <- ifelse(grepl("^I\\(.*\\^2\\)$", vars),
                 paste0("`", vars, "`"),
                 vars)
  formula <- as.formula(paste('Y ~', paste(vars, collapse = '+')))
  
  vars <- gsub('`', '', vars)
  
  modelos_finales_df <- matrix(NA, nrow=dim(stations)[1], ncol = length(vars) + 4)
  modelos_finales_df <- as.data.frame(modelos_finales_df)
  colnames(modelos_finales_df) <- c('stations', 'NAME2', 'intercept', vars, 'R1')
  modelos_finales_df$stations <- stations$STAID
  modelos_finales_df$NAME2 <- stations$NAME2
  
  for(i in 1:dim(stations)[1]){
    cat(stations$NAME2[i], '\n')
    
    ind <- which(v$station == stations$STAID[i])
    cat(ind[c(1, 2)], '\n')
    
    mod <- rq(formula = formula, tau = tau, data = v, subset = ind)
    mod_nulo <- rq(formula = Y ~ 1, tau = tau, data = v, subset = ind)
    
    cat('Intercepto:', coef(mod)[1], '\n')
    
    R1 <- 1 - mod$rho / mod_nulo$rho
    cat('R1: ', R1, '\n')
    
    modelos_finales_df[i, 3:(2 + length(vars) + 1)]<-c(coef(mod))
    modelos_finales_df[i, 'R1'] <- R1
    
  }
  
  return(list(
    modelos = modelos_finales_df,
    formula = formula))
}

modelos_finales_q0.95 <- modelos_finales(vars, 0.95, v_q0.95)[["modelos"]]
formula <- modelos_finales(vars, 0.95, v_q0.95)[["formula"]]
modelos_finales_q0.90 <- modelos_finales(vars_q0.90, 0.90, v_q0.90)[["modelos"]]
formula_q0.90 <- modelos_finales(vars_q0.90, 0.90, v_q0.90)[["formula"]]
modelos_finales_q0.75 <- modelos_finales(vars_q0.75, 0.75, v_q0.75)[["modelos"]]
formula_q0.75 <- modelos_finales(vars_q0.75, 0.75, v_q0.75)[["formula"]]

# save(Y, df_final, stations, stations_dist, 
#      modelos_proyecto_q0.95, modelos_proyecto_q0.95_AIC, 
#      vars, v_q0.95, 
#      modelos_finales_q0.95,
#      formula,
#      file = 'data.RData')
save(Y, df_final, stations, stations_dist,
     modelos_proyecto_q0.90, 
     vars_q0.90, v_q0.90,
     modelos_finales_q0.90,
     escalado_info_q0.90, 
     formula_q0.90,
     file = 'data_q0.90/data.RData')
save(Y, df_final, stations, stations_dist,
     modelos_proyecto_q0.75, 
     vars_q0.75, v_q0.75,
     modelos_finales_q0.75,
     escalado_info_q0.75, 
     formula_q0.75,
     file = 'data_q0.75/data.RData')
