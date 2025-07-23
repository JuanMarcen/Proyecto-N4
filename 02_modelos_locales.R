# Ajuste de modelos locales, para comparar R1
# Estos modelos también sirven como punto de partida 
# de los valores iniciales de los modelos bayesianos

rm(list = setdiff(ls(), c('df_final',
                          'errores_s_q0.95',
                          'errores_p_q0.95',
                          'stations',
                          'stations_dist',
                          'modelos_proyecto_q0.95')))
setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/ProyectoN4')
# load('data.RData')
library(quantreg)

# variables escogidas tras realizar comparación
vars <- errores_s_q0.95[[2]]$vars
vars <- c(vars[c(1, 2)], sort(vars[3:length(vars)]))

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
# esta fórmula permite observar cuales tienen valor poly
# (aunque luego no se usa, porque se mantienen los nombres originales)

#----CONJUNTO DE DATOS PARA JUSTAR TODOS MODELOS----

# Crear una lista para ir almacenando las columnas
columnas <- list()

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
  } else {
    if (var == 's.1' || var == 'c.1'){
      columnas[[length(columnas) + 1]] <- scale(df_final[var])
    }else{
      # Añadir la variable directamente desde df_final
      columnas[[length(columnas) + 1]] <- scale(df_final[var])
    }
  }
}

# Combinar todas las columnas en un único dataframe
v_q0.95 <- do.call(cbind, columnas)
v_q0.95 <- v_q0.95[, vars]
v_q0.95$elev <- as.numeric(df_final$elev)
v_q0.95$dist <- as.numeric(df_final$dist)
v_q0.95$Date <- df_final$Date
v_q0.95$station <- as.integer(df_final$station)
v_q0.95$Y <- df_final$Y
v_q0.95 <- v_q0.95[, c((ncol(v_q0.95)-2):ncol(v_q0.95), 1:(ncol(v_q0.95)-3))]

# save(Y, df_final, stations, stations_dist, 
#      modelos_proyecto_q0.95, modelos_proyecto_q0.95_AIC, 
#      vars, v_q0.95, file = 'data.RData')

#----Modelos finales----
vars <- ifelse(grepl("^I\\(.*\\^2\\)$", vars),
       paste0("`", vars, "`"),
       vars)
formula <- as.formula(paste('Y ~', paste(vars, collapse = '+')))

vars <- gsub('`', '', vars)

modelos_finales_q0.95 <- matrix(NA, nrow=dim(stations)[1], ncol = length(vars) + 4)
modelos_finales_q0.95 <- as.data.frame(modelos_finales_q0.95)
colnames(modelos_finales_q0.95) <- c('stations', 'NAME2', 'intercept', vars, 'R1')
modelos_finales_q0.95$stations <- stations$STAID
modelos_finales_q0.95$NAME2 <- stations$NAME2

for(i in 1:dim(stations)[1]){
  cat(stations$NAME2[i], '\n')
  
  ind <- which(v_q0.95$station == stations$STAID[i])
  cat(ind[c(1, 2)], '\n')
  
  mod <- rq(formula = formula, tau = 0.95, data = v_q0.95, subset = ind)
  mod_nulo <- rq(formula = Y ~ 1, tau = 0.95, data = v_q0.95, subset = ind)
  
  cat('Intercepto:', coef(mod)[1], '\n')
  
  R1 <- 1 - mod$rho / mod_nulo$rho
  cat('R1: ', R1, '\n')
  
  modelos_finales_q0.95[i, 3:(2 + length(vars) + 1)]<-c(coef(mod))
  modelos_finales_q0.95[i, 'R1'] <- R1
  
}

# save(Y, df_final, stations, stations_dist, 
#      modelos_proyecto_q0.95, modelos_proyecto_q0.95_AIC, 
#      vars, v_q0.95, 
#      modelos_finales_q0.95,
#      formula,
#      file = 'data.RData')
