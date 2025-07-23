rm(list=ls())
setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/ProyectoN4')

stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")
stations_dist <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations_dist.rds")

library(quantreg)

#----Creacion Dataframe----
library(zoo)
df <- cbind(Y, X[,-c(1,2,3)])
colnames(df)[3] <- 'Y'
df$Y <- df$Y / 10

# relleno nulos en la parte de Y
which(is.na(df))
for (col_name in names(df)[3:ncol(df)]){
  df[[col_name]] <- na.approx(df[[col_name]],rule=2)
}
which(is.na(df))

# elevación y distancia a costa
elev_sc <- scale(stations_dist$HGHT)
dist_sc <- scale(stations_dist$DIST)

# días l
l <- 151:243 # 31 mayo - 31 agosto
l <- rep(l,times=40*64)

# año t
library(lubridate)
t <- year(Y$Date) - 1960 + 1

# juntado 
df_conj_filled <- cbind(df, l, t, rep(elev_sc,each=93*64), rep(dist_sc,each=93*64))
colnames(df_conj_filled)[c(14, 15, 16, 17)]<-c('l', 't', 'elev', 'dist')

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

df_harm <- cbind(df_conj_filled, cs(df_conj_filled$l, 1))

# cálculo local de anomalías. Ref 1981-2010
for (i in 1:dim(stations)[1]){
  ind <- which(df_harm$station == stations$STAID[i])
  ind_jja <- which(df_harm$t[ind] >= 22 & df_harm$t[ind] <= 51 & df_harm$l[ind] >= 152)
  
  for (j in 4:13){
    var <- names(df_harm)[j]
    formula <- as.formula(paste(var, "~ s.1 + c.1"))
    mod <- lm(formula, data = df_harm[ind,], subset = ind_jja)
    preds <- predict(mod, newdata = data.frame(
      c.1 = df_harm$c.1[ind],
      s.1 = df_harm$s.1[ind]
    ))
    
    res <- df_harm[ind, var] - preds
    # print(sum(preds[ind_jja] - mod$fitted.values <= 1e-10))
    
    df_harm[ind,var] <- res 
  }
  
}

# cuadrado de anomalias
for (j in 4:13){
  var <- names(df_harm)[j]
  df_harm[,paste0('I(',var,'^2)')] <- df_harm[,var]^2
}

# lags
library(dplyr)
df_final <- df_harm %>% 
  group_by(station,t) %>% 
  mutate(across(c(3:12),
                .fns = ~lag(.),
                .names = '{.col}_lag')) %>%
  as.data.frame() %>% na.omit()

# cuadrado lags
for (j in 30:39){
  var <- names(df_final)[j]
  df_final[,paste0('I(',var,'^2)')] <- df_final[,var]^2
}

# save(Y, df_final, stations, stations_dist, file = 'data.RData')
#----Selección modelos locales, mediante BIC----
my_eBIC <- function(model, gamma, p){
  
  loss <- model$rho
  
  n <- length(model$fitted.values)
  k <- length(coef(model)) - 1
  
  
  eBIC <- 2 * n * log((1 / n) * loss) + k * log(n) + 2 * gamma * log(choose(p,k))
  #AIC <- - 2 * n * log (0.5*0.5) +  2*n*log((1/n)*loss) + 2*n + k*2
  
  return(eBIC)
  
}

step_rq_eBIC <- function(initial_model, data, scope, 
                       gamma = 0, 
                       trace = TRUE, harmonics = FALSE){
  
  # size of covariates set
  vars <- labels(terms(scope)) #formula terms
  #print(vars)
  p <- length(vars) 
  response <- as.character(scope[[2]]) #response variable
  #print(response)
  tau <- initial_model$tau
  #print(tau)
  #data <- get(as.character(initial_model$call$data))
  #print(dim(data))
  
  if (harmonics == TRUE){
    n <- length(vars) / 2
    vars <- sapply(1:n, function(i) paste0('c.',i,' + s.',i))
  }
  
  # Initial model
  formula_current <- initial_model$formula
  model_current <- initial_model
  best_eBIC <- my_eBIC(model_current, gamma, p)
  
  selected_vars <- attr(terms(formula(model_current)), "term.labels")
  #print(selected_vars)
  remaining_vars <- setdiff(vars, selected_vars)
  #print(remaining_vars)
  steps <- list()
  steps[[1]] <- list(formula = formula_current, eBIC = best_eBIC)
  
  improved <- TRUE
  
  while (improved && length(remaining_vars) > 0){
    improved <- FALSE
    eBICs <- c()
    models <- list()
    
    for (var in remaining_vars){
      formula_try <- as.formula(
        paste(response, '~', paste(c(selected_vars, var), collapse = '+'))
      )
      # Manejo del error en rq()
      model_try <- tryCatch(
        rq(formula_try, data = data, tau = tau),
        error = function(e) return(NULL)
      )
      
      # Si el modelo falló, pasa al siguiente
      if (is.null(model_try)) {
        if (trace) cat("Saltando variable (error en ajuste):", var, "\n")
        eBIC_val <- Inf
        eBICs <- c(eBICs, eBIC_val)
        models[[var]] <- list(model = NULL, formula = formula_try, eBIC = eBIC_val)
        next
      }
      
      eBIC_val <- my_eBIC(model_try, gamma, p)
      eBICs <- c(eBICs,eBIC_val)
      models[[var]] <- list(model = model_try, formula = formula_try, eBIC = eBIC_val)
    }
    
    min_eBIC <- min(eBICs)
    
    if(min_eBIC < best_eBIC){
      best_var <- remaining_vars[which.min(eBICs)]
      selected_vars <- c(selected_vars, best_var)
      remaining_vars <- setdiff(remaining_vars, best_var)
      
      model_current <- models[[best_var]]$model
      formula_current <- models[[best_var]]$formula
      best_eBIC <- min_eBIC
      
      #steps[[length(steps) + 1]] <- list(formula = formula_current, eBIC = best_eBIC)
      
      improved <- TRUE
      
      if (trace){
        cat("Added:", best_var, "| eBIC =", round(best_eBIC, 2), "\n")
      }
    }
    
  }
  
  # eBIC
  model_current$eBIC <- best_eBIC
  
  # R1
  model_null <- suppressWarnings(
    rq(paste(response, '~ 1'), data = data, tau = tau)
  )
  model_current$R1 <- 1 - model_current$rho / model_null$rho
  
  if (trace && harmonics == TRUE) {
    cat("\nFinal model:\n")
    print(formula_current)
    cat('R1_final: ', model_current$R1, '\n')
    cat('eBIC final: ', model_current$eBIC, '\n')
  }
  
  if (trace && harmonics == FALSE) {
    cat("\nFinal model:\n")
    print(formula_current)
    cat('R1 initial: ', initial_model$R1, '| R1_final: ', model_current$R1, '\n')
    cat('eBIC initial: ', initial_model$eBIC, '| eBIC final: ', model_current$eBIC, '\n')
  }
  
  return(model_current)
}

modelos_tau <- function(tau, formula, df){
  estaciones <- stations$STAID
  ind <- which(Y$station == estaciones[1])
  mod <- rq(formula, data=df[ind, ])
  coef <- coef(mod)[-1] #intercepto nos da igual SOLO LO QUEREMOS PARA LOS NOMBRES
  #data frame en el que guardamos los valores de los coeficientes escogidos
  modelos <- matrix(rep(0, length(estaciones) * (length(coef) + 1)), nrow=length(estaciones))
  modelos <- as.data.frame(modelos, row.names = stations$STAID)
  colnames(modelos) <- c(names(coef), 'R1')
  
  
  for (s in 1:length(estaciones)){
    cat('\n','Estación: ', stations$NAME2[s], '\n')
    
    ind <- which(df$station == estaciones[s])#subset
    
    model_null <- rq(Y~1, data=df[ind,], tau=tau)#modelo nulo
    
    # step de modelo nulo hasta  formula
    mod_step <- step_rq_eBIC(
      initial_model = model_null,
      data = df[ind, ],
      scope = formula
    )
    
    coef_sel <- mod_step$coefficients[-1] #intercepto no interesa
    
    for (i in 1:length(coef_sel)){
      name_sel <- names(coef_sel)[i]
      for (j in 1:length(coef)){
        name <- names(coef)[j]
        if (name_sel == name){
          modelos[s, j] = coef_sel[i]
        }
      }
    }
    
    # R1
    modelos$R1[s] <- mod_step$R1
    
  }
  
  if (sum(modelos$c.1 != 0) == length(estaciones) & sum(modelos$s.1 != 0) == length(estaciones)){
    cat('\n','TODOS ARMÓNICOS HAN SIDO SELECCIONADOS')
  }
  
  return(modelos)
}

formula <- as.formula(
  paste('Y ~ c.1 + s.1 +',
        paste(names(df_final)[c(4:13,20:ncol(df_final))], 
              collapse = '+')))

modelos_proyecto_q0.95 <- modelos_tau(0.95, formula, df_final)
#save(Y, df_final, stations, stations_dist, modelos_proyecto_q0.95, file = 'data.RData')

#----Selección covariables----
modelos_proyecto_q0.95$stations <- stations$STAID
modelos_proyecto_q0.95$NAME2 <- stations$NAME2
modelos_proyecto_q0.95 <- modelos_proyecto_q0.95[,c(ncol(modelos_proyecto_q0.95)-1, ncol(modelos_proyecto_q0.95), 1:(ncol(modelos_proyecto_q0.95)-2))]

met_ajuste <- function(mod, mod_nulo){
  return(1 - mod$rho / mod_nulo$rho)
}

contador <- function(x){
  sum(x != 0)
}

cont_coef_mod <- function(df,p){
  
  vars <- list()
  
  #grupos
  for (s in unique(df[['grupo']])){ 
    
    ind <- which(df[,'grupo'] == s)
    
    coef_no_ceros <- apply(df[ind, -c(1, 2, 3, 46)], 
                           MARGIN = 2, FUN = contador)
    
    c <- as.integer(p * dim(df[ind, ])[1])
    
    v <- names(which(coef_no_ceros > c))
    
    vars[[paste0('g', s)]] <- v
    
  }
  
  vars[['juntos']] <- unique(unlist(vars))
  
  return(vars[['juntos']])
}

R1_todos<-function(modelos,tau){
  R1<-rep(NA,length=dim(stations)[1])
  for (i in 1:dim(stations)[1]){
    vars<-names(modelos[i,-c(1,2)][which(modelos[i,-c(1,2)]!=0)])
    
    formula<-as.formula(paste('Y ~ ',paste(vars,collapse = '+')))
    
    ind<-which(df_final$station==stations$STAID[i])
    
    mod_nulo<-rq(Y~1,data=df_final,tau=tau,subset=ind)
    
    mod<-rq(formula,data=df_final,tau=tau,subset=ind)
    
    R1[i]<-met_ajuste(mod,mod_nulo)
  }
  
  return(R1)
}

error_modelo<-function(modelos, tau, k, p, metodo){
  #paso 1: clustering
  t_modelos <- t(modelos)
  
  t_modelos <- data.frame(
    t_modelos
  )
  colnames(t_modelos) <- stations$NAME2
  
  matrix <- as.matrix(t_modelos[5:44, ]) #quito R1 para el clustering y etiquetas
  matrix <- apply(matrix, 2, as.numeric)
  cor <- cor(matrix, method = metodo)
  
  clust <- cutree(hclust(dist(cor)), k = k)
  
  modelos$grupo <- clust
  # 44 columnas
  modelos <- modelos[, c(1, 2, 46, 3:45)]
  
  #paso 2 y 3
  vars <- cont_coef_mod(modelos, p)
  if (is.null(vars)) {
    stop("La función `cont_coef_mod` ha devuelto un vector vacío. No se puede construir una fórmula.")
  }
  
  #paso 4 y 5
  formula_juntos <- as.formula(paste('Y ~ ', paste(vars, collapse = '+')))
  
  R1_juntos <- rep(NA,dim(modelos)[1])
  for (i in 1:dim(modelos)[1]){
    ind <- which(df_final$station == stations$STAID[i])
    mod_nulo <- rq(Y ~ 1, data = df_final, subset = ind, tau = tau)
    mod <- rq(formula_juntos, data = df_final, subset = ind, tau = tau)
    R1_juntos[i] <- met_ajuste(mod, mod_nulo)
  }
  
  modelos$R1_juntos <- R1_juntos
  
  error <- 1 - R1_juntos / modelos$R1
  modelos$error <- error
  
  max_error <- max(modelos$error)
  mean_error <- mean(modelos$error)
  min_error <- min(modelos$error)
  
  ciudad <- modelos$NAME2[which.max(modelos$error)]
  
  n.vars <- length(vars)
  
  
  return(list(
    k = k,
    p = p,
    metodo = metodo,
    n.vars = n.vars,
    min_error = min_error,
    mean_error = mean_error,
    max_error = max_error,
    ciudad = ciudad,
    vars = vars
  ))
}

prop <- seq(80/100, 95/100, by=0.05)
k<-c(2, 3)

# pearson
errores_p_q0.95 <- vector("list", length(prop)*length(k))
i <- 1
for (kk in k){
  for (p in prop){
    errores_p_q0.95[[i]] <- error_modelo(modelos_proyecto_q0.95, tau = 0.95, k = kk, p = p, metodo = 'pearson')
    i <- i + 1
  }
}

ciudades_p_q0.95 <- sapply(errores_p_q0.95, function(x) x[[8]])
ciudades_p_q0.95

# spearman
errores_s_q0.95 <- vector("list", length(prop)*length(k))
i <- 1
for (kk in k){
  for (p in prop){
    errores_s_q0.95[[i]] <- error_modelo(modelos_proyecto_q0.95, tau = 0.95, k = kk, p = p, metodo = 'spearman')
    i <- i+1
  }
}

ciudades_s_q0.95 <- sapply(errores_s_q0.95, function(x) x[[8]])
ciudades_s_q0.95

# como data frames
# pearson
errores_df_p_q0.95 <- do.call(rbind, lapply(errores_p_q0.95, function(x) as.numeric(x[-length(x)]))) #quito vars, para mostrar resultado mejor
colnames(errores_df_p_q0.95) <- names(errores_p_q0.95[[1]])[-length(errores_p_q0.95[[1]])]  # Mantiene los nombres correctos
errores_df_p_q0.95 <- as.data.frame(errores_df_p_q0.95)
errores_df_p_q0.95$metodo <- rep('pearson', length = length(errores_p_q0.95))
errores_df_p_q0.95$ciudad <- ciudades_p_q0.95



#spearman
errores_df_s_q0.95 <- do.call(rbind, lapply(errores_s_q0.95, function(x) as.numeric(x[-length(x)]))) #quito vars, para mostrar resultado mejor
colnames(errores_df_s_q0.95) <- names(errores_s_q0.95[[1]])[-length(errores_s_q0.95[[1]])]  # Mantiene los nombres correctos
errores_df_s_q0.95 <- as.data.frame(errores_df_s_q0.95)
errores_df_s_q0.95$metodo <- rep('spearman', length=length(errores_s_q0.95))
errores_df_s_q0.95$ciudad <- ciudades_s_q0.95



# #----Mismo proceso pero con AIC, para ver si se obtienen mejores resultados----
# modelos_tau <- function(tau, formula, df, pen = 2){
#   estaciones <- stations$STAID
#   ind <- which(df$station == estaciones[1])
#   mod <- rq(formula, data=df[ind, ])
#   coef <- coef(mod)[-1] #intercepto nos da igual SOLO LO QUEREMOS PARA LOS NOMBRES
#   #data frame en el que guardamos los valores de los coeficientes escogidos
#   modelos <- matrix(rep(0, length(estaciones) * (length(coef) + 1)), nrow=length(estaciones))
#   modelos <- as.data.frame(modelos, row.names = stations$STAID)
#   colnames(modelos) <- c(names(coef), 'R1')
#   
#   
#   for (s in 1:length(estaciones)){
#     cat('\n','Estación: ', stations$NAME2[s], '\n')
#     
#     ind <- which(df$station == estaciones[s])#subset
#     
#     model_null <- rq(Y~1, data=df[ind,], tau=tau)#modelo nulo
#     
#     #añadir penalizacion
#     mod_step<-step(model_null, scope = formula, direction = 'forward',k=pen)# step de modelo nulo hasta nuestra formula
#     # mod_step <- step_rq_eBIC(
#     #   initial_model = model_null,
#     #   data = df[ind, ],
#     #   scope = formula
#     # )
#     
#     coef_sel <- mod_step$coefficients[-1] #intercepto no interesa
#     
#     for (i in 1:length(coef_sel)){
#       name_sel <- names(coef_sel)[i]
#       for (j in 1:length(coef)){
#         name <- names(coef)[j]
#         if (name_sel == name){
#           modelos[s, j] = coef_sel[i]
#         }
#       }
#     }
#     
#     # R1
#    # modelos$R1[s] <- mod_step$R1
#     
#   }
#   
#   if (sum(modelos$c.1 != 0) == length(estaciones) & sum(modelos$s.1 != 0) == length(estaciones)){
#     cat('\n','TODOS ARMÓNICOS HAN SIDO SELECCIONADOS')
#   }
#   
#   return(modelos)
# }
# 
# formula <- as.formula(
#   paste('Y ~ c.1 + s.1 +',
#         paste(names(df_final)[c(4:13,20:ncol(df_final))], 
#               collapse = '+')))
# 
# modelos_proyecto_q0.95_AIC <- modelos_tau(0.95, formula, df_final)
# modelos_proyecto_q0.95_AIC$stations <- stations$STAID
# modelos_proyecto_q0.95_AIC$NAME2 <- stations$NAME2
# modelos_proyecto_q0.95_AIC <- modelos_proyecto_q0.95_AIC[,c(ncol(modelos_proyecto_q0.95_AIC)-1, ncol(modelos_proyecto_q0.95_AIC), 1:(ncol(modelos_proyecto_q0.95_AIC)-2))]
# 
# R1<-R1_todos(modelos_proyecto_q0.95_AIC[,-ncol(modelos_proyecto_q0.95_AIC)], 0.95)
# modelos_proyecto_q0.95_AIC$R1<-R1
# 
# save(Y, df_final, stations, stations_dist, 
#      modelos_proyecto_q0.95, modelos_proyecto_q0.95_AIC , file = 'data.RData')
# 
# 
# # pearson
# prop <- seq(80/100, 95/100, by=0.05)
# k<-c(2, 3)
# errores_p_q0.95_AIC <- vector("list", length(prop)*length(k))
# i <- 1
# for (kk in k){
#   for (p in prop){
#     errores_p_q0.95_AIC[[i]] <- error_modelo(modelos_proyecto_q0.95_AIC, tau = 0.95, k = kk, p = p, metodo = 'pearson')
#     i <- i + 1
#   }
# }
# 
# ciudades_p_q0.95 <- sapply(errores_p_q0.95_AIC, function(x) x[[8]])
# ciudades_p_q0.95
# 
# # spearman
# errores_s_q0.95_AIC <- vector("list", length(prop)*length(k))
# i <- 1
# for (kk in k){
#   for (p in prop){
#     errores_s_q0.95_AIC[[i]] <- error_modelo(modelos_proyecto_q0.95_AIC, tau = 0.95, k = kk, p = p, metodo = 'spearman')
#     i <- i+1
#   }
# }
# 
# ciudades_s_q0.95 <- sapply(errores_s_q0.95_AIC, function(x) x[[8]])
# ciudades_s_q0.95
# 
# # como data frames para comparar de manera visual
# # pearson
# errores_df_p_q0.95_AIC <- do.call(rbind, lapply(errores_p_q0.95_AIC, function(x) as.numeric(x[-length(x)]))) #quito vars, para mostrar resultado mejor
# colnames(errores_df_p_q0.95_AIC) <- names(errores_p_q0.95_AIC[[1]])[-length(errores_p_q0.95_AIC[[1]])]  # Mantiene los nombres correctos
# errores_df_p_q0.95_AIC <- as.data.frame(errores_df_p_q0.95_AIC)
# errores_df_p_q0.95_AIC$metodo <- rep('pearson', length = length(errores_p_q0.95_AIC))
# errores_df_p_q0.95_AIC$ciudad <- ciudades_p_q0.95
# 
# #spearman
# errores_df_s_q0.95_AIC <- do.call(rbind, lapply(errores_s_q0.95_AIC, function(x) as.numeric(x[-length(x)]))) #quito vars, para mostrar resultado mejor
# colnames(errores_df_s_q0.95_AIC) <- names(errores_s_q0.95_AIC[[1]])[-length(errores_s_q0.95_AIC[[1]])]  # Mantiene los nombres correctos
# errores_df_s_q0.95_AIC <- as.data.frame(errores_df_s_q0.95_AIC)
# errores_df_s_q0.95_AIC$metodo <- rep('spearman', length=length(errores_s_q0.95_AIC))
# errores_df_s_q0.95_AIC$ciudad <- ciudades_s_q0.95
# 
# # guardado de los errores 
# save(Y, df_final, stations, stations_dist, 
#      modelos_proyecto_q0.95, modelos_proyecto_q0.95_AIC, 
#      errores_p_q0.95, errores_s_q0.95, file = 'data.RData')
