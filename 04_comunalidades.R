# estudio de comunalidades en las variables escogidas
rm(list = ls())

load('data_q0.95/data.RData')
load('data_q0.90/data.RData')
load('data_q0.75/data.RData')
vars_q0.95 <- vars

load('predictions.RData')
rm(list = setdiff(ls(), c(
  'stations',
  'vars_q0.95', 'vars_q0.90', 'vars_q0.75',
  'betas_q0.95', 'betas_q0.90', 'betas_q0.75'
)))

# variables comunes en 3 modelos
# transformar a poly
vars_poly <- function(vars){
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
  
  return(new_vars)
}

#transformar a normal
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

vars_q0.95_poly <- vars_poly(vars_q0.95)
vars_q0.90_poly <- vars_poly(vars_q0.90)
vars_q0.75_poly <- vars_poly(vars_q0.75)

common <- Reduce(intersect, list(vars_q0.95_poly,
                                 vars_q0.90_poly,
                                 vars_q0.75_poly))
common <- c('intercept', common[1:2], sort(common[3:length(common)]))

#transformación de nombres columna betas
colnames(betas_q0.95) <- vars_poly(colnames(betas_q0.95))
colnames(betas_q0.90) <- vars_poly(colnames(betas_q0.90))
colnames(betas_q0.75) <- vars_poly(colnames(betas_q0.75))

df_total <- rbind(
  round(betas_q0.95[1, common], 3),
  round(betas_q0.90[1, common], 3),
  round(betas_q0.75[1, common], 3)
)
rownames(df_total) <- c('0.95', '0.90', '0.75')
df_total <- t(df_total)

#escribir en Latex
library(xtable)
xtable(df_total, digits = 3)

# comunalidades por estacion 
# para hacerlo por estacion debería ver que beta se corresponde con cual
