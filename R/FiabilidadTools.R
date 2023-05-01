Fiabilidad <- function(vars, data) {
  library(lavaan)
  library(semTools)
  # Extraer los valores de las variables en 'vars' del data frame 'data'
  temp_data <- data %>% select(all_of(vars))

  # Crear el modelo utilizando los nombres de las variables en 'vars'
  model_original <- paste("F1 =~", paste0(vars, collapse = " + "))

  # Estimar el modelo
  fit.original <- cfa(model_original, data = temp_data, estimator = "WLSMV", mimic = "Mplus", ordered = TRUE)

  # Calcular la fiabilidad compuesta
  resultado <- compRelSEM(fit.original, tau.eq = F, ord.scale = T)
  output <- sprintf("El coeficiente omega para tus datos es %f", resultado)
  return(output)
}
