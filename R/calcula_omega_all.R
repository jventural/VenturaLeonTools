calcula_omega_all <- function(extracted, data) {
  # Inicializar el dataframe de resultados
  resultados <- data.frame(Variable = character(),
                           Omega = numeric())

  # Función interna para calcular la fiabilidad
  fiabilidad_interna <- function(vars, data) {
    # Extraer los valores de las variables en 'vars' del data frame 'data'
    temp_data <- data %>% select(all_of(vars))

    # Crear el modelo utilizando los nombres de las variables en 'vars'
    model_original <- paste("F1 =~", paste0(vars, collapse = " + "))

    # Estimar el modelo
    fit.original <- cfa(model_original, data = temp_data, estimator = "WLSMV", mimic = "Mplus", ordered = TRUE)

    # Calcular la fiabilidad compuesta
    resultado <- compRelSEM(fit.original, tau.eq = F, ord.scale = T)
    return(resultado)
  }

  # Iterar sobre las llaves (nombres de las variables) en el objeto 'extracted'
  for (key in names(extracted)) {
    vars <- extracted[[key]]
    omega <- fiabilidad_interna(vars = vars, data = data)
    resultados <- rbind(resultados, data.frame(Variable = key, Omega = omega))
  }
  rownames(resultados) <- NULL

  # Convertir la columna Omega a numérica
  resultados$Omega <- as.numeric(resultados$Omega)

  return(resultados)
}
