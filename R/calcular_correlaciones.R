calcular_correlaciones <- function(data, columnas, method = c("spearman", "pearson"), winsorize = FALSE) {
  library(dplyr)
  library(WRS2)
  # Asegurar que method sea válido
  method <- match.arg(method)

  # Seleccionar las columnas
  data <- data[, columnas]

  if (winsorize && method == "pearson") {
    # Calcular la correlación de Pearson winsorizado
    result <- winall(data, tr = 0.2)$cor %>%
      round(2) %>%
      diag_aba_na() %>%
      as.data.frame()
  } else {
    # Calcular la correlación normal
    result <- data %>%
      cor(method = method) %>%
      round(2) %>%
      diag_aba_na() %>%
      as.data.frame()
  }

  return(result)
}

