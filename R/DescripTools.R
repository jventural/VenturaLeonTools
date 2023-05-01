calcular_porcentajes <- function(data, columnas) {
  resultados_lista <- map(columnas, function(col) {
    data %>%
      group_by(.data[[col]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Porcentaje = n / sum(n) * 100)
  })
  
  resultados_lista <- setNames(resultados_lista, columnas)
  return(resultados_lista)
}