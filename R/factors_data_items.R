factors_data_items <- function(summary_data, data_items, num_factors, apply_threshold = TRUE) {
  library(dplyr)
  library(readxl)
  library(rlang)

  # Nombre de los items
  Items_name <- summary_data %>% select(Items) %>% pull()

  # Seleccionar solo los items con los nombres previamente elegidos
  target <- data_items %>% filter(Items %in% Items_name)

  factor_names <- paste0("F", 1:num_factors)

  result <- inner_join(summary_data, target) %>%
    select(-h2, -u2) %>%
    as_tibble()

  if (apply_threshold) {
    result <- result %>%
      mutate(across(all_of(factor_names), ~ case_when(. <= 0.30 ~ 0, TRUE ~ .)))
  }

  result <- result %>%
    arrange(across(all_of(factor_names), desc))

  return(result)
}

