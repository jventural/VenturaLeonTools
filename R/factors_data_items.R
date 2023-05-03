factors_data_items <- function(summary_data, CCOV_items, num_factors) {
  library(dplyr)
  library(readxl)
  library(rlang)
  # Nombre de los items
  Items_name <- summary_data %>% select(Items) %>% pull()
  # Seleccionar solo los items con los nombres previamente elegidos
  target <- CCOV_items %>% filter(Items %in% Items_name)

  factor_names <- paste0("F", 1:num_factors)

  result <- inner_join(summary_data, target) %>%
    select(-h2, -u2) %>%
    as_tibble() %>%
    mutate(across(all_of(factor_names), ~ case_when(. <= 0.30 ~ 0, TRUE ~ .))) %>%
    arrange(across(all_of(factor_names), desc))

  return(result)
}
