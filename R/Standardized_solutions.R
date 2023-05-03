Standardized_solutions <- function(specification, name_items, apply_threshold = TRUE) {
  library(tidyr)
  library(dplyr)
  library(stringr)

  result <- standardizedsolution(specification) %>%
    filter(op == "=~") %>%
    mutate(item  = str_remove(rhs, name_items) %>% as.double(),
           factor = str_remove(lhs, "f")) %>%
    select(lhs, rhs, est.std) %>%
    pivot_wider(
      names_from = lhs,
      values_from = c(est.std))

  num_factors <- ncol(result) - 1

  if (apply_threshold) {
    result <- result %>%
      mutate(across(paste0("f", 1:num_factors), ~ case_when(
        . <= 0.30 ~ 0, TRUE ~ .)))
  }

  factor_names <- paste0("f", 1:num_factors)
  result <- result %>%
    arrange_at(vars(factor_names), .funs = list(desc)) %>%
    rename(Items = rhs)

  return(result)
}
