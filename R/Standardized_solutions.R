Standardized_solutions <- function(specification, name_items, apply_threshold = TRUE) {
  library(tidyr)
  library(dplyr)
  library(stringr)

  num_factors <- length(specification@lhs)

  result <- standardizedsolution(specification) %>%
    filter(op == "=~") %>%
    mutate(item  = str_remove(rhs, name_items) %>% as.double(),
           factor = str_remove(lhs, "f")) %>%
    select(lhs, rhs, est.std) %>%
    pivot_wider(
      names_from = lhs,
      values_from = c(est.std))

  if (apply_threshold) {
    result <- result %>%
      mutate(across(paste0("f", 1:num_factors), ~ case_when(
        . <= 0.30 ~ 0, TRUE ~ .)))
  }

  factor_order <- paste0("desc(", paste0("f", 1:num_factors), ")", collapse = ", ")
  factor_order <- parse_expr(paste("arrange(", factor_order, ")"))
  result <- result %>%
    eval_tidy(factor_order) %>%
    rename(Items = rhs)

  return(result)
}

