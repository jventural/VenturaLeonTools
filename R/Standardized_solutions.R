Standardized_solutions <- function(specification, name_items, num_factors, apply_threshold = TRUE) {
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

  if (apply_threshold) {
    result <- result %>%
      mutate(across(paste0("f", 1:num_factors), ~ case_when(
        . <= 0.30 ~ 0, TRUE ~ .)))
  }

  result <- result %>%
    arrange(desc(f1), desc(f2), desc(f3), desc(f4), desc(f5)) %>%
    rename(Items = rhs)

  return(result)
}
