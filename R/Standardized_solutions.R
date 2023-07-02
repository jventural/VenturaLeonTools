Standardized_solutions <- function (specification, name_items, apply_threshold = TRUE)
{
  library(tidyr)
  library(dplyr)
  library(stringr)
  result <- standardizedsolution(specification) %>% filter(op ==
                                                             "=~") %>% mutate(item = str_remove(rhs, name_items) %>%
                                                                                as.double(), factor = str_remove(lhs, "f")) %>% select(lhs,
                                                                                                                                       rhs, est.std) %>% pivot_wider(names_from = lhs, values_from = c(est.std))

  # obtener los nombres de las columnas que comienzan con "f"
  factor_names <- colnames(result)[startsWith(colnames(result), "f")]

  if (apply_threshold) {
    result <- result %>% mutate(across(factor_names,
                                       ~case_when(. <= 0.3 ~ 0, TRUE ~ .)))
  }

  result <- result %>% arrange_at(vars(factor_names), .funs = list(desc)) %>%
    rename(Items = rhs)
  return(result)
}

