normality_test_SW <- function(data, variables) {
  library(dplyr)
  library(tidyr)
  library(broom)
  check_normality <- function(p_value) {
    if (p_value < 0.05) {
      return("No-normal")
    } else {
      return("Normal")
    }
  }
  result <- data %>%
    pivot_longer(
      cols = {{variables}},
      names_to = "Variables",
      values_to = "Ptje_vi"
    ) %>%
    mutate(Variables = factor(Variables, levels = select(data, {{variables}}) %>% names())) %>%
    group_by(Variables) %>%
    summarise(
      `Shapiro-Wilk` = broom::tidy(shapiro.test(Ptje_vi))$statistic,
      p.value = ifelse(broom::tidy(shapiro.test(Ptje_vi))$p.value < 0.001, "p < .001", format(broom::tidy(shapiro.test(Ptje_vi))$p.value, scientific = FALSE)),
      Normality = check_normality(as.numeric(broom::tidy(shapiro.test(Ptje_vi))$p.value))
    )

  return(result)
}
