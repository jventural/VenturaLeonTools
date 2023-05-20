mardia_test <- function(data) {
  library(psych)
  library(dplyr)

  A <- mardia(data, plot=F)

  result <- data.frame(Test = c("Mardia Skewness", "Mardia Kurtosis"),
                       Statistic = c(A$small.skew, A$kurtosis),
                       p.value = c(A$p.small, A$p.kurt),
                       Result = c(ifelse(A$p.small < 0.05, "NO", "YES"), ifelse(A$p.kurt < 0.05, "NO", "YES")))

  # Ajustar p.value a mostrar "p < .001" cuando sea 0 o menor a 0.001
  result$p.value <- ifelse(result$p.value <= 0.001, "p < .001", result$p.value)

  result <- result %>% mutate(across(where(is.numeric), round, 3))

  return(result)
}
