mardia_test <- function(data) {
  library(psych)
  A <- mardia(data, plot=F)

  result <- data.frame(Test = c("Mardia Skewness", "Mardia Kurtosis"),
                       Statistic = c(A$small.skew, A$kurtosis),
                       p.value = c(A$p.small, A$p.kurt),
                       Result = c(ifelse(A$p.small < 0.05, "NO", "YES"), ifelse(A$p.kurt < 0.05, "NO", "YES")))

  result <- result %>% mutate(across(where(is.numeric), round, 3))

  return(result)
}
