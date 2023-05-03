specification_models <- function(modelos, data, estimator) {
  AD <- list()

  for (i in 1:length(modelos)) {
    fit.original = cfa(paste0(modelos[i]),
                       data = data,
                       estimator = estimator,
                       rotation = "oblimin",
                       mimic = "Mplus",
                       ordered = TRUE)
    AD[[i]] = fit.original
    print(fit.original)
  }

  return(AD)
}
