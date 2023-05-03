extract_fit_measures <- function(Specifications) {
  CD <- list()

  for (i in 1:length(Specifications)) {
    bondad_modelo <- fitMeasures(Specifications[[i]], c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "tli.scaled", "rmsea.scaled"))
    CD[[i]] = bondad_modelo
  }

  Bondades_Original <- map_dfr(CD, bind_rows) %>% as.data.frame() %>% round(3) %>%
    mutate(Modelos = rep(paste0("f", 1:length(Specifications)))) %>% relocate(Modelos, .before = chisq.scaled)

  return(Bondades_Original)
}
