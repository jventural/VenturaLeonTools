grafico_boxplots <- function(data, cols) {
  library(reshape2)
  library(ggplot2)

  data <- data %>% mutate(ID = 1:nrow(data))

  dat.m <- melt(data, id.vars = 'ID', measure.vars = cols) %>%
    rename(Variables = "variable")

  p <- ggplot(dat.m) +
    geom_boxplot(aes(x=ID, y=value, fill=Variables)) +
    facet_wrap(~Variables, scales = "free") +
    theme_bw() +
    xlab(" ")

  return(p)
}
