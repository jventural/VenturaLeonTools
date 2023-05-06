calculate_descriptives <- function(data, start_col, end_col) {
  data %>%
    select(start_col:end_col) %>%
    psych::describe() %>%
    as.data.frame() %>%
    mutate("%" = mean/max*100) %>%
    select(mean, sd, min, max, skew, kurtosis, "%") %>%
    rename(Media = mean, DE = sd, Min. = min, Max. = max, g1 = skew, g2 = kurtosis) %>%
    round(2) %>%
    rownames_to_column(var = "Variables")
}
