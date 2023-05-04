Calcule_Comparative <- function(data, cols, group_var) {
  library(tidyverse)
  group_values <- as.character(unique(data[[group_var]]))

  data_with_order <- data %>%
    mutate(order = match(colnames(data), cols)) %>%
    pivot_longer(
      cols = cols,
      names_to = "Variables_interes",
      values_to = "Ptje_vi"
    )

  data_with_order %>%
    group_by(Variables_interes) %>%
    summarise(
      sd_1 = sd(Ptje_vi[data[[group_var]] == group_values[1]]),
      sd_2 = sd(Ptje_vi[data[[group_var]] == group_values[2]]),
      const = 1,  # Agrega una columna constante para unir los data frames
      order = first(order) # Agrega el orden de las columnas
    ) %>%
    left_join(
      data_with_order %>%
        group_by(Variables_interes) %>%
        summarise(
          broom::tidy(t.test(Ptje_vi ~ data[[group_var]], var.equal = FALSE)),
          d_cohen = WRS2::akp.effect(Ptje_vi ~ data[[group_var]], EQVAR = FALSE)$AKPeffect,
          order = first(order) # Agrega el orden de las columnas
        ) %>%
        select(
          Variables_interes, estimate, estimate1, estimate2, statistic, p.value, d_cohen, statistic, parameter, order
        ) %>%
        rename(M_1 = estimate1, M_2 = estimate2, Diff = estimate, gl = parameter, t = statistic, p = p.value)
    ) %>%
    mutate(
      M1_SD1 = paste0(round(M_1, 2), " (", round(sd_1, 2), ")"),
      M2_SD2 = paste0(round(M_2, 2), " (", round(sd_2, 2), ")")
    ) %>%
    rename(
      `M1(SD1)` = M1_SD1,
      `M2(SD2)` = M2_SD2
    ) %>%
    mutate(
      Interpretacion = if_else(
        abs(d_cohen) > 0.80, "Grande",
        if_else(abs(d_cohen) > 0.50, "Mediano",
                if_else(abs(d_cohen) > 0.30, "Pequeño", "Trivial"))
      )
    ) %>%
    select(
      Variables_interes, `M1(SD1)`, `M2(SD2)`, t, gl, p, d_cohen, Interpretacion, order
    ) %>%
    rename_with(
      ~str_replace_all(.x, "M1", group_values[1]),
      starts_with("M1")
    ) %>%
    rename_with(
      ~str_replace_all(.x, "M2", group_values[2]),
      starts_with("M2")
    ) %>%
    arrange(order) %>% # Ordena el resultado final según la columna 'order'
    select(-order) # Elimina la columna 'order
}
