Calcule_Comparative <- function(data, cols, group_var) {
  group_values <- as.character(unique(data[[group_var]]))

  data %>%
    pivot_longer(
      cols = cols,
      names_to = "Variables_interes",
      values_to = "Ptje_vi"
    ) %>%
    group_by(Variables_interes) %>%
    summarise(
      sd_1 = sd(Ptje_vi[data[[group_var]] == group_values[1]]),
      sd_2 = sd(Ptje_vi[data[[group_var]] == group_values[2]]),
      const = 1  # Agrega una columna constante para unir los data frames
    ) %>%
    left_join(
      data %>%
        pivot_longer(
          cols = cols,
          names_to = "Variables_interes",
          values_to = "Ptje_vi"
        ) %>%
        group_by(Variables_interes) %>%
        summarise(
          broom::tidy(t.test(Ptje_vi ~ data[[group_var]], var.equal = FALSE)),
          d_cohen = WRS2::akp.effect(Ptje_vi ~ data[[group_var]], EQVAR = FALSE)$AKPeffect
        ) %>%
        select(
          Variables_interes, estimate, estimate1, estimate2, statistic, p.value, d_cohen, statistic, parameter
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
      Variables_interes, `M1(SD1)`, `M2(SD2)`, t, gl, p, d_cohen, Interpretacion
    ) %>%
    rename_with(
      ~str_replace_all(.x, "M1", group_values[1]),
      starts_with("M1")
    ) %>%
    rename_with(
      ~str_replace_all(.x, "M2", group_values[2]),
      starts_with("M2")
    )
}
