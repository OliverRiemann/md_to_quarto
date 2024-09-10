
# Anteriormente -----------------------------------------------------------

output <- svyby(
  ~ bp1_2_01 + bp1_2_02 + bp1_2_03 +
    bp1_2_04 + bp1_2_05 + bp1_2_06 +
    bp1_2_07 + bp1_2_08 + bp1_2_09 +
    bp1_2_10 + bp1_2_11 + bp1_2_12,
  by = ~var_id,
  design2,
  svytotal,
  vartype = "cvpct"
) %>%
  as_tibble()

(data2 <- output %>%
  select(-matches("cv")) %>%
  pivot_longer(
    cols = matches("^bp1"),
    names_to = "var",
    values_to = "total"
  ) %>%
  mutate(
    resp = str_sub(var, nchar(var)),
    var = substr(var, 1, nchar(var) - 1)
  ) %>%
  filter(
    var_id != "otro",
    resp != "3"
  ) %>%
  group_by(var_id, var) %>%
  mutate(
    tot = sum(total, na.rm = T),
    por = total / tot,
    fecha = as.Date(
      paste("01", substr(fecha_var, start = 1, stop = 2),
            substr(fecha_var, start = 3, stop = 4),
            sep = "-"
      ),
      format = "%d-%m-%y"
    ),
    var = str_wrap(case_when(
      var == "bp1_2_01" ~ "Casa",
      var == "bp1_2_02" ~ "Trabajo",
      var == "bp1_2_03" ~ "Calles que habitualmente usa",
      var == "bp1_2_04" ~ "Escuela",
      var == "bp1_2_05" ~ "Mercado",
      var == "bp1_2_06" ~ "Centro comercial",
      var == "bp1_2_07" ~ "Banco",
      var == "bp1_2_08" ~ "Cajeros automáticos",
      var == "bp1_2_09" ~ "Transporte público",
      var == "bp1_2_10" ~ "Automóvil",
      var == "bp1_2_11" ~ "Carretera",
      var == "bp1_2_12" ~ "Parque o centro recreativo"
    ))
  ) %>%
  ungroup() %>%
  filter(resp == "2") )


# Duda --------------------------------------------------------------------


output2 <- svyby(
  ~ bp1_2_01 + bp1_2_02 + bp1_2_03 +
    bp1_2_04 + bp1_2_05 + bp1_2_06 +
    bp1_2_07 + bp1_2_08 + bp1_2_09 +
    bp1_2_10 + bp1_2_11 + bp1_2_12,
  by = ~var_id,
  design2,
  svymean,
  vartype = "cvpct"
) %>%
  as_tibble()

(data3 <- output2 %>%
    select(-matches("cv")) %>%
    pivot_longer(
      cols = matches("^bp1"),
      names_to = "var",
      values_to = "total"
    ) %>%
    mutate(
      resp = str_sub(var, nchar(var)),
      var = substr(var, 1, nchar(var) - 1)
    ) %>%
    filter(
      var_id != "otro",
    ) %>%
    group_by(var_id, var) %>%
    mutate(
      tot = sum(total, na.rm = T),
      por = total / tot,
      fecha = as.Date(
        paste("01", substr(fecha_var, start = 1, stop = 2),
              substr(fecha_var, start = 3, stop = 4),
              sep = "-"
        ),
        format = "%d-%m-%y"
      ),
      var = str_wrap(case_when(
        var == "bp1_2_01" ~ "Casa",
        var == "bp1_2_02" ~ "Trabajo",
        var == "bp1_2_03" ~ "Calles que habitualmente usa",
        var == "bp1_2_04" ~ "Escuela",
        var == "bp1_2_05" ~ "Mercado",
        var == "bp1_2_06" ~ "Centro comercial",
        var == "bp1_2_07" ~ "Banco",
        var == "bp1_2_08" ~ "Cajeros automáticos",
        var == "bp1_2_09" ~ "Transporte público",
        var == "bp1_2_10" ~ "Automóvil",
        var == "bp1_2_11" ~ "Carretera",
        var == "bp1_2_12" ~ "Parque o centro recreativo"
      ))
    ) %>%
    ungroup() %>% 
    )
