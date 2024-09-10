# Setup -------------------------------------------------------------------
pacman::p_load(
  tidyverse,
  srvyr,
  janitor,
  zoo,
  MetBrewer,
  scales
)

library(survey)

path_output <- "03_grafs/"
path_temp <- "04_temp/"

font_add_google(name = "Poppins", family = "Poppins")
font_add_google(name = "Roboto", family = "Roboto")
font_add_google("Montserrat")
showtext_auto()

files <- list.files(
  path = "01_input", pattern = "CB.*\\.dbf",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)


files_temp <- files[grep("1222|0923|1223", files, ignore.case = T)]


# Limpieza ----------------------------------------------------------------


ensu_full_srvyr <- map(files_temp, function(x) {
  # x <- files_temp[1]

  print(x)

  fecha_var <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  temp <- foreign::read.dbf(x) %>%
    clean_names() %>%
    mutate(
      var_id = ifelse(cve_ent == "14" &
        cve_mun %in% c("039"),
      "Guadalajara", "otro"
      )
    )

  design2 <- temp %>%
    as_survey_design(ids = upm_dis, weights = fac_sel, strata = est_dis)

  # Calcular el total de las respuestas

  output2 <- svyby(
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

  data2 <- output2 %>%
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
    filter(resp == "2")
})


# Gráfica -----------------------------------------------------------------


df_graf <- ensu_full_srvyr %>%
  reduce(rbind) %>%
  mutate(
    var_id = str_to_title(var_id),
    trim = as.yearmon(fecha)
  )

#* Barras de sensación de inseguridad por tipo----
df_graf %>%
  filter(fecha == "2023-09-01") %>%
  mutate(
    rank = rank(-por, ties.method = "first"),
    cut_var = case_when(
      rank >= min(rank) & rank < median(rank) ~ "primeros",
      rank >= median(rank) & rank <= max(rank) ~ "ultimos",
    )
  ) %>%
  select(var, cut_var) -> aux_orden

df_temp <- left_join(df_graf, 
                     aux_orden,
                     by = "var")

axis_lim <- max(df_temp$por + 0.05)

funico <- unique(df_temp$cut_var)

for (i in funico) {
  # i = funico[1]

  print(i)

  bar_temp2 <- df_temp %>%
    filter(cut_var == i) %>%
    mutate(
      var = as.factor(var)
    ) %>%
    group_by(fecha) %>%
    arrange(desc(por),
      .by_group = TRUE
    ) %>%
    dplyr::ungroup()


  orden_var <- bar_temp %>%
    filter(fecha == "2023-09-01") %>%
    arrange(-por) %>%
    pull(var) %>%
    unique()

  bar_temp$var <- factor(bar_temp$var, levels = orden_var)

  ggplot(
    bar_temp,
    aes(x = por, y = fct_rev(as.factor(var)), fill = as.factor(trim))
  ) +
    geom_bar(aes(),
      stat = "identity", position = "dodge2"
    ) +
    geom_text(aes(label = percent(por, accuracy = 0.1)),
      fontface = "bold", family = "Montserrat", size = 23, hjust = -0.1,
      position = position_dodge2(width = 0.9)
    ) +
    ggtitle(
      "% de población mayor de 18 años que se\nsiente insegura en espacio público",
      "Guadalajara, 3T-2023"
    ) +
    labs(y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
    tema_euzen(size_var = 65, font_var = "Roboto", lineheight_var = .3) +
    scale_alpha_manual(values = c(0.6, 0.9)) +
    scale_fill_manual(values = rev(met.brewer(name = "Peru1", n = 4))) +
    scale_x_continuous("% de población", labels = label_percent(0.1), limits = c(0, axis_lim)) +
    guides(
      fill = guide_legend(reverse = T),
      alpha = guide_legend(reverse = T)
    ) +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom"
    ) -> graf

  ggsave(paste0(path_output, "ensu_sens_inseg_", i, "2.png"), graf,
    width = 12.5, height = 13
  )
}
