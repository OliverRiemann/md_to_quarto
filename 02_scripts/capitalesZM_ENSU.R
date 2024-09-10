# ENSU CAPITALES ZMG

pacman::p_load(
  tidyverse, scales, purrr, lubridate, janitor, zoo, cowplot, ggrepel, numform, leaflegend, htmltools, ggpol, doBy, viridis, foreign, reldist, cartography,
  RPostgreSQL, yaml, sf, grid, gridExtra, leaflet, rpostgis, googlesheets4, sp, treemapify, ggbeeswarm, ggthemes, shapefiles, stargazer, rgeos, biscale,
  ggalluvial, htmltools, htmlwidgets, stringr, reshape2, gt, wordcloud, SnowballC, tm, cluster, rgdal, grid, survey, MetBrewer, srvyr, readxl, naturalsort, zoo,
  ggrepel
)

Sys.setlocale(locale = "es_ES.UTF-8")

source("tema_euzen.R")

path_output <- "03_grafs/"
path_temp <- "04_temp/"

options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "adjust")

######################################################### PERCEPCION DE INSEGURIDAD #########################################################

# Percepcion Guadalajara vs Nacional----

#* Cargar bases----
files <- list.files(
  path = "01_input",
  pattern = "CB.*\\.dbf",
  full.names = T,
  recursive = T
)

files_temp <- files[(grep("0321|0621|0921|1221|0322|0622|0922|1222|23|24", files))]
#* Limpiar bases----

ensu_full <- map(files_temp, function(x) {
  # x = files_temp[1]

  print(x)

  fecha_var <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  if (grepl("15\\.", x, ignore.case = T)) {
    foreign::read.dbf(x) %>%
      clean_names() %>%
      rename(
        cve_ent = ent,
        fac_sel = factor,
        est_dis = edis,
        bp1_1 = p1
      ) -> temp
  } else if (grepl("0316|0616|0916", x, ignore.case = T)) {
    foreign::read.dbf(x) %>%
      clean_names() %>%
      rename(
        cve_ent = ent,
        cve_mun = mun
      ) -> temp
  } else {
    foreign::read.dbf(x) %>%
      clean_names() -> temp
  }

  temp %>%
    mutate(
      fac_sel = as.numeric(fac_sel),
      var_id = "Nacional",
      cd = as.character(cd),
      nom_mun = as.character(nom_mun),
      var_cd = case_when(
        cd == "01" ~ "Aguascalientes",
        cd == "02" ~ "Mexicali",
        cd == "04" ~ "La Paz",
        cd == "05" ~ "Campeche",
        cd == "06" ~ "Saltillo",
        cd == "09" ~ "Colima",
        cd == "11" ~ "Tuxtla Gutiérrez",
        cd == "13" ~ "Chihuahua",
        cd == "19" ~ "Durango",
        cd == "57" ~ "Guanajuato",
        cd == "22" ~ "Chilpancingo",
        cd == "23" ~ "Pachuca",
        cd == "59" ~ "Guadalajara",
        cd == "26" ~ "Toluca",
        cd == "29" ~ "Morelia",
        cd == "32" ~ "Cuernavaca",
        cd == "33" ~ "Tepic",
        cd == "64" ~ "Monterrey",
        cd == "35" ~ "Oaxaca",
        cd == "36" ~ "Puebla",
        cd == "37" ~ "Querétaro",
        cd == "93" ~ "Chetumal",
        cd == "39" ~ "San Luis Potosí",
        cd == "40" ~ "Culiacán",
        cd == "43" ~ "Hermosillo",
        cd == "45" ~ "Villahermosa",
        cd == "95" ~ "Ciudad Victoria",
        cd == "49" ~ "Tlaxcala",
        cd == "50" ~ "Veracruz",
        cd == "52" ~ "Mérida",
        cd == "53" ~ "Zacatecas",
        T ~ "Otro"
      )
    ) %>%
    select(
      var_id, var_cd, upm_dis,
      est_dis, fac_sel, bp1_1
    ) %>%
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>%
    filter(!is.na(bp1_1)) -> temp2

  vars <- list(c("var_id", "bp1_1"), c("var_cd", "bp1_1"))

  output <- map(vars, function(v) {
    print(v[[1]])

    temp2 %>%
      group_by(across(all_of(v))) %>% 
      summarise(
        por = survey_mean(vartype = "cv"),
        total = survey_total(vartype = "cv"),
        .groups = "drop"
      ) %>%
      mutate(var = v[[1]])
  })

  data <- output %>%
    reduce(full_join) %>%
    mutate(
      var_id = coalesce(var_id, var_cd),
      bp1_1 = as.numeric(bp1_1),
      fecha = as.Date(
        paste("01", substr(fecha_var, start = 1, stop = 2),
          substr(fecha_var, start = 3, stop = 4),
          sep = "-"
        ),
        format = "%d-%m-%y"
      )
    ) %>%
    select(-matches("cv|var_cd")) %>%
    filter(bp1_1 == "2")
  
  return(data)
})


ensu_full %>%
  reduce(rbind) %>%
  filter(var_id != "Otro") %>%
  mutate(
    trim = as.yearqtr(fecha),
    # var_id = fct_reorder(var_id, -por, tail, n = 1, .desc = TRUE)
  ) -> df_percp

write_rds(df_percp, "04_temp/ensu_percep_capitales_10.rds")
# usar el código de Graficas_corregidas(Orden).R
# #* Serie de tiempo Top Min y Máx ----
# ggplot(
#   df_percp,
#   aes(x = trim, y = reorder(var_id, (por)), fill = por)) +
#   geom_tile(data = df_percp %>%
#               filter(trim == "2023 Q3"),
#             color = "black") +
#   geom_tile(color = "black") +
#   geom_text(aes(label = scales::percent(por, accuracy = 0.1)),
#             fontface = "bold", size = 6, family = "Montserrat") +
#   ggtitle("% de la población mayor de 18 años que se siente insegura",
#           "Ciudades capitales, 1T 2021 - 2T 2024") +
#   labs(x = "Trimestre", y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
#   tema_euzen(size_var = 28, font_var = "Roboto") +
#   scale_x_yearqtr(format = "%qT-%y", n = 10) +
#   scale_fill_gradientn(colors = met.brewer("Morgenstern"))+
#   guides(fill = "none") +
#   theme(
#     axis.text.y = element_text(hjust = 1)
#   ) -> graf
#
# graf
#
# ggsave(paste0(path_output, "ensu_percep_capitales.png"), graf,
#        width = 18, height = 10)

# Comparativo percepción de inseguridad por ciudades ----------------------

files_temp <- files[(grep("0318|0618|0918|1218|0319|0619|0919|1219|0320|0920|1220|0321|0621|0921|1221|0322|0622|0922|1222|23|24", files, ignore.case = T))]
#* Limpiar bases----
map(files_temp, function(x) {
  # x = files_temp[1]

  print(x)

  fecha_var <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  if (grepl("15\\.", x, ignore.case = T)) {
    temp <- foreign::read.dbf(x) %>%
      clean_names() %>%
      rename(
        cve_ent = ent,
        fac_sel = factor,
        est_dis = edis,
        bp1_1 = p1
      )
  } else if (grepl("0316|0616|0916", x, ignore.case = T)) {
    temp <- foreign::read.dbf(x) %>%
      clean_names() %>%
      rename(
        cve_ent = ent,
        cve_mun = mun
      )
  } else {
    temp <- foreign::read.dbf(x) %>%
      clean_names()
  }

  temp2 <- temp %>%
    mutate(
      fac_sel = as.numeric(fac_sel),
      var_id = "Nacional",
      cd = as.character(cd),
      nom_mun = as.character(nom_mun),
      var_cd = case_when(
        cd == "59" ~ "Guadalajara",
        cve_ent == "21" & cve_mun == "114" ~ "Puebla",
        cd == 64 ~ "Monterrey",
        T ~ "Otro"
      )
    ) %>%
    select(
      var_id, var_cd, upm_dis,
      est_dis, fac_sel, bp1_1
    ) %>%
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>%
    filter(!is.na(bp1_1))

  vars <- list(list("var_id", "bp1_1"), list("var_cd", "bp1_1"))

  map(vars, function(v) {
    print(v[[1]])

    temp2 %>%
      group_by_at(.vars = unlist(v)) %>%
      summarise(
        por = survey_mean(vartype = "cv"),
        total = survey_total(vartype = "cv"),
        .groups = "drop"
      ) %>%
      mutate(var = v[[1]])
  }) -> output

  output %>%
    reduce(full_join) %>%
    mutate(
      var_id = coalesce(var_id, var_cd),
      bp1_1 = as.numeric(bp1_1),
      fecha = as.Date(
        paste("01", substr(fecha_var, start = 1, stop = 2),
          substr(fecha_var, start = 3, stop = 4),
          sep = "-"
        ),
        format = "%d-%m-%y"
      )
    ) %>%
    select(-matches("var_cd")) %>%
    filter(bp1_1 == "2") -> data
}) -> ensu_full

ensu_full %>%
  reduce(full_join) %>%
  filter(var_id != "Otro") %>%
  mutate(
    trim = as.yearqtr(fecha),
    alphavar = case_when(
      var_id %in% c("Ciudad de México", "Guadalajara", "Monterrey") ~ "Color",
      T ~ "NoColor"
    )
  ) -> df_percp


#* Heatmap de conflictos----

graf <- ggplot(
  df_percp,
  aes(x = trim, y = por, color = var_id, alpha = alphavar)
) +
  geom_line(size = 3) +
  geom_point(size = 5) +
  geom_text_repel(
    data = df_percp %>%
      filter(fecha == as.Date("2024-06-01")),
    aes(label = paste0(scales::percent(por, accuracy = .1), " - ", var_id)),
    fontface = "bold", size = 15, family = "Montserrat", show.legend = F, nudge_x = 0.1, hjust = 0
  ) +
  ggtitle(
    "% de la población mayor de 18 años que se siente insegura",
    "Ciudades, 1T 2021 - 2T 2024"
  ) +
  labs(
    x = "Trimestre", y = "",
    caption = "Fuente: Encuesta Nacional de Seguridad Publica Urbana - INEGI
       Nota: Las Zonas Metropolitanas (ZM) no incluyen sus respectivas capitales"
  ) +
  tema_euzen(size_var = 50, font_var = "Roboto", lineheight_var = 0.6) +
  scale_x_yearqtr(format = "%qT-%y", n = 9) +
  scale_color_manual(values = met.brewer("Signac", 10)[-c(5, 6)]) +
  scale_alpha_manual(values = c(1, .65)) +
  expand_limits(x = as.yearqtr(as.Date(c("2018-03-01", "2025-04-01")))) +
  guides(color = "none", alpha = "none") +
  scale_y_continuous("% de población", labels = scales::percent)
graf
ggsave(paste0(path_output, "ensu_percep_zm.png"), graf,
  width = 20, height = 10
)
# esta ya está lista

# Serie de tiempo capitales
files_temp <- files[(grep("18\\.|19\\.|20\\.|21\\.|22\\.|23\\.|24", files, ignore.case = T))]
#* Limpiar bases----
map(files_temp, function(x) {
  # x = files_temp[1]

  print(x)

  fecha_var <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  if (grepl("15\\.", x, ignore.case = T)) {
    foreign::read.dbf(x) %>%
      clean_names() %>%
      rename(
        cve_ent = ent,
        fac_sel = factor,
        est_dis = edis,
        bp1_1 = p1
      ) -> temp
  } else if (grepl("0316|0616|0916", x, ignore.case = T)) {
    foreign::read.dbf(x) %>%
      clean_names() %>%
      rename(
        cve_ent = ent,
        cve_mun = mun
      ) -> temp
  } else {
    foreign::read.dbf(x) %>%
      clean_names() -> temp
  }

  temp %>%
    mutate(
      fac_sel = as.numeric(fac_sel),
      var_id = "Nacional",
      cd = as.character(cd),
      nom_mun = as.character(nom_mun),
      var_cd = case_when(
        cd == "01" ~ "Aguascalientes",
        cd == "02" ~ "Mexicali",
        cd == "04" ~ "La Paz",
        cd == "05" ~ "Campeche",
        cd == "06" ~ "Saltillo",
        cd == "09" ~ "Colima",
        cd == "11" ~ "Tuxtla Gutiérrez",
        cd == "13" ~ "Chihuahua",
        cd == "19" ~ "Durango",
        cd == "57" ~ "Guanajuato",
        cd == "22" ~ "Chilpancingo",
        cd == "23" ~ "Pachuca",
        cd == "59" ~ "Guadalajara",
        cd == "26" ~ "Toluca",
        cd == "29" ~ "Morelia",
        cd == "32" ~ "Cuernavaca",
        cd == "33" ~ "Tepic",
        cd == "64" ~ "Monterrey",
        cd == "35" ~ "Oaxaca",
        cd == "36" ~ "Puebla",
        cd == "37" ~ "Querétaro",
        cd == "93" ~ "Chetumal",
        cd == "39" ~ "San Luis Potosí",
        cd == "40" ~ "Culiacán",
        cd == "43" ~ "Hermosillo",
        cd == "45" ~ "Villahermosa",
        cd == "95" ~ "Ciudad Victoria",
        cd == "49" ~ "Tlaxcala",
        cd == "50" ~ "Veracruz",
        cd == "52" ~ "Mérida",
        cd == "53" ~ "Zacatecas",
        T ~ "Otro"
      )
    ) %>%
    select(
      var_id, var_cd, cd, upm_dis,
      est_dis, fac_sel, bp1_1
    ) %>%
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>%
    filter(!is.na(bp1_1)) -> temp2

  vars <- list(list("var_id", "bp1_1"), list("var_cd", "bp1_1"))

  map(vars, function(v) {
    print(v[[1]])

    temp2 %>%
      group_by_at(.vars = unlist(v)) %>%
      summarise(
        por = survey_mean(vartype = "cv"),
        total = survey_total(vartype = "cv"),
        .groups = "drop"
      ) %>%
      mutate(var = v[[1]])
  }) -> output

  output %>%
    reduce(full_join) %>%
    mutate(
      var_id = coalesce(var_id, var_cd),
      bp1_1 = as.numeric(bp1_1),
      fecha = as.Date(
        paste("01", substr(fecha_var, start = 1, stop = 2),
          substr(fecha_var, start = 3, stop = 4),
          sep = "-"
        ),
        format = "%d-%m-%y"
      )
    ) %>%
    select(-matches("cv|var_cd")) %>%
    filter(bp1_1 == "2") -> data
}) -> ensu_full

ensu_full %>%
  reduce(full_join) %>%
  filter(var_id != "Otro") %>%
  mutate(
    trim = as.yearqtr(fecha)
  ) -> df_percp

write_rds(df_percp, file = "04_temp/df_percep.rds")

temp <- df_percp %>%
  group_by(var_id) %>%
  filter(fecha == "2024-06-01") %>%
  ungroup() %>%
  mutate(
    var_label = paste0(var_id, " (", percent(por, .1), ")"),
    rank = rank(por),
    por = case_when(
      var_id == "Zacatecas" ~ por + 0.03,
      var_id == "Toluca" ~ por + .04,
      var_id == "Chilpancingo" ~ por + 0.01,
      var_id == "Cuernavaca" ~ por - 0.02,
      var_id == "Guadalajara" ~ por + 0.01,
      var_id == "Puebla" ~ por - 0.02,
      var_id == "Tepic" ~ por + 0.05,
      var_id == "Durango" ~ por + .03,
      var_id == "Mérida" ~ por + 0.03,
      var_id == "Saltillo" ~ por - 0.04,
      var_id == "La Paz" ~ por - 0.04,
      var_id == "Tuxtla Gutiérrez" ~ por - 0.02,
      var_id == "Colima" ~ por - 0.02,
      T ~ por
    )
  ) %>%
  filter(rank %in% c(seq(1, 5), seq(29, 32), 15, 22))

temp %>%
  pull(por, var_label) -> last_labels

df_percp %>%
  dplyr::semi_join(temp, by = c("var_id")) -> df_graf

#* Población mayor de 18 años que se siente insegura----
graf <- ggplot(
  df_graf,
  aes(x = trim, y = por, col = reorder(var_id, por))
) +
  geom_line(aes(group = var_id), size = 1.5, alpha = .4,
            show.legend = FALSE) +
  geom_line(
    data = df_graf %>% filter(var_id %in% c("Guadalajara", "Nacional")),
    aes(group = var_id), size = 1.5
  ) +
  geom_point(
    data = df_graf %>% filter(var_id %in% c("Guadalajara", "Nacional")), size = 3) +
  geom_point(alpha = 0.5, size = 3) +
  ggtitle(
    "% de la población mayor de 18 años que se siente insegura",
    "Los mejores y peores 5 estados (2T 2024)"
  ) +
  labs(
    x = "Trimestre",
    caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI"
  ) +
  tema_euzen(size_var = 45, font_var = "Roboto") +
  scale_x_yearqtr(format = "T%q-%y", n = 6) +
  scale_color_manual("", values = c(rev(met.brewer("Paquin")), "#a30ecc")) +
  scale_y_continuous("% de población",
    labels = scales::percent, limits = c(0, 1),
    sec.axis = sec_axis(~., breaks = last_labels, labels = names(last_labels))
  ) +
  guides(color = guide_legend(nrow = 2)) +
  theme(
    axis.text.y.right = element_text(size = 15),
    plot.caption = element_text(size = 10),
    legend.position = "bottom",
    plot_caption = element_text(family = "Montserrat", size = 12)
  )
graf
ggsave(paste0(path_output, "ensu_percep_minmax.png"), graf,
  width = 16, height = 8
)
