# Setup----
dir.create("01_input")
dir.create("02_scripts")
dir.create("03_grafs")
dir.create("04_temp")

pacman::p_load(
  tidyverse, scales, lubridate, janitor, cowplot, ggrepel, numform, leaflegend, htmltools, ggpol, rio, viridis, tableHTML, openxlsx, cartography,
  RPostgreSQL, yaml, sf, grid, gridExtra, leaflet, rpostgis, googlesheets4, sp, treemapify, ggbeeswarm, ggthemes, shapefiles, stargazer, rgeos, biscale,
  ggalluvial, htmltools, htmlwidgets, stringr, reshape2, gt, wordcloud, SnowballC, tm, cluster, rgdal, grid, survey, MetBrewer, srvyr
)

Sys.setlocale(locale = "es_ES.UTF-8")

source("tema_euzen.R")

path_output <- "03_grafs/"
path_temp <- "04_temp/"

options(survey.adjust.domain.lonely = T)
options(survey.lonely.psu = "adjust")


# Victimas ----------------------------------------------------------------


## Cargar bases----
files <- list.files(
  path = "01_input", pattern = "CB.*\\.dbf",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

files_temp <- files[(grep("1218|0619|1219|1220|0621|1221|0622|1222|0623|1223|0624",
  files,
  ignore.case = T
))]

viv_files <- list.files(
  path = "01_input", pattern = "VIV.*\\.dbf",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)
# Numero de victimas----

#* Limpiar bases----
ensu_full <- map(files_temp, function(x) {
  # x <- files_temp[1]

  print(x)

  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  viv_temp <- viv_files[(grep(fecha, viv_files, ignore.case = T))]

  left_join(
    foreign::read.dbf(x) %>%
      clean_names(),
    foreign::read.dbf(viv_temp) %>%
      clean_names() %>%
      select(cve_ent, upm, viv_sel, fac_viv)
  ) %>%
    mutate(
      var_id = "nacional",
      var_cd = ifelse(cve_ent == "19" &
        cve_mun %in% c("039"),
      "Monterrey", "otro"
      ),
      vict_var = ifelse(bp1_6_1 == "1" | bp1_6_2 == "1" | bp1_6_3 == "1" |
        bp1_6_4 == "1" | bp1_6_5 == "1" | bp1_6_6 == "1", "1", "0"),
      fecha = as.Date(
        paste("01", substr(fecha, start = 1, stop = 2),
          substr(fecha, start = 3, stop = 4),
          sep = "-"
        ),
        format = "%d-%m-%y"
      )
    ) %>%
    select(
      fecha, var_id, var_cd, cve_ent,
      cve_mun, nom_mun, upm_dis,
      est_dis, fac_viv, vict_var
    ) %>%
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_viv) -> temp

  temp %>%
    group_by(fecha, var_id, vict_var) %>%
    summarise(
      por = survey_mean(vartype = "cv"),
      total = survey_total(vartype = "cv")
    ) -> nac

  temp %>%
    filter(var_cd == "Monterrey") %>%
    group_by(fecha, var_cd, vict_var) %>%
    summarise(
      por = survey_mean(vartype = "cv"),
      total = survey_total(vartype = "cv")
    ) -> estat

  plyr::rbind.fill(nac, estat) %>%
    mutate(
      var_id = coalesce(var_id, var_cd),
      vict_var = str_wrap(case_when(
        vict_var == "1" ~ "Hogares con víctima",
        vict_var == "0" ~ "Hogares sin víctima"
      ), 15)
    ) %>%
    select(-c(var_cd)) -> data
})

ensu_full %>%
  reduce(full_join) %>%
  mutate(
    var_id = str_to_title(var_id),
    mesano = zoo::as.yearmon(fecha, format = "%b/%Y"),
    trim = as.yearqtr(fecha)
  ) %>%
  filter(var_id == "Monterrey") %>%
  as_tibble() -> df_vict

#* Stacked bar Nacional vs ZM Monterrey----
n_facet <- length(unique(df_vict$fecha))

ggplot(
  df_vict,
  aes(x = as.factor(mesano), y = por, fill = vict_var)
) +
  geom_bar(aes(),
    stat = "identity", position = "stack"
  ) +
  geom_text(
    aes(
      label = percent(por, accuracy = 0.1),
      col = vict_var
    ),
    fontface = "bold", family = "Montserrat", size = 10,
    position = position_stack(vjust = 0.5)
  ) +
  # facet_wrap(~as.factor(fecha), ncol = n_facet) +
  ggtitle(
    "Condición de victimización en el hogar",
    "Monterrey, % de hogares con al menos una víctima"
  ) +
  labs(x = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 35, font_var = "Roboto", lineheight_var = 0.6) +
  scale_alpha_manual(values = c(0.9, 0.7)) +
  scale_fill_manual(values = c(
    "#133e7e",
    "#da6c42"
  )) +
  scale_color_manual(values = c(
    "white",
    "black"
  )) +
  scale_y_continuous("% de hogares", labels = scales::percent_format(accuracy = 1L)) +
  guides(
    fill = guide_legend(reverse = F),
    alpha = "none",
    color = "none"
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  ) -> graf
graf
ggsave(paste0(path_output, "ensu_vict.png"), graf,
  width = 16, height = 8
)
#
# Victimas por tipo de delito ZM de Monterrey----

#* Limpiar bases----
ensu_full <- map(files_temp, function(x) {
  print(x)

  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  viv_temp <- viv_files[(grep(fecha, viv_files, ignore.case = T))]

  left_join(
    foreign::read.dbf(x) %>%
      clean_names(),
    foreign::read.dbf(viv_temp) %>%
      clean_names() %>%
      select(cve_ent, upm, viv_sel, fac_viv)
  ) %>%
    mutate(
      var_cd = ifelse(cve_ent == "19" &
        cve_mun %in% c("039"),
      "Monterrey", "otro"
      )
    ) -> temp

  design <- svydesign(ids = ~upm_dis, weights = ~fac_viv, strata = ~est_dis, data = temp)

  as_tibble(svyby(
    ~ bp1_6_1 + bp1_6_2 + bp1_6_3 +
      bp1_6_4 + bp1_6_5 + bp1_6_6,
    by = ~var_cd, design, svymean, vartype = "cvpct", na.rm = T
  )) -> output

  output %>%
    select(-matches("cv")) %>%
    pivot_longer(
      cols = matches("bp1"),
      names_to = "var",
      values_to = "por"
    ) %>%
    mutate(
      resp = str_sub(var, nchar(var)),
      var = substr(var, 1, nchar(var) - 1),
      fecha = as.Date(
        paste("01", substr(fecha, start = 1, stop = 2),
          substr(fecha, start = 3, stop = 4),
          sep = "-"
        ),
        format = "%d-%m-%y"
      ),
      var = str_wrap(case_when(
        var == "bp1_6_1" ~ "Robo total de vehículo",
        var == "bp1_6_2" ~ "Robo parcial de vehículo",
        var == "bp1_6_3" ~ "Robo a casa habitación",
        var == "bp1_6_4" ~ "Robo o asalto en calle o transporte público",
        var == "bp1_6_5" ~ "Robo en forma distinta",
        var == "bp1_6_6" ~ "Extorsión"
      ), 20)
    ) %>%
    filter(
      var_cd != "otro",
      resp == "1"
    ) -> data
})

ensu_full %>%
  reduce(full_join) %>%
  mutate(
    fecha = zoo::as.yearmon(fecha, format = "%b/%Y")
  ) -> df_graf

#* Barras por tipo de delito ZM de Monterrey----
ggplot(
  df_graf,
  aes(x = reorder(var, -por), y = por, fill = as.factor(fecha), group = as.factor(fecha))
) +
  geom_bar(stat = "identity", position = "dodge2") +
  geom_text(aes(label = percent(por, accuracy = 0.1)),
    fontface = "bold", family = "Montserrat", size = 6, hjust = -.2, angle = 90, col = "black",
    position = position_dodge2(width = 0.9),
  ) +
  ggtitle(
    "Hogares con integrantes víctimas, por tipo de delito",
    "Monterrey, 2T 2024"
  ) +
  labs(x = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 26, font_var = "Roboto", lineheight_var = .8) +
  scale_fill_manual(values = met.brewer(name = "Signac", n = 13)[-5:-6]) +
  scale_y_continuous("% de hogares", labels = scales::percent_format(accuracy = 1L), limits = c(0, max(df_graf$por) + 0.025)) +
  guides(
    fill = guide_legend(reverse = F, nrow = 1),
    color = "none"
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) -> graf

ggsave(paste0(path_output, "ensu_tip_vict.png"),
  graf,
  width = 18, height = 10
)



# # Acoso Sexual
#
# files_temp <- files[(grep("0622|1221", files, ignore.case = T))];
#
#
# ensu_full <- map(files_temp, function(x) {
#
#   print(x)
#
#   fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_")
#
#   viv_temp <- viv_files[(grep(fecha, viv_files, ignore.case = T))];
#
#   left_join(
#     foreign::read.dbf(x) %>%
#       clean_names(),
#     foreign::read.dbf(viv_temp) %>%
#       clean_names() %>%
#       select(cve_ent, upm, viv_sel, fac_viv)
#   ) %>%
#     mutate(
#       var_id = "nacional",
#       var_cd = ifelse(cve_ent == "19" &
#                           cve_mun %in% c("039"),
#                         "Monterrey", "otro")
#     ) -> temp
#
#   nrow(temp)
#
#   design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
#
#   vars <- c("var_id", "var_cd")
#
#   funion <- function(z) {
#
#     as_tibble(svyby(~bp4_1_1 + bp4_1_2 + bp4_1_3 +
#                       bp4_1_4 + bp4_1_5 + bp4_1_6 +
#                       bp4_1_7 + bp4_1_8 + bp4_1_9,
#                     by = as.formula(paste0("~", z)), design, svytotal, vartype = "cvpct"))
#
#   }
#
#   output <- map(vars, funion)
#
#   output %>%
#     reduce(full_join) %>%
#     select(-matches("cv")) %>%
#     pivot_longer(cols = matches("bp4"),
#                  names_to = "var",
#                  values_to = "total") %>%
#     mutate(var_id = coalesce(var_cd, var_id),
#            resp = str_sub(var, nchar(var)),
#            var = substr(var, 1, nchar(var) - 1)) %>%
#     filter(var_id != "otro") %>%
#     select(-var_cd) %>%
#     group_by(var_id, var) %>%
#     mutate(
#       tot = sum(total, na.rm = T),
#       por = total / tot,
#       fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2),
#                             substr(fecha, start = 3, stop = 4), sep = "-"),
#                       format = "%d-%m-%y"),
#
#     ) %>%
#     ungroup() %>%
#     filter(resp == "1") -> data
#
# })
#
# ensu_full %>%
#   reduce(full_join) %>%
#   mutate(
#     var_id = case_when(
#       var_id == "Monterrey" ~ "Monterrey",
#       var_id == "nacional" ~ "Nacional"
#     )
#   ) %>%
#   group_by(var_id,fecha) %>%
#   summarise(por=sum(por)) -> df_graf
#
# ggplot(
#   df_graf,
#   aes(x = fecha, y = por, col = var_id)
# ) +
#   geom_line(aes(size = var_id, alpha = var_id)) +
#   geom_point(alpha = 0.6, size = 3) +
#   # geom_line(stat = "smooth",method = "lm", size = 1.5, linetype ="dashed", alpha = 0.5) +
#   geom_text(aes(label = percent(por, accuracy = 0.1)),
#             fontface = "bold", family = "Montserrat", size = 5,
#             show.legend = F, nudge_y = -.03) +
#   ggtitle("Víctimas de acoso y violencia sexual entre el 4T-2019 al 4T-2021 (Nacional contra ZM de Monterrey)",
#           "% de población mayor de 18 años sufrió algún tipo de acoso o violencia sexual en los últimos 6 meses") +
#   labs(caption = "Metodología: Realización propia con los datos de ENSU 4T 2019 a 4T 2021 (INEGI)") +
#   tema_euzen() +
#   scale_size_manual(values = c(2, 1)) +
#   scale_alpha_manual(values = c(0.9, 0.6)) +
#   scale_color_manual(values = c(met.brewer(name = "OKeeffe1", n = 2)[-3:-4], "grey20")) +
#   scale_x_date("", date_breaks = "3 month", labels = date_format("%b\n%y"), limits  = as.Date(c('2019-11-01','2021-12-01'))) +
#   scale_y_continuous("% de población",
#                      labels = scales::percent_format(accuracy = 1L),
#                      limits=c(.15, .5)) +
#   guides(size = "none") +
#   theme(legend.position = "bottom",
#         legend.title = element_blank()) -> graf
#
# ggsave(paste0(path_output, "ensu_acoso.png"), graf,
#        width = 12, height = 10)


#* Barras de abuso sexual
#
# ensu_full %>%
#   reduce(full_join) %>%
#   filter(fecha=="2022-06-01") %>%
#   mutate(
#     var_id = case_when(
#       var_id == "Monterrey" ~ "Monterrey",
#       var_id == "nacional" ~ "Nacional"
#     ),
#     var = str_wrap(case_when(
#       var == "bp4_1_1" ~ "Intimidación sexual",
#       var == "bp4_1_2" ~ "Violación o intento de violación",
#       var == "bp4_1_3" ~ "Acoso sexual",
#       var == "bp4_1_4" ~ "Intimidación sexual",
#       var == "bp4_1_5" ~ "Violación o intento de violación",
#       var == "bp4_1_6" ~ "Abuso sexual",
#       var == "bp4_1_7" ~ "Abuso sexual",
#       var == "bp4_1_8" ~ "Intimidación sexual",
#       var == "bp4_1_9" ~ "Abuso sexual",
#     ), 20)
#   ) %>%
#   group_by(var_id, var) %>%
#   summarise(por=sum(por))-> df_graf
#
# ggplot(
#   df_graf,
#   aes(x = por, y = reorder(var, por), fill = var_id, group = var_id)
# ) +
#   geom_bar(stat = "identity", position = "dodge2") +
#   geom_text(aes(label = percent(por, accuracy = 0.1)),
#             fontface = "bold", family = "Montserrat", size = 5, hjust = -0.5,
#             position = position_dodge2(width = 0.9)) +
#   ggtitle("Tipo de acoso y violencia sexual 2T-2022 \n(Nacional contra ZM de Monterrey)",
#           "% de población mayor de 18 años sufrio algún tipo de\nviolencia sexual en los últimos 6 meses") +
#   labs(y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
#   tema_euzen() +
#   scale_fill_manual(values = c(met.brewer(name = "OKeeffe1", n = 2)[-3:-4], "grey20")) +
#   scale_x_continuous("% de población", labels = scales::percent_format(accuracy = 1L), limits = c(0, .4)) +
#   theme(
#     legend.title = element_blank(),
#     legend.position = "bottom"
#   ) -> graf
#
# ggsave(paste0(path_output, "ensu_tipo_sexual.png"), graf,
#        width = 12, height = 12)



# Serie comparativa entre VICTIMAS; PERCEP Y ATESTI

df_atesti %>%
  filter(var != "Consumo de alcohol en las calles") %>%
  group_by(var_id, fecha) %>%
  summarise(mean_ates = mean(por)) %>%
  left_join(df_percp %>%
    select(-c(bp1_1, total, var, trim)) %>%
    rename(percep = por), by = c("var_id", "fecha")) -> prueba

prueba %>%
  left_join(df_vict %>%
    filter(vict_var == "Hogares con\nvíctima") %>%
    select(-c(vict_var, por_cv, total, total_cv, mesano)) %>%
    rename(vict = por), by = c("var_id", "fecha")) -> p1

p1 %>%
  pivot_longer(cols = c(mean_ates, percep, vict), names_to = "var", values_to = "value") -> p2

p2 %>%
  mutate(
    var = recode(var,
      "mean_ates" = "Atestiguación de delitos (promedio)",
      "percep" = "Percepción de inseguridad",
      "vict" = "Hogares con víctimas"
    ),
    fecha = as.yearqtr(fecha)
  ) -> df_smash

unique(df_smash$var)
graf <- ggplot(
  df_smash,
  aes(x = fecha, y = value, color = var)
) +
  geom_line(show.legend = F, alpha = .8, size = 2) +
  geom_line(
    data = df_smash %>%
      filter(
        var == "Hogares con víctimas",
        !is.na(value)
      ),
    aes(x = fecha, y = value, colour = var),
    show.legend = F, alpha = .8, size = 2
  ) +
  geom_point(alpha = .8, size = 4) +
  geom_smooth(method = "lm", se = F, show.legend = F) +
  geom_text_repel(aes(label = percent(value, accuracy = 0.1)),
    fontface = "bold", family = "Montserrat", size = 8,
    angle = 90, force = 3, nudge_y = 0.1,
    show.legend = F
  ) +
  ggtitle(
    "Percepcion de inseguridad, atestiguación de delitos y victimas",
    "Monterrey, 2T-2023"
  ) +
  labs(caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 45, font_var = "Roboto") +
  scale_alpha_manual(values = c(0.8, 0.6)) +
  scale_color_manual(values = c(rev(met.brewer(name = "Signac", n = 7)[c(1, 4, 7)]), "grey20")) +
  scale_x_yearqtr("Trimestre", format = "T%q-%y", n = 10) +
  scale_y_continuous("% de población (% de hogares)", labels = scales::percent_format(accuracy = 1L), limits = c(.1, .9)) + # limits = c(min(temp$por), max(temp$por + 0.025)) +
  guides(alpha = "none") +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

# graf
#
ggsave(paste0(path_output, "ensu_compilado_1.png"), graf,
  width = 18, height = 10
)
# ggsave(paste0(path_output, "ensu_compilado.png"), graf,
#        width = 16, height = 10)
