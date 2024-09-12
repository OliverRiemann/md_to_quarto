pacman::p_load(
  tidyverse, scales, lubridate, janitor, cowplot, ggrepel, numform, leaflegend, htmltools, ggpol, rio, viridis, tableHTML, openxlsx, cartography,
  RPostgreSQL, yaml, sf, grid, gridExtra, leaflet, rpostgis, googlesheets4, sp, treemapify, ggbeeswarm, ggthemes, shapefiles, stargazer, rgeos, biscale,
  ggalluvial, htmltools, htmlwidgets, stringr, reshape2, gt, wordcloud, SnowballC, tm, cluster, rgdal, grid, survey, MetBrewer
)

font_add_google("Montserrat")
showtext_auto()
Sys.setlocale(locale = "es_ES.UTF-8")

source("tema_euzen.R")

path_output <- "03_grafs/"
path_temp <- "04_temp/"

options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "adjust")

ensu <- foreign::read.dbf("01_input/DATOS_ENSU/CB/ENSU_CB_1222.dbf") %>%
  clean_names() %>%
  mutate(
    var_id = "nacional",
    var_cd = ifelse(cve_ent == "19" &
      cve_mun %in% c("039"),
    "Monterrey", "otro"
    )
  )

files <- list.files(
  path = "01_input", pattern = "CB.*\\.dbf",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)
files_temp <- files[(grep("18\\.|19\\.|20\\.|21\\.|22\\.|23\\.|24", files, ignore.case = T))]
ensu_full <- map(files_temp, function(x) {
  print(x)

  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

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
    clean_names() %>%
    mutate(
      fac_sel = as.numeric(fac_sel),
      var_id = "nacional",
      var_zm = ifelse(cve_ent == "19" &
        cve_mun %in% c("039"),
      "Monterrey", "otro"
      )
    ) -> temp

  print(nrow(temp[temp$var_zm == "Monterrey", ]))

  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)


  vars <- c("var_id", "var_zm")

  funion <- function(z) {
    as_tibble(svyby(~bp3_2, by = as.formula(paste0("~", z)), design, svytotal, na.rm = T, vartype = "cvpct"))
  }

  output <- map(vars, funion)

  output %>%
    reduce(full_join) %>%
    select(-matches("cv")) %>%
    pivot_longer(
      cols = matches("bp3"),
      names_to = "var",
      values_to = "total"
    ) %>%
    mutate(
      var_id = coalesce(var_zm, var_id),
      resp = str_sub(var, nchar(var)),
      var = substr(var, 1, nchar(var) - 1)
    ) %>%
    filter(var_id != "otro") %>%
    select(-matches("zm")) %>%
    group_by(var_id) %>%
    mutate(
      tot = sum(total, na.rm = T),
      por = total / tot,
      fecha = as.Date(
        paste("01", substr(fecha, start = 1, stop = 2),
          substr(fecha, start = 3, stop = 4),
          sep = "-"
        ),
        format = "%d-%m-%y"
      ),
      resp = factor(case_when(
        resp == "1" ~ "Muy o algo efectivo",
        resp == "2" ~ "Muy o algo efectivo",
        resp == "3" ~ "Poco o nada efectivo",
        resp == "4" ~ "Poco o nada efectivo",
        resp == "9" ~ "Ns/Nc"
      ), levels = c(
        "Muy o algo efectivo",
        "Poco o nada efectivo",
        "Ns/Nc"
      ))
    ) %>%
    ungroup() -> data
})

df_graf <- ensu_full %>%
  reduce(full_join) %>%
  mutate(
    var_id = case_when(
      var_id == "Monterrey" ~ "Monterrey",
      var_id == "nacional" ~ "Nacional"
    )
  ) %>%
  group_by(var_id, resp, fecha) %>%
  summarise(por = sum(por)) %>%
  filter(
    resp != "Ns/Nc",
    resp != "Poco o nada efectivo",
    fecha > as_date("2018-03-02"),
  ) %>%
  mutate(
    trim = as.yearqtr(fecha)
  ) %>%
  dplyr::ungroup() %>%
  group_by(fecha) %>%
  mutate(
    nudge = ifelse(
      por == max(por),
      0.05, -0.05
    )
  )

graf <- ggplot(
  df_graf,
  aes(
    x = trim,
    y = por,
    color = var_id
  )
) +
  geom_line(
    size = 2,
    alpha = .5,
    show.legend = FALSE
  ) +
  geom_point(alpha = 0.6, size = 4) +
  geom_text_repel(aes(label = percent(por, accuracy = 0.1)),
    fontface = "bold", family = "Montserrat",
    size = 7.5,
    direction = "y",
    show.legend = F,
    force = 10,
    nudge_y = df_graf$nudge
  ) +
  ggtitle(
    "Percepción de efectividad del gobierno para resolver los problemas de la ciudad",
    "% de población mayor de 18 años que contestaron, 1T-2018 a 3T-2023"
  ) +
  labs(caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 35, font_var = "Roboto", lineheight_var = 0.4) +
  # facet_wrap(~var_id, ncol = 1) +
  scale_color_manual(values = c((met.brewer(name = "OKeeffe1", n = 2)))) +
  scale_x_yearqtr("", format = "T%q-%y", n = 12) +
  scale_y_continuous("% de población que contestaron",
    labels = scales::percent_format(accuracy = 1L),
    limits = c(0, max(df_graf$por) * 1.075)
  ) +
  guides(size = "none") +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave(paste0(path_output, "ensu_efectividad_P.png"), graf,
  width = 18, height = 8.5
)

#* Serie de tiempo confianza en autoridades de SP
#*

files_temp <- files[(grep("1221|0322|0622|0922|1222|0323|0623|0923|1223|24", files, ignore.case = T))]
ensu_full <- map(files_temp, function(x) {
  print(x)

  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")


  temp <- foreign::read.dbf(x) %>%
    clean_names()

  temp <- temp %>%
    mutate(
      var_cd =
        ifelse(cve_ent == "19" &
          cve_mun %in% c("039"),
        "Monterrey", "otro"
        )
    )

  design <- svydesign(
    ids = ~upm_dis,
    weights = ~fac_sel,
    strata = ~est_dis,
    data = temp
  )

  output <- svyby(
    ~ bp1_9_1 + bp1_9_2 + bp1_9_3 +
      bp1_9_4 + bp1_9_5,
    by = ~var_cd,
    design,
    svymean,
    vartype = "cvpct",
    na.rm = T
  ) %>% as_tibble()

  data <- output %>%
    dplyr::select(-matches("cv")) %>%
    pivot_longer(
      cols = matches("bp1"),
      names_to = "var",
      values_to = "total"
    ) %>%
    mutate(
      resp = str_sub(var, nchar(var)),
      var = substr(var, 1, nchar(var) - 1)
    ) %>%
    group_by(var, var_cd) %>%
    mutate(
      tot = sum(total, na.rm = T),
      por = total / tot,
      fecha = as.Date(
        paste("01", substr(fecha, start = 1, stop = 2),
          substr(fecha, start = 3, stop = 4),
          sep = "-"
        ),
        format = "%d-%m-%y"
      ),
      var = case_when(
        var == "bp1_9_1" ~ "Policía Preventiva Municipal",
        var == "bp1_9_2" ~ "Policía Estatal",
        var == "bp1_9_3" ~ "Guardia Nacional",
        var == "bp1_9_4" ~ "Ejército",
        var == "bp1_9_5" ~ "Marina"
      ),
      var = factor(var, levels = c(
        "Policía Preventiva Municipal",
        "Policía Estatal",
        "Guardia Nacional",
        "Ejército",
        "Marina"
      )),
      resp = factor(case_when(
        resp == "1" ~ "Confía",
        resp == "2" ~ "Confía",
        resp == "3" ~ "Desconfía",
        resp == "4" ~ "Desconfía",
        resp == "9" ~ "Ns/Nc"
      ), levels = c(
        "Desconfía", "Confía", "Ns/Nc"
      ))
    ) %>%
    group_by(var_cd, var, resp, fecha) %>%
    dplyr::summarise(por = sum(por)) %>%
    filter(resp == "Confía", var_cd != "otro")

  return(data)
})

df_graf <- ensu_full %>%
  reduce(full_join) %>%
  mutate(
    fecha = zoo::as.yearmon(fecha, format = "%b/%Y")
  )

write_rds(df_graf, file = "04_temp/df_confianza_sp.rds")
#* Barras: Confianza autoridades de SP
graf <- ggplot(
  df_graf,
  aes(x = reorder(var, -por), y = por, fill = as.factor(fecha), group = as.factor(fecha))
) +
  geom_bar(stat = "identity", position = "dodge2") +
  geom_text(aes(label = percent(por, accuracy = 0.1), y = por - .2),
    fontface = "bold", family = "Montserrat", size = 6, hjust = -0.1, angle = 90, col = "white",
    position = position_dodge2(width = 0.9),
  ) +
  ggtitle(
    "Confianza en autoridades de Seguridad Pública",
    ""
  ) +
  labs(
    x = "", y = "% de personas que confían",
    caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI"
  ) +
  tema_euzen(size_var = 28, font_var = "Roboto") +
  scale_fill_manual(values = met.brewer(name = "Renoir")) +
  scale_y_continuous("% de personas que confían", labels = scales::percent_format(accuracy = 1L), limits = c(0, max(df_graf$por) + 0.025)) +
  guides(
    fill = guide_legend(reverse = F, nrow = 1),
    color = "none"
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggsave(paste0(path_output, "ensu_confianza_serie_P.png"), graf,
  width = 18, height = 9
)
# ggsave(paste0(path_output, "ensu_confianza_serie.png"), graf,
#        width = 18, height = 9)

#* Serie de tiempo confianza en autoridades de SP
#*
files <- list.files(
  path = "01_input/ENSU/bases/",
  pattern = "CB*.dbf",
  full.names = T, recursive = T
)

(files_temp <- files[(grepl("24|23.dbf|22.dbf|21.dbf|20.dbf|19.dbf", 
                           files, ignore.case = T))])
ensu_full <- map(files_temp, function(x) {
  print(x)

  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  temp <- foreign::read.dbf(x) %>%
    clean_names()

  temp <- temp %>%
    mutate(
      var_cd = ifelse(cve_ent == "19" &
        cve_mun %in% c("039"),
      "Monterrey", "otro"
      )
    )

  design <- svydesign(
    ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, 
    data = temp
    )

  output <- as_tibble(svyby(
    ~ bp1_9_1 + bp1_9_2 + bp1_9_3 +
      bp1_9_4 + bp1_9_5,
    by = ~var_cd, design,
    svymean, vartype = "cvpct", na.rm = T
  ))

  data <- output %>%
    select(-matches("cv")) %>%
    pivot_longer(
      cols = matches("bp1"),
      names_to = "var",
      values_to = "total"
    ) %>%
    mutate(
      resp = str_sub(var, nchar(var)),
      var = substr(var, 1, nchar(var) - 1)
    ) %>%
    group_by(var, var_cd) %>%
    mutate(
      tot = sum(total, na.rm = T),
      por = total / tot,
      fecha = as.Date(
        paste("01", substr(fecha, start = 1, stop = 2),
          substr(fecha, start = 3, stop = 4),
          sep = "-"
        ),
        format = "%d-%m-%y"
      ),
      var = case_when(
        var == "bp1_9_1" ~ "Policía Preventiva Municipal",
        var == "bp1_9_2" ~ "Policía Estatal",
        var == "bp1_9_3" ~ "Guardia Nacional",
        var == "bp1_9_4" ~ "Ejército",
        var == "bp1_9_5" ~ "Marina"
      ),
      var = factor(var, levels = c(
        "Policía Preventiva Municipal",
        "Policía Estatal",
        "Guardia Nacional",
        "Ejército",
        "Marina"
      )),
      resp = factor(case_when(
        resp == "1" ~ "Confía",
        resp == "2" ~ "Confía",
        resp == "3" ~ "Desconfía",
        resp == "4" ~ "Desconfía",
        resp == "9" ~ "Ns/Nc"
      ), levels = c(
        "Desconfía", "Confía", "Ns/Nc"
      ))
    ) %>%
    dplyr::ungroup() %>% 
    group_by(var_cd, var, resp, fecha) %>%
    summarise(por = sum(por)) %>%
    filter(resp == "Confía",
           var_cd != "otro")
  
  return(data)
})


df_graf <- ensu_full %>%
  reduce(full_join) %>%
  filter(var == "Policía Preventiva Municipal") %>%
  mutate(
    trim = as.yearqtr(fecha)
  )

write_rds(df_graf, "04_temp/df_confianza_policia.rds")
# Serie de tiempo----
graf <- ggplot(
  df_graf,
  aes(x = trim, y = por, col = var)
) +
  geom_line(aes(group = var),
    alpha = 0.6, size = 1.5,
    show.legend = FALSE
  ) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = percent(por, accuracy = 0.1)),
    angle = 90, size = 10, fontface = "bold", family = "Montserrat", 
    hjust = -0.75,
    segment.size = 0.1, show.legend = F, force = 2
  ) +
  ggtitle(
    "Confianza en la Policía Preventiva Municipal",
    "% de la poblacion mayor de 18 años que confía, 3T 2023"
  ) +
  labs(x = "Trimestre", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 35, font_var = "Roboto") +
  scale_color_manual(values = c(
    wesanderson::wes_palette(name = "FantasticFox1", n = 5)[1],
    wesanderson::wes_palette(name = "FantasticFox1", n = 5)[3]
  )) +
  scale_x_yearqtr(format = "%qT-%y", n = 10) +
  scale_y_continuous("% de población",
    labels = scales::percent_format(accuracy = 1L),
    limits = c(
      NA,
      max(df_graf$por)*1.12
    )
  ) +
  guides(col = guide_legend(title = "")) +
  theme(
    legend.position = "top"
  )

ggsave(paste0(path_output, "ensu_conf_ppm.png"), graf,
  width = 18, height = 9
)


#* Serie Corrupción
#*
#*

files <- list.files(
  path = "01_input", pattern = "CB.*\\.dbf",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

files_temp <- files[(grepl("19\\.|20\\.|21\\.|22\\.|23\\.|24\\.", files, ignore.case = T))]
files_temp <- files_temp[(grepl("\\_062(\\d+)|\\_122(\\d+)", files_temp, ignore.case = T))]


ensu_full <- map(files_temp, function(x) {
  # x <- files_temp[7]

  print(x)

  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  temp <- foreign::read.dbf(x) %>%
    clean_names()

  temp <- temp %>%
    mutate(
      var_id = "nacional",
      var_cd = ifelse(cve_ent == "19" &
        cve_mun %in% c("039"),
      "Monterrey", "otro"
      )
    )

  print(length(temp$var_cd[temp$var_cd == "Monterrey"]))

  design <- svydesign(
    ids = ~upm_dis,
    weights = ~fac_sel,
    strata = ~est_dis, data = temp
  )

  vars <- c("var_id", "var_cd")

  funion <- function(z) {
    as_tibble(svyby(~bp3_6,
      by = as.formula(paste0("~", z)),
      design,
      svytotal,
      na.rm = T,
      vartype = "cvpct"
    ))
  }

  output <- map(vars, funion)

  df_graf <- output %>%
    reduce(full_join) %>%
    mutate(var_id = coalesce(var_id, var_cd)) %>%
    filter(var_id %in% c("nacional", "Monterrey")) %>%
    select(-matches("cv"), -var_cd) %>%
    pivot_longer(
      cols = matches("bp3_6"),
      names_to = "var",
      values_to = "total"
    ) %>%
    mutate(
      resp = str_sub(var, nchar(var)),
      var = substr(var, 1, nchar(var) - 1)
    ) %>%
    group_by(var, var_id) %>%
    mutate(
      tot = sum(total, na.rm = T),
      por = total / tot,
      resp = factor(case_when(
        resp == "1" ~ "Sí",
        resp == "2" ~ "No",
        T ~ "Ns/Nc"
      ), levels = c(
        "Sí", "No", "Ns/Nc"
      )),
      fecha = as.Date(
        paste("01", substr(fecha, start = 1, stop = 2),
          substr(fecha, start = 3, stop = 4),
          sep = "-"
        ),
        format = "%d-%m-%y"
      ),
      var_id = recode(var_id,
        "Monterrey" = "Monterrey",
        "nacional" = "Nacional"
      )
    )

  return(df_graf)
})


df_graf <- ensu_full %>%
  reduce(rbind) %>%
  filter(resp == "Si") %>%
  mutate(trim = as.yearqtr(fecha)) %>%
  dplyr::ungroup() %>%
  group_by(fecha) %>%
  mutate(
    nudge = ifelse(
      por == max(por),
      0.02, -0.02
    )
  )

graf <- df_graf %>%
  ggplot(
    aes(x = trim, y = por, col = var_id)
  ) +
  geom_line(
    aes(group = var_id),
    size = 3.5, alpha = 0.6,
    show.legend = FALSE
  ) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = percent(por, accuracy = 0.1)),
    size = 10,
    fontface = "bold",
    family = "Montserrat",
    direction = "y",
    nudge_y = df_graf$nudge,
    segment.size = 1,
    show.legend = F,
    point.padding = 2
  ) +
  ggtitle(
    "Corrupción por parte de autoridades de Seguridad Pública",
    "% población ha estado involucrado en corrupción en los últimos 6 meses, 4T-2023"
  ) +
  labs(x = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 40, font_var = "Roboto", lineheight_var = 0.2) +
  scale_color_manual(values = c(
    wesanderson::wes_palette(name = "FantasticFox1", n = 5)[1],
    wesanderson::wes_palette(name = "FantasticFox1", n = 5)[3]
  )) +
  scale_x_yearqtr(format = "%qT-%y", n = 7) + # limit=as.yearqtr(as.Date(c("2019-03-01", "2022-06-01")))
  scale_y_continuous("% de población", labels = scales::percent_format(accuracy = 1L)) +
  guides(col = guide_legend(title = "")) +
  theme(
    legend.position = "top"
  )


ggsave(paste0(path_output, "ensu_corruphist.png"), graf,
  width = 18, height = 8.5
)



### Barras de último trimestre
#
#
# ensu_full %>%
#   reduce(full_join) %>%
#   mutate(
#     var_id = case_when(
#       var_id == "Monterrey" ~ "Monterrey",
#       var_id == "nacional" ~ "Nacional"
#     )) %>%
#   group_by(var_id, resp, fecha) %>%
#   summarise(por=sum(por)) %>%
#   filter(fecha == "2022-09-01") -> df_graf
#
#
# ggplot(df_graf,
#        aes(x = var_id,
#            y=por,
#            fill=fct_rev(resp)))+
#   geom_bar(stat = "identity", position = position_fill()) +
#   geom_text(aes(label=ifelse(por<.1, NA, percent(por, .1))),
#             position = position_fill(vjust = .5),
#             fontface = "bold", family = "Montserrat", size = 4,
#             show.legend = F)+
#   labs(title="Percepción de efecividad del gobierno para resolver los problemas \nde la ciudad en el 4T-2021 (Nacional contra ZM de Monterrey)",
#        subtitle = "% de población mayor de 18 anos que contestaron",
#        x="") +
#   labs(caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
#   tema_euzen(size_var = 24, font_var = "Roboto") +
#   scale_fill_manual(values = c( "grey20", met.brewer(name = "OKeeffe1", n = 2))) +
#   scale_y_continuous("% de población que contestaron",
#                      labels = scales::percent_format(accuracy = 1L)) +
#   theme(legend.position = "top",
#         legend.title = element_blank())-> graf
#
# ggsave(paste0(path_output, "ensu_efectividad_ulttrim.png"), graf,
#        width = 8, height = 10)
#

#
# files <- list.files(path = "01_input/ENSU/bases/", pattern = "CB*.dbf", full.names = T, recursive = T)
#
# files_temp <- files[(grep("18\\.|19\\.|20\\.|21\\.|22\\.|23\\.", files, ignore.case = T))];
#
# ensu_full <- map(files_temp, function(x) {
#
#   print(x)
#
#     fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")
#
#   if (grepl("15\\.", x, ignore.case = T)) {
#
#     foreign::read.dbf(x) %>%
#       clean_names() %>%
#       rename(cve_ent = ent,
#              fac_sel = factor,
#              est_dis = edis,
#              bp1_1 = p1) -> temp
#
#   } else if (grepl("0316|0616|0916", x, ignore.case = T)) {
#
#     foreign::read.dbf(x) %>%
#       clean_names() %>%
#       rename(cve_ent = ent,
#              cve_mun= mun) -> temp
#
#   } else {
#
#     foreign::read.dbf(x) %>%
#       clean_names() -> temp
#
#   }
#
#   temp %>%
#     clean_names() %>%
#     mutate(
#       fac_sel = as.numeric(fac_sel),
#       var_id = "nacional",
#       var_zm = ifelse(cve_ent == "19" &
#                         cve_mun %in% c("039"),
#                       "Monterrey", "otro")
#     ) -> temp
#
#   print(nrow(temp[temp$var_zm=="Monterrey",]))
#
#   design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
#
#
#   vars <- c("var_id", "var_zm")
#
#   funion <- function(z) {
#
#     as_tibble(svyby(~bp1_8_1, by = as.formula(paste0("~", z)), design, svytotal, na.rm = T, vartype = "cvpct"))
#
#   }
#
#   output <- map(vars, funion)
#
#   output %>%
#     reduce(full_join) %>%
#     select(-matches("cv")) %>%
#     pivot_longer(cols = matches("bp1_8_1"),
#                  names_to = "var",
#                  values_to = "total") %>%
#     mutate(var_id = coalesce(var_zm, var_id),
#            resp = str_sub(var, nchar(var)),
#            var = substr(var, 1, nchar(var) - 1)) %>%
#     filter(var_id != "otro") %>%
#     select(-matches("zm")) %>%
#     group_by(var_id) %>%
#     mutate(
#       tot = sum(total, na.rm = T),
#       por = total / tot,
#       fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2),
#                             substr(fecha, start = 3, stop = 4), sep = "-"),
#                       format = "%d-%m-%y"),
#       resp = factor(case_when(
#         resp == "1" ~ "Muy o algo efectivo",
#         resp == "2" ~ "Muy o algo efectivo",
#         resp == "3" ~ "Poco o nada efectivo",
#         resp == "4" ~ "Poco o nada efectivo",
#         resp == "9" ~ "Ns/Nc"
#       ), levels = c("Muy o algo efectivo",
#                     "Poco o nada efectivo",
#                     "Ns/Nc")
#       )
#
#     ) %>%
#     ungroup()  -> data
# })
#
# ensu_full %>%
#   reduce(full_join) %>%
#   mutate(
#     var_id = case_when(
#       var_id == "Monterrey" ~ "Monterrey",
#       var_id == "nacional" ~ "Nacional"
#     )) %>%
#   group_by(var_id, resp, fecha) %>%
#   summarise(por=sum(por)) %>%
#   filter(resp!="Ns/Nc",
#          resp!="Poco o nada efectivo",
#          fecha > as_date("2018-03-02"),
#   ) %>%
#   mutate(
#     trim = as.yearqtr(fecha)
#   )-> df_graf
#
# ggplot(
#   df_graf,
#   aes(x=trim,
#       y=por,
#       color=var_id))+
#   # geom_rect(aes(xmin = as.Date("2022-01-02"), xmax =  as.Date("2022-10-01"), ymin = 0, ymax = 1),
#   #           alpha = .25, show.legend = F, color=NA)+
#   geom_line(size=2, alpha = .5) +
#   geom_point(alpha = 0.6, size = 4) +
#   # geom_line(stat = "smooth",method = "lm", size = 1.5, linetype ="dashed", alpha = 0.5) +
#   geom_text_repel(aes(label = percent(por, accuracy = 0.1)),
#                   fontface = "bold", family = "Montserrat", size = 6,
#                   show.legend = F, nudge_y = -.02) +
#
#   ggtitle("Percepción de efecividad de la Policía Municipal",
#           "% de población mayor de 18 años que contestaron, 1T-2018 a 2T-2023") +
#   labs(caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
#   tema_euzen(size_var = 28, font_var = "Roboto", lineheight_var = 0.4) +
#   # facet_wrap(~var_id, ncol = 1) +
#   scale_color_manual(values = c((met.brewer(name = "OKeeffe1", n = 2)))) +
#   scale_x_yearqtr("Trimestre", format = "T%q-%y", n  = 6) +
#   scale_y_continuous("% de población que contestaron",
#                      labels = scales::percent_format(accuracy = 1L)) +
#   guides(size = "none") +
#   theme(legend.position = "top",
#         legend.title = element_blank()) -> graf
#
# ggsave(paste0(path_output, "ensu_efectividad_Mun.png"), graf,
#        width = 18, height = 8)
