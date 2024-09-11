pacman::p_load(
  pacman
)
# 3.6 bp3_6 En los últimos seis meses, ¿la policía u otras autoridades de seguridad pública le insinuó, le pidió de forma directa o generó
# las condiciones para que les diera dinero, un regalo o favor para agilizar, aprobar, o bien, evitar infracciones o detenciones?
#   [trimestre II y IV]
files <- "01_input/ENSU/bases/ensu_bd_2024_dbf/ensu_bd_junio_2024_dbf/ENSU_CB_0624.dbf"

#* Limpiar bases----
ensu_full <- map(files, function(x) {
  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  temp <- foreign::read.dbf(x) %>%
    clean_names()

  temp %>%
    mutate(
      fac_sel = as.numeric(fac_sel),
      var_id = "nacional",
      var_cd = case_when(
        cve_ent == "19" & cve_mun == "039" ~ "Monterrey",
        .default = "otro"
      )
    ) %>%
    select(
      var_id, var_cd, upm_dis,
      est_dis, fac_sel, bp3_6
    ) %>%
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>%
    filter(!is.na(bp3_6)) -> temp2

  vars <- list(list("var_id", "bp3_6"), list("var_cd", "bp3_6"))

  map(vars, function(v) {

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
      var_id = case_when(
        var_id == "Monterrey" ~ "Monterrey",
        var_id == "nacional" ~ str_to_title(var_id)
      ),
      bp3_6 = case_when(
        bp3_6 == 1 ~ "Sí",
        bp3_6 == 2 ~ "No"
      ),
      fecha = as.Date(
        paste("01", substr(fecha_var, start = 1, stop = 2),
          substr(fecha_var, start = 3, stop = 4),
          sep = "-"
        ),
        format = "%d-%m-%y"
      )
    ) %>%
    drop_na(var_id) %>%
    select(-matches("cv|var_cd")) -> data
})

ensu_full %>%
  reduce(full_join) %>%
  mutate(trim = zoo::as.yearqtr(fecha)) -> df_percp


ggplot(
  df_percp,
  aes(x = por, y = fct_rev(as.factor(var_id)), fill = fct_rev(bp3_6))
) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = df_percp %>% 
              filter(por > 0.04),
            aes(label = percent(por, accuracy = 0.1)),
    fontface = "bold", family = "Montserrat", size = 7, color = "white",
    position = position_stack(vjust = 0.5), show.legend = F
  ) +
  ggtitle("Confianza en autoridades de Seguridad Pública", "% de personas mayores de 18 años que contestaron, 2T 2024") +
  labs(y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 28, font_var = "Roboto", lineheight_var = .77) +
  scale_alpha_manual(values = c(0.9, 0.6)) +
  scale_fill_manual(values = rev(met.brewer(name = "Signac", n = 5)[c(-2, -4)])) +
  scale_color_manual(values = c("white", "transparent")) +
  scale_x_continuous("% de población", labels = scales::percent_format(accuracy = 1L)) +
  guides(fill = guide_legend(reverse = T)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) -> graf


ggsave(paste0(path_output, "ensu_corrupcion.png"), graf,
       width = 11, height = 9)



# confianza en autoridades
pacman::p_load(
  tidyverse, scales, lubridate, janitor, cowplot, ggrepel, numform, leaflegend, htmltools, ggpol, rio, viridis, tableHTML, openxlsx, cartography,
  RPostgreSQL, yaml, sf, grid, gridExtra, leaflet, rpostgis, googlesheets4, sp, treemapify, ggbeeswarm, ggthemes, shapefiles, stargazer, rgeos, biscale,
  ggalluvial, htmltools, htmlwidgets, stringr, reshape2, gt, wordcloud, SnowballC, tm, cluster, rgdal, grid, survey, MetBrewer, srvyr, shadowtext
)

Sys.setlocale(locale = "es_ES.UTF-8")

source("../../01. SCRIPTS/tema_euzen.R")

path_output <- "03_grafs/"
path_temp <- "04_temp/"

options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "adjust")

files <- list.files(path = "01_input/DATOS_ENSU/CB", pattern = "*.dbf", full.names = T, recursive = T)

#* Serie de tiempo confianza en autoridades de SP
#*
pacman::p_load(shadowtext)
(files_temp <- files[(grep("0324|0624", files, ignore.case = T))])

ensu_full <- map(files_temp, function(x) {
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

  design <- svydesign(
    ids = ~upm_dis,
    weights = ~fac_sel,
    strata = ~est_dis,
    data = temp
  )

  vars <- c("var_id", "var_cd")

  funion <- function(z) {
    svyby(
      ~ bp1_9_1 + bp1_9_2 + bp1_9_3 +
        bp1_9_4 + bp1_9_5,
      by = as.formula(paste0("~", z)), design,
      svytotal, na.rm = T,
      vartype = "cvpct"
    ) %>%
      as_tibble()
  }


  output <- map(vars, funion)

  data <- output %>%
    reduce(full_join) %>%
    select(-matches("cv")) %>%
    pivot_longer(
      cols = matches("bp1"),
      names_to = "var",
      values_to = "total"
    ) %>%
    mutate(
      var_id = coalesce(var_cd, var_id),
      resp = str_sub(var, nchar(var)),
      var = substr(var, 1, nchar(var) - 1)
    ) %>%
    filter(var_id != "otro") %>%
    select(-var_cd) %>%
    group_by(var, var_id) %>%
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
    group_by(var_id, var, resp, fecha) %>%
    summarise(por = sum(por)) %>%
    filter(var_id != "otro") %>%
    ungroup()

  return(data)
})

df_graf <- ensu_full %>%
  reduce(full_join) %>%
  mutate(
    fecha = zoo::as.yearmon(fecha, format = "%B/%Y"),
    var_id = str_to_title(var_id)
  )

write_rds(df_graf, "04_temp/df_confianza_sp_reciente.rds")

graf <- ggplot(
  df_graf,
  aes(
    x = por, y = fct_rev(as.factor(var_id)),
    fill = fct_rev(resp),
    group = fct_rev(var)
  )
) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    data = df_graf %>%
      filter(por >= 0.04),
    aes(label = percent(por, accuracy = 0.1)),
    color = alpha("white", 0.9),
    fontface = "bold", family = "Montserrat",
    size = 6,
    position = position_stack(vjust = 0.5),
    show.legend = F
  ) +
  facet_wrap(~var, nrow = 12) +
  ggtitle(
    "Confianza en autoridades de Seguridad Publica",
    "% de personas mayores de 18 años que contestaron, 2T 2024"
  ) +
  labs(y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 32, font_var = "Roboto", lineheight_var = .77) +
  scale_alpha_manual(values = c(0.9, 0.6)) +
  scale_fill_manual(values = rev(c(met.brewer(name = "OKeeffe1", n = 2)[-3:-4], "grey20"))) +
  scale_color_manual(values = c("white", "transparent")) +
  scale_x_continuous("% de población", labels = scales::percent_format(accuracy = 1L)) +
  guides(fill = guide_legend(reverse = T)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggsave(paste0(path_output, "ensu_confianza_spPRUEBA1.png"), graf,
  width = 9.5, height = 10
)
# ggsave(paste0(path_output, "ensu_confianza_sp.png"), graf,
#        width = 8, height = 9)

# tabla_programas BP3_2A
files <- list.files(
  path = "01_input", pattern = "CB.*\\.dbf",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)
#* Limpiar bases----
files_temp <- files[(grep("0923|0323|0623", files, ignore.case = T))]
#* Serie de tiempo confianza en autoridades de SP
#*

#* Limpiar bases----
map(files_temp, function(x) {
  # x = files[1]

  print(x)

  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  if (grepl("0323|0623|0923", x, ignore.case = T)) {
    foreign::read.dbf(x) %>%
      clean_names() %>%
      rename(
        CVE_ENT = cve_ent,
        CVE_MUN = cve_mun
      ) -> temp
  } else if (grepl("0316|0616|0916", x, ignore.case = T)) {
    foreign::read.dbf(x) %>%
      clean_names() %>%
      rename(
        CVE_ENT = ent,
        CVE_MUN = mun
      ) -> temp
  } else {
    foreign::read.dbf(x) %>%
      clean_names() -> temp
  }

  temp %>%
    mutate(
      fac_sel = as.numeric(fac_sel),
      var_id = "nacional",
      var_cd = case_when(
        cve_ent == "19" & CVE_MUN == "039" ~ "Monterrey",
        grepl("16$|17$|0318", fecha_var, ignore.case = T) & cd == 24 ~ "Monterrey",
        T ~ "otro"
      )
    ) %>%
    select(
      var_id, var_cd, upm_dis,
      est_dis, fac_sel, bp3_2a
    ) %>%
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>%
    filter(!is.na(bp3_2a)) -> temp2

  vars <- list(list("var_id", "bp3_2a"), list("var_cd", "bp3_2a"))

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
      var_id = case_when(
        var_id == "Monterrey" ~ "Monterrey",
        var_id == "nacional" ~ str_to_title(var_id)
      ),
      bp3_2a = factor(case_when(
        bp3_2a == 1 ~ "Sí",
        bp3_2a == 2 ~ "No",
        T ~ "Ns/Nc"
      ), levels = c("Sí", "No", "Ns/Nc")),
      fecha = as.Date(
        paste("01", substr(fecha_var, start = 1, stop = 2),
          substr(fecha_var, start = 3, stop = 4),
          sep = "-"
        ),
        format = "%d-%m-%y"
      )
    ) %>%
    drop_na(var_id) %>%
    select(-matches("cv|var_cd")) -> data
}) -> ensu_full

ensu_full %>%
  reduce(full_join) %>%
  mutate(trim = zoo::as.yearqtr(fecha)) -> df_percp

#* Tabla de Monterrey----
df_percp %>%
  filter(var_id == "Monterrey") %>%
  select(bp3_2a, total, por, fecha) %>%
  pivot_wider(
    names_from = fecha,
    values_from = c(total, por)
  ) %>%
  transmute(
    `Conocimiento programas` = bp3_2a,
    `Mar 2023` = paste0(percent(`por_2023-03-01`, accuracy = 0.1), " (", prettyNum(`total_2023-03-01`, big.mark = ","), ")"),
    `Jun 2023` = paste0(percent(`por_2023-06-01`, accuracy = 0.1), " (", prettyNum(`total_2023-06-01`, big.mark = ","), ")"),
    `Sept 2023` = paste0(percent(`por_2023-09-01`, accuracy = 0.1), " (", prettyNum(`total_2023-09-01`, big.mark = ","), ")"),
    `Diferencia % con trimestre anterior` = paste0(round((`por_2023-09-01` - `por_2023-06-01`) * 100, digits = 1), " pp"),
    dif_trim = `por_2023-09-01` - `por_2023-06-01`,
  ) %>%
  arrange(dif_trim) %>%
  select(-c(dif_trim)) -> p1

class(p1$`Conocimiento programas`)

p1 %>%
  mutate(`Conocimiento programas` = factor(`Conocimiento programas`,
    levels = c("Sí", "No", "Ns/Nc")
  )) %>%
  arrange(`Conocimiento programas`) -> p1
unique(p1$`Conocimiento programas`)
p1 %>%
  write_csv("04_temp/tabla_conoc_program1.csv")

tabla_conoc_program1 <- read_csv("04_temp/tabla_conoc_program1.csv")

tabla_programas <- read_csv("04_temp/tabla_conoc_program.csv")

# rev(c(met.brewer(name = "OKeeffe1", n = 2)[-3:-4], "grey20")
