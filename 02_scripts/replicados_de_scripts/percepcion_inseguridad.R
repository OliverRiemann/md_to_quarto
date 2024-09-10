# Setup -------------------------------------------------------------------
pacman::p_load(
  tidyverse,
  srvyr,
  janitor,
  zoo,
  MetBrewer,
  scales,
  ggrepel,
  sysfonts
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

files <- files[!grepl("15", files)]

Sys.setlocale(locale = "es_ES.UTF-8")

source("tema_euzen.R")

path_output <- "03_grafs/"
path_temp <- "04_temp/"


# Cálculo de proporciones y totales ---------------------------------------

survey_mean_total <- function(v, df = temp2) {
  print(v[[1]])

  df %>%
    group_by(across(all_of(v))) %>%
    summarise(
      por = survey_mean(vartype = "cv"),
      total = survey_total(vartype = "cv"),
      .groups = "drop"
    ) %>%
    mutate(var = v[[1]])
}

# Limpieza ----------------------------------------------------------------

ensu_full <- map(files, function(x) {
  # x = files[1]

  print(x)

  fecha_var <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")


  if (grepl("1216|0317|0617|0917|1217|0318|0920|0618|0918|1218|0319|0619|0919|1219|0320|1220|0321|0621|0921|1221|0322|0622|0922|1222|0323|0623|0923|1223|0324", x, ignore.case = T)) {
    temp <- foreign::read.dbf(x) %>%
      clean_names() %>%
      rename(
        CVE_ENT = cve_ent,
        CVE_MUN = cve_mun
      ) -> temp
  } else if (grepl("0316|0616|0916", x, ignore.case = T)) {
    temp <- foreign::read.dbf(x) %>%
      clean_names() %>%
      rename(
        CVE_ENT = ent,
        CVE_MUN = mun
      )
  } else {
    temp <- foreign::read.dbf(x) %>%
      clean_names()
  }

  temp2 <- temp %>%
    mutate(
      fac_sel = as.numeric(fac_sel),
      var_id = "nacional",
      var_gdl = case_when(
        CVE_ENT == "19" & CVE_MUN == "039" ~ "Guadalajara",
        grepl("16$|17$|0318", fecha_var, ignore.case = T) & cd == 24 ~ "Guadalajara",
        T ~ "otro"
      )
    ) %>%
    select(
      var_id, var_gdl, upm_dis,
      est_dis, fac_sel, bp1_1
    ) %>%
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>%
    filter(!is.na(bp1_1))

  vars <- list(c("var_id", "bp1_1"), c("var_gdl", "bp1_1"))

  output <- vars %>% map(\(vars) survey_mean_total(vars, temp2))

  data <- output %>%
    reduce(full_join) %>%
    mutate(
      var_id = coalesce(var_id, var_gdl),
      var_id = case_when(
        var_id == "Guadalajara" ~ "Guadalajara",
        var_id == "nacional" ~ str_to_title(var_id)
      ),
      bp1_1 = as.numeric(bp1_1),
      fecha = as.Date(
        paste("01", substr(fecha_var, start = 1, stop = 2),
          substr(fecha_var, start = 3, stop = 4),
          sep = "-"
        ),
        format = "%d-%m-%y"
      )
    ) %>%
    drop_na(var_id) %>%
    select(!matches("cv|var_gdl")) %>%
    filter(bp1_1 == "2")

  return(data)
})


df_percep <- ensu_full %>%
  reduce(rbind) %>%
  mutate(trim = zoo::as.yearqtr(fecha))


# Gráfica -----------------------------------------------------------------

graf <- ggplot(
  df_percep,
  aes(x = trim, y = por, col = var_id)
) +
  geom_line(aes(group = var_id),
    alpha = 0.6, size = 1.5
  ) +
  geom_point(alpha = 0.8, size = 2.5) +
  ggtitle(
    " % de la población mayor de 18 años que se siente insegura",
    "1T-2016 a 1T-2024"
  ) +
  labs(x = "Trimestre", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI\nNota: 1) Se omite el valor del segundo trimestre de 2020, por la pandemia ocasionada por el COVID-19\n2) Los datos previos al segundo trimestre de 2018 de Guadalajara son respecto a la Zona Metropolitana") +
  tema_euzen(size_var = 48, font_var = "Roboto", lineheight_var = .3) +
  scale_color_manual(values = c(
    wesanderson::wes_palette(name = "FantasticFox1", n = 5)[1],
    wesanderson::wes_palette(name = "FantasticFox1", n = 5)[3]
  )) +
  scale_x_yearqtr(format = "%Y-T%q", n = 8, limit = as.yearqtr(c("2016-01-01", "2023-06-01"))) +
  scale_y_continuous("% de población",
    labels = scales::percent_format(accuracy = 1L),
    limits = c(
      min(df_percp$por) - 0.025,
      max(df_percp$por) + 0.1
    )
  ) +
  guides(col = guide_legend(title = "")) +
  theme(
    legend.position = "top",
  ) +
  geom_text_repel(aes(label = percent(por, accuracy = 0.1)),
    angle = 90, size = 15, fontface = "bold", family = "Montserrat", nudge_y = .05,
    segment.size = 0.05, show.legend = F, force = 2
  )

ggsave(paste0(path_output, "ensu_perc2.png"), graf,
  width = 16, height = 8
)
