## Incidencia delictiva comparativa del 1 año contra el 2do año de octubre a sept 2022 a octubre septiembre 2023
summary(trimGdl)

pacman::p_load(
  tidyverse, tidyselect, zoo, readxl, ggrepel, scales, purrr, lubridate, janitor, zoo, cowplot, ggrepel, numform, leaflegend, htmltools, ggpol, doBy, viridis, foreign, reldist, cartography,
  RPostgreSQL, yaml, sf, grid, gridExtra, leaflet, rpostgis, googlesheets4, sp, treemapify, ggbeeswarm, ggthemes, shapefiles, stargazer, rgeos, biscale,
  ggalluvial, htmltools, htmlwidgets, stringr, reshape2, gt, wordcloud, SnowballC, tm, cluster, rgdal, grid, survey, MetBrewer, srvyr, readxl, sysfonts, showtext, zoo, scales, openxlsx
)

Sys.setlocale(locale = "es_ES.UTF-8")

source("tema_euzen.R")

path_output <- "03_grafs/"

font_add_google(name = "Poppins", family = "Poppins")
font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()

# Incidencia delictiva comparativa desde octubre de 2018


rangos <- data.frame(
  inicio = c(zoo::as.yearqtr("2016 Q1"), zoo::as.yearqtr("2018 Q4"), zoo::as.yearqtr("2021 Q4")),
  fin    = c(zoo::as.yearqtr("2018 Q4"), zoo::as.yearqtr("2021 Q4"), zoo::as.yearqtr("2023 Q4")),
  fill   = c("EAR", "IDT", "PLN") # Colores de sombreado
)

rangos <- tibble(
  inicio = c(zoo::as.yearqtr("2016 Q1"), zoo::as.yearqtr("2018 Q4"), zoo::as.yearqtr("2021 Q4")),
  fin    = c(zoo::as.yearqtr("2018 Q4"), zoo::as.yearqtr("2021 Q4"), zoo::as.yearqtr("2024 Q2")),
  fill   = c("EAR", "IDT", "PLN") # Colores de sombreado
)
summary(rangos$inicio)

graf <- df_tasa %>%
  filter(Ciudad == "Ciudad de Monterrey") %>%
  ggplot(
    aes(x = ID, y = tasa)
  ) +
  geom_point(size = 4, color = "#E0963F") +
  geom_line(size = 3, alpha = .7, color = "#E0963F") +
  geom_rect(
    data = rangos,
    aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf, fill = fill),
    alpha = 0.2, inherit.aes = FALSE
  ) +
  geom_text_repel(
    aes(label = round(tasa, 1)),
    fontface = "bold", family = "Montserrat",
    size = 7.5, color = "#E09669",
    hjust = -1.1, vjust = 0.5, angle = 90, segment.size = 0.08,
    nudge_y = ifelse(df_tasa$ID == "2016 Q3", -100, 0)
  ) +
  scale_fill_manual(
    values = c("EAR" = "#c04808", "IDT" = "#f8e078", "PLN" = "#f8b048"),
    name = ""
  ) +
  labs(
    title = "Comparativa Incidencia Delictiva Monterrey",
    subtitle = "1T-2016 al 2T-2024",
    x = "Trimestre",
    y = "Tasa por cada\n 100 mil hab.",
    caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal"
  ) +
  scale_x_yearqtr(
    "Trimestre",
    format = "%Y-T%q",
    n = 5
  ) +
  scale_y_continuous(
    labels = ~ prettyNum(., ","),
    limits = c(NA, 300)
  ) +
  tema_euzen(size_var = 35, lineheight_var = 0.5)

ggsave(paste0(path_output, "anexoComp1.png"), graf,
  width = 15, height = 8
)


# Rectángulos para la graf ------------------------------------------------


rangos1 <- data.frame(
  inicio = c(zoo::as.yearqtr("2021 Q3"), zoo::as.yearqtr("2022 Q3")),
  fin    = c(zoo::as.yearqtr("2022 Q3"), zoo::as.yearqtr("2023 Q4")),
  fill   = c("1° año", "2° año") # Colores de sombreado
)

agregado_mty_2021_2024 <- agregado_mty %>%
  separate_wider_delim(
    cols = "ID", delim = "T-", names = c("trim", "year")
  ) %>%
  mutate(
    trim = paste0(year, " Q", trim) %>%
      as.yearqtr(),
    tasa = (incidencia / poblacion) * 1e5
  ) %>%
  select(-year) %>%
  filter(trim >= "2021 Q4")

graf <- agregado_mty_2021_2024 %>%
  ggplot(aes(x = trim, y = tasa)) +
  geom_point(size = 4, color = "#E0963F") +
  geom_line(size = 3, alpha = .7, color = "#E0963F") +
  geom_rect(
    data = rangos1,
    aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf, fill = fill),
    alpha = 0.2, inherit.aes = FALSE
  ) +
  labs(
    title = str_wrap("Comparativa Incidencia Delictiva Monterrey, 1° y 2° año de gobierno"),
    subtitle = "4T-2021 al 2T-2024",
    x = "Trimestre", y = "Tasa por cada\n 100 mil hab.",
    caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal"
  ) +
  geom_text_repel(
    aes(label = round(tasa, 1)),
    fontface = "bold", family = "Montserrat", size = 10,
    color = "#E09669",
    hjust = 0.5, vjust = -4, segment.size = 0.5,
    nudge_y = case_when(
      agregado_mty_2021_2024$trim == "2022 Q3" ~ -22,
      .default = 0
    ),
    nudge_x = case_when(
      agregado_mty_2021_2024$trim == "2022 Q3" ~ -0.03,
      .default = 0
    )
  ) +
  scale_x_yearqtr(
    "Trimestre",
    format = "%Y-T%q",
    n = 10
  ) +
  scale_y_continuous(
    labels = ~ prettyNum(., ",")
  ) +
  scale_fill_manual(
    values = c("1° año" = "#53B8CF", "2° año" = "purple")
  ) +
  tema_euzen(size_var = 35, lineheight_var = 0.6)


ggsave(paste0(path_output, "anexoComp2.png"), graf,
  width = 15, height = 8
)


# Anexo Comp 3 ------------------------------------------------------------

alto_impacto_mty
alto_impacto_tasa <- left_join(
  ENOE_Monterrey,
  alto_impacto_mty,
  by = "ID"
) %>%
  mutate(
    tasa = (incidencia / poblacion) * 1e5
  )

annotate
## Graf anexo comp 3 -------------------------------------------------------

graf <- alto_impacto_tasa %>%
  ggplot(
    aes(x = trim, y = tasa)
  ) +
  geom_line(size = 3, alpha = .7, color = "#EF645B") +
  geom_point(size = 4, color = "#EF645B") +
  geom_text_repel(
    aes(label = round(tasa, 1)),
    fontface = "bold", family = "Montserrat",
    size = 7.5,
    direction = "y",
    color = "#EF645B",
    hjust = -1.1, vjust = 0.5, angle = 90,
    segment.size = 0.2,
    show.legend = F,
    nudge_y = case_when(
      alto_impacto_tasa$trim == "2016 Q1" ~ -25,
      alto_impacto_tasa$trim == "2016 Q2" ~ -25,
      alto_impacto_tasa$trim == "2016 Q3" ~ -25,
      .default = 0
    ),
    nudge_x = case_when(
      alto_impacto_tasa$trim == "2016 Q4" ~ 0.05,
      alto_impacto_tasa$trim == "2016 Q3" ~ -0.03,
      .default = 0
    )
  ) +
  geom_rect(
    data = rangos,
    aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf, fill = fill),
    alpha = 0.2, inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = c("EAR" = "#c04808", "IDT" = "#f8e078", "PLN" = "#f8b048")
  ) +
  labs(
    x = "Trimestre",
    y = "Tasa por cada\n 100 mil hab.",
    title = "Evolución de Incidencia Delictiva de Alto Impacto",
    subtitle = "En Monterrey, 1T 2016 a 4T 2024",
    caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal\n Nota: Los delitos de alto impacto incluyen homicidio doloso y culposo, lesiones por arma de fuego, feminicidio, violación, secuestro,\n extorsión, robo de vehículo, robo a casa habitación, robo a negocio con violencia, robo a transporte público colectivo,\n robo a transeúnte en vía pública y robo a transportista."
  ) +
  scale_x_yearqtr(
    "Trimestre",
    format = "%Y-T%q",
    n = 8,
  ) +
  scale_y_continuous(labels = comma_format(), limits = c(0, 90)) +
  tema_euzen(
    font_var = "Poppins", size_var = 35, lineheight_var = 0.6
  ) +
  theme(
    legend.position = "right",
    legend.title = element_blank()
  )

ggsave(paste0(path_output, "anexoComp3.png"), graf,
  width = 18, height = 10
)

## Graf anexo comp 4 -------------------------------------------------------
alto_impacto_tasa_2021_2024 <- alto_impacto_tasa %>%
  filter(trim >= "2021 Q4")
graf <- alto_impacto_tasa_2021_2024 %>%
  ggplot(
    aes(x = trim, y = tasa)
  ) +
  geom_line(size = 3, alpha = .7, color = "#EF645B") +
  geom_point(size = 4, color = "#EF645B") +
  geom_text_repel(
    aes(label = round(tasa, 1)),
    fontface = "bold",
    family = "Montserrat", size = 10, color = "#EF645B",
    hjust = 0.5, vjust = -3.5, segment.size = 0.5,
    show.legend = FALSE,
    direction = "y",
    nudge_y = case_when(
      alto_impacto_tasa_2021_2024$trim == "2022 Q3" ~ -7.5,
      .default = 0
    ),
    nudge_x = case_when(
      alto_impacto_tasa_2021_2024$trim == "2022 Q1" ~ -0.01,
      .default = 0
    )
  ) +
  geom_rect(data = rangos1, aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf, fill = fill), alpha = 0.2, inherit.aes = FALSE) +
  scale_fill_manual(values = c("1° año" = "#53B8CF", "2° año" = "purple")) +
  labs(
    x = "Trimestre",
    y = "Tasa por cada\n 100 mil hab.",
    title = "Evolución de Incidencia Delictiva de Alto Impacto",
    subtitle = "En Monterrey, 4T 2021 a 4T 2024"
  ) +
  labs(caption = paste(
    "Fuente: Secretariado de Seguridad Pública del Gobierno Federal \n",
    str_wrap("Nota: Los delitos de alto impacto incluyen homicidio doloso y culposo, lesiones por arma de fuego, feminicidio, violación, secuestro, extorsión, robo de vehículo, robo a casa habitación, robo a negocio con violencia, robo a transporte público colectivo, robo a transeúnte en vía pública y robo a transportista.", ,
      width = 80
    )
  )) +
  scale_x_yearqtr("Trimestre", format = "%Y-T%q", n = 8) +
  scale_y_continuous(
    labels = ~ prettyNum(., ",")
  ) +
  tema_euzen(font_var = "Poppins", size_var = 40, lineheight_var = 0.6) +
  theme(
    legend.position = "right",
    legend.title = element_blank()
  )

ggsave(paste0(path_output, "anexoComp4.png"), graf,
  width = 18, height = 10
)


# Comp 5 ------------------------------------------------------------------


## Procesamiento -----------------------------------------------------------

alto_impacto_mty$tipo <- "Alto Impacto"
alto_impacto_vida$tipo <- "Contra la vida"
alto_impacto_patrimonio$tipo <- "Contra el patrimonio"

alto_impacto2 <- rbind(
  alto_impacto_mty, alto_impacto_vida, alto_impacto_patrimonio
) %>%
  right_join(ENOE_Monterrey, by = "ID") %>%
  mutate(
    tasa = (incidencia / poblacion) * 1e5,
    nudge = case_when(
      trim == "2016 Q1" & tipo == "Alto Impacto" ~ -30,
      trim == "2016 Q1" ~ -25,
      trim == "2016 Q2" ~ -10,
      trim == "2016 Q3" ~ -20,
      .default = 0
    )
  )


## Gráfica anexo comp 5 ----------------------------------------------------


graf <- alto_impacto2 %>%
  ggplot(aes(x = trim, y = tasa, colour = tipo)) +
  geom_line(
    size = 3, alpha = .7, show.legend = FALSE
  ) +
  geom_point(size = 4) +
  labs(x = "Trimestre", y = "Tasa por cada\n 100 mil hab.", title = "Evolución de Incidencia Delictiva de Alto Impacto", subtitle = "En Monterrey, 1T 2016 a 4T 2024") +
  labs(caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal\n Nota: Los delitos contra la vida son homicidio doloso y culposo, lesiones por arma de fuego, feminicidio, violación, secuestro.\n Los delitos contra son el patrimonio son extorsión, robo de vehículo, robo a casa habitación, robo a negocio con violencia,\n robo a transporte público colectivo,robo a transeúnte en vía pública y robo a transportista.") +
  facet_wrap(~tipo, scales = "free_y", ncol = 1) +
  scale_x_yearqtr("Trimestre",
    format = "%Y-T%q", n = 8,
  ) +
  scale_y_continuous(
    labels = ~ prettyNum(., ",")
  ) +
  scale_color_manual(
    values = c("#EF645B", "#EF692D", "#EF688A")
  ) +
  geom_rect(
    data = rangos,
    aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf, fill = fill),
    alpha = 0.2, inherit.aes = FALSE
  ) +
  scale_fill_manual(values = c("EAR" = "#c04808", "IDT" = "#f8e078", "PLN" = "#f8b048")) +
  geom_text_repel(
    aes(label = round(tasa, 1)),
    fontface = "bold",
    family = "Montserrat",
    size = 7.5, color = "#222422",
    hjust = -0.5, vjust = 0.5, angle = 90,
    segment.size = 0.2,
    direction = "y",
    nudge_y = alto_impacto2$nudge,
    # nudge_y = case_when(
    #   alto_impacto2$trim == "2016 Q1" & alto_impacto2$tipo == "Alto Impacto" ~ -10,
    #   .default = 0
    # ),
    show.legend = F
  ) +
  tema_euzen(font_var = "Poppins", size_var = 35, lineheight_var = 0.6) +
  theme(
    legend.title = element_blank()
  )


ggsave(paste0(path_output, "anexoComp5.png"), graf,
  width = 18, height = 10
)


ggplot(alto_impacto2 %>% filter(trim >= "2021 Q4"), aes(x = trim, y = tasa, group = tipo, col = tipo)) +
  geom_line(size = 3, alpha = .7) +
  geom_point(size = 4) +
  facet_wrap(~tipo, scales = "free_y", ncol = 1) +
  labs(x = "Trimestre", y = "Tasa por cada\n 100 mil hab.", title = "Evolución de Incidencia Delictiva de Alto Impacto", subtitle = "En Monterrey, 4T 2021 a 4T 2024") +
  labs(caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal\n Nota: Los delitos contra la vida son homicidio doloso y culposo, lesiones por arma de fuego, feminicidio, violación, secuestro.\n Los delitos contra son el patrimonio son extorsión, robo de vehículo, robo a casa habitacion, robo a negocio con violencia,\n robo a transporte público colectivo,robo a transeúnte en vía pública y robo a transportista.") +
  tema_euzen(font_var = "Poppins", size_var = 55, lineheight_var = 0.4) +
  theme(
    legend.title = element_blank()
  ) +
  scale_x_yearqtr("Trimestre", format = "%Y-T%q", n = 8, limit = as.yearqtr(c("2021-09-01", "2023-09-01"))) +
  scale_y_continuous(labels = comma_format()) +
  scale_color_manual(values = c("#EF645B", "#EF692D", "#EF688A")) +
  geom_rect(data = rangos1, aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf, fill = fill), alpha = 0.2, inherit.aes = FALSE) +
  scale_fill_manual(values = c("1° año" = "#53B8CF", "2° año" = "purple")) +
  geom_text_repel(
    data = alto_impacto2 %>% filter(tipo == "Alto Impacto", trim >= "2021 Q4"),
    aes(label = tasa),
    fontface = "bold", family = "Montserrat", size = 15, color = "#222422",
    hjust = 1.3, vjust = 1.5, angle = 90, segment.size = 0.2,
    show.legend = F
  ) +
  geom_text_repel(
    data = alto_impacto2 %>% filter(tipo == "Contra la vida", trim >= "2021 Q4"),
    aes(label = tasa),
    fontface = "bold", family = "Montserrat", size = 15, color = "#222422",
    hjust = -1.1, vjust = 1.5, angle = 90, segment.size = 0.2,
    show.legend = F
  ) +
  geom_text_repel(
    data = alto_impacto2 %>% filter(tipo == "Contra el patrimonio", trim >= "2021 Q4"),
    aes(label = tasa),
    fontface = "bold", family = "Montserrat", size = 15, color = "#222422",
    hjust = 1.3, vjust = 1.5, angle = 90, segment.size = 0.5,
    show.legend = F
  ) -> graf


ggsave(paste0(path_output, "anexoComp6.png"), graf,
  width = 18, height = 10
)

seguimientoGdl

seguimientoGdl <- merge(seguimientoGdl, ENOE_Monterrey, by.x = "ID", by.y = "Trimestre", all.x = T, all.y = F)
seguimientoGdl$var.id <- "Monterrey"

names(trimGdl)

seguimientoGdl <- seguimientoGdl %>% mutate(tasa = round(((Incidencia / Población) * 100000), digits = 1))

ggplot(
  seguimientoGdl,
  aes(x = trim, y = tasa, group = 1)
) +
  geom_line(size = 3, alpha = .7, color = "#BF176A") +
  geom_point(size = 4, color = "#BF176A") +
  geom_text_repel(
    aes(label = tasa),
    fontface = "bold", family = "Montserrat", size = 17, color = "#BF176A",
    hjust = -1.1, vjust = 0.5, angle = 90, segment.size = 0.2,
    show.legend = F
  ) +
  geom_rect(data = rangos, aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf, fill = fill), alpha = 0.2, inherit.aes = FALSE) +
  scale_fill_manual(values = c("EAR" = "#c04808", "IDT" = "#f8e078", "PLN" = "#f8b048")) +
  labs(x = "Trimestre", y = "Tasa por cada\n 100 mil hab.", title = "Evolución de Incidencia de Delitos de Seguimiento Municipal:\n Robo, Abuso de Confianza, Extorsión y Narcomenudeo.", subtitle = "En Monterrey, 1T 2016 a 4T 2024") +
  labs(caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal") +
  tema_euzen(font_var = "Poppins", size_var = 55, lineheight_var = 0.4) +
  theme(
    legend.position = "right",
    legend.title = element_blank()
  ) +
  scale_x_yearqtr("Trimestre", format = "%Y-T%q", n = 8, limit = as.yearqtr(c("2016-01-01", "2023-09-01"))) +
  scale_y_continuous(labels = comma_format(), limits = c(0, 150)) -> graf

ggsave(paste0(path_output, "anexoComp7.png"), graf,
  width = 18, height = 9
)


ggplot(
  seguimientoGdl %>% filter(trim >= "2021 Q4"),
  aes(x = trim, y = tasa, group = 1)
) +
  geom_line(size = 3, alpha = .7, color = "#BF176A") +
  geom_point(size = 4, color = "#BF176A") +
  geom_text_repel(
    data = seguimientoGdl %>% filter(trim >= "2021 Q4"),
    aes(label = tasa),
    fontface = "bold", family = "Montserrat", size = 17, color = "#BF176A",
    hjust = 0.5, vjust = -4, segment.size = 0.2,
    show.legend = F
  ) +
  geom_rect(data = rangos1, aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf, fill = fill), alpha = 0.2, inherit.aes = FALSE) +
  scale_fill_manual(values = c("1° año" = "#53B8CF", "2° año" = "purple")) +
  labs(x = "Trimestre", y = "Tasa por cada\n 100 mil hab.", title = "Evolución de Incidencia de Delitos de Seguimiento Municipal:\n Robo, Abuso de Confianza, Extorsión y Narcomenudeo.", subtitle = "En Monterrey, 4T 2021 a 4T 2024") +
  labs(caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal") +
  tema_euzen(font_var = "Poppins", size_var = 55, lineheight_var = 0.4) +
  theme(
    legend.position = "right",
    legend.title = element_blank()
  ) +
  scale_x_yearqtr("Trimestre", format = "%Y-T%q", n = 8, limit = as.yearqtr(c("2021-03-01", "2023-09-01"))) +
  scale_y_continuous(labels = comma_format(), limits = c(20, 75)) -> graf

ggsave(paste0(path_output, "anexoComp8.png"), graf,
  width = 18, height = 9
)
