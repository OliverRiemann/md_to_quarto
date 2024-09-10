# Setup -------------------------------------------------------------------

pacman::p_load(
  tidyverse, tidyselect, zoo, readxl,
  ggrepel, scales, purrr, lubridate,
  janitor, zoo, cowplot, ggrepel, numform,
  leaflegend, htmltools, ggpol, doBy,
  viridis, foreign, reldist, cartography,
  RPostgreSQL, yaml, sf, grid, gridExtra,
  leaflet, rpostgis, googlesheets4, sp,
  treemapify, ggbeeswarm, ggthemes, shapefiles,
  stargazer, rgeos, biscale, ggalluvial,
  htmltools, htmlwidgets, stringr, reshape2,
  gt, wordcloud, SnowballC, tm, cluster,
  rgdal, grid, survey, MetBrewer, srvyr,
  readxl, sysfonts, showtext, zoo, scales,
  openxlsx
)

Sys.setlocale(locale = "es_ES.UTF-8")

source("tema_euzen.R")

path_output <- "03_grafs/"

font_add_google(name = "Montserrat", family = "Montserrat")
font_add_google(name = "Roboto", family = "Roboto")
font_add_google("Montserrat")
showtext_auto()


# Carga -------------------------------------------------------------------

incidencia <- read_csv(
  "01_input/Municipal-Delitos-2015-2024_jun2024/Municipal-Delitos-2015-2024_jun2024.csv",
  col_names = T, locale = locale(encoding = "latin1")
) %>%
  clean_names() %>%
  rename(
    "year" = "a_o",
    "cve_ent" = "clave_ent",
    "cve_mun" = "cve_municipio",
    "bien_juridico_afectado" = "bien_jur_dico_afectado"
  )


# Procesamiento -----------------------------------------------------------

incidencia <- incidencia %>%
  mutate(
    across(
      c(
        "municipio",
        "bien_juridico_afectado",
        "tipo_de_delito",
        "subtipo_de_delito",
        "modalidad"
      ),
      factor
    )
  ) %>%
  filter(
    year != 2015
  )


## incidencia nacional -----------------------------------------------------


incidencia_N <- incidencia %>%
  pivot_longer(
    cols = enero:diciembre,
    names_to = "mes",
    values_to = "incidencia"
  ) %>%
  group_by(
    year,
    bien_juridico_afectado,
    tipo_de_delito,
    subtipo_de_delito,
    modalidad,
    mes
  ) %>%
  summarise(
    incidencia = sum(incidencia)
  ) %>%
  mutate(
    alto_impacto = as.factor(
      case_when(
        subtipo_de_delito %in% c(
          "Homicidio doloso", "Homicidio culposo",
          "Feminicidio", "Secuestro", "Trata de personas",
          "Robo a transeúnte en via pública",
          "Robo a negocio", "Extorsión",
          "Robo de vehículo automotor",
          "Robo de autopartes", "Robo a casa habitación",
          "Violencia familiar", "Violación simple",
          "Violación equiparada",
          "Narcomenudeo"
        ) ~ "Alto impacto",
        .default = "Otros"
      )
    ),
    Trimestre = as.factor(
      case_when(
        mes %in% c("enero", "febrero", "marzo") ~ "1T",
        mes %in% c("abril", "mayo", "junio") ~ "2T",
        mes %in% c("julio", "agosto", "septiembre") ~ "3T",
        mes %in% c("octubre", "noviembre", "diciembre") ~ "4T",
        .default = "NA"
      )
    ),
    mes = factor(mes,
      levels = c(
        "enero", "febrero", "marzo", "abril", "mayo",
        "junio", "julio", "agosto", "septiembre", "octubre",
        "noviembre", "diciembre"
      )
    ),
    ID = factor(paste(Trimestre, year, sep = "-")),
    ID1 = factor(paste(Trimestre, year, sep = "-")) %>%
      as.yearqtr(format = "%qT-%Y") %>%
      as.Date()
  ) %>%
  filter(
    incidencia > 0
  )

incidencia_mty <- incidencia %>%
  filter(
    municipio == "Monterrey"
  ) %>%
  pivot_longer(
    enero:diciembre,
    names_to = "mes",
    values_to = "incidencia"
  ) %>%
  filter(
    !is.na(incidencia)
  ) %>%
  mutate(
    alto_impacto = as.factor(
      case_when(
        subtipo_de_delito %in% c(
          "Homicidio doloso", "Homicidio culposo",
          "Feminicidio", "Secuestro", "Trata de personas",
          "Robo a transeúnte en via pública",
          "Robo a negocio", "Extorsión",
          "Robo de vehículo automotor",
          "Robo de autopartes", "Robo a casa habitación",
          "Violencia familiar", "Violación simple",
          "Violación equiparada",
          "Narcomenudeo"
        ) ~ "Alto impacto",
        .default = "Otros"
      )
    ),
    Trimestre = as.factor(
      case_when(
        mes %in% c("enero", "febrero", "marzo") ~ "1T",
        mes %in% c("abril", "mayo", "junio") ~ "2T",
        mes %in% c("julio", "agosto", "septiembre") ~ "3T",
        mes %in% c("octubre", "noviembre", "diciembre") ~ "4T",
        .default = "NA"
      )
    ),
    mes = factor(mes,
      levels = c(
        "enero", "febrero", "marzo", "abril", "mayo",
        "junio", "julio", "agosto", "septiembre", "octubre",
        "noviembre", "diciembre"
      )
    ),
    ID = factor(paste(Trimestre, year, sep = "-")),
    ID1 = as.Date(as.yearqtr(ID, format = "%qT-%Y")),
    ID1 = factor(paste(Trimestre, year, sep = "-")) %>%
      as.yearqtr(format = "%qT-%Y")
  ) %>%
  filter(
    incidencia > 0
  )

df_incidencia_mty <- incidencia_mty %>%
  group_by(
    ID, ID1
  ) %>%
  summarise(
    incidencia = sum(incidencia)
  )


### Gráfica - incidencia MTY ------------------------------------------------

graf <- df_incidencia_mty %>%
  ggplot(
    aes(x = ID1, y = incidencia)
  ) +
  geom_bar(
    position = "dodge",
    stat = "identity",
    fill = "#EF645B",
    alpha = 0.8
  ) +
  geom_smooth(
    aes(group = 1),
    method = "loess",
    se = FALSE,
    color = "#5A5A5A",
    linetype = "dashed"
  ) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = FALSE
  ) +
  labs(
    x = "Trimestre",
    y = "incidencia",
    title = "Evolución de la incidencia Delictiva",
    subtitle = "En Monterrey, 1T 2016 a 2T 2024",
    aption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal"
  ) +
  scale_x_yearqtr(
    "Trimestre",
    format = "%Y-T%q", n = 6,
  ) +
  scale_y_continuous(
    labels = comma_format(scale = 1 / 1000, suffix = "K"),
    limits = c(0, max(df_incidencia_mty$incidencia) * 1.1)
  ) +
  geom_text_repel(
    aes(label = f_thous(incidencia)),
    fontface = "bold", family = "Montserrat",
    size = 10, color = "black",
    direction = "y",
    hjust = -0.75, vjust = 0.5, angle = 90,
    show.legend = F, segment.size = 0.5
  ) +
  tema_euzen(
    font_var = "Montserrat", size_var = 45
  )

ggsave(paste0(path_output, "incidenciabarra.png"), graf,
  width = 18, height = 9
)


# Poblacion ---------------------------------------------------------------


# https://www.inegi.org.mx/sistemas/Infoenoe/Default_15mas.aspx

ENOE_Monterrey <- read_csv(
  "01_input/poblacion/poblacion_monterrey_enoe_2024_08_08 - Hoja 1.csv"
) %>%
  pivot_longer(
    cols = -1,
    names_to = "ID",
    values_to = "poblacion"
  )


ENOE_Nacional <- read_csv(
  "01_input/poblacion/poblacion_nacional_enoe_2024_08_08 - Hoja 1.csv"
) %>%
  pivot_longer(
    cols = -1,
    names_to = "ID",
    values_to = "poblacion"
  )


## Agregado Nacional (Incidentes + Población) ------------------------------

agregado_N <- incidencia_N %>%
  group_by(ID) %>%
  summarise(
    incidencia = sum(incidencia, na.rm = T)
  )

agregado_N <- left_join(
  ENOE_Nacional,
  agregado_N,
  by = "ID"
)

## Agregado Monterrey ------------------------------------------------------

agregado_mty <- incidencia_mty %>%
  group_by(ID) %>%
  summarise(
    incidencia = sum(incidencia, na.rm = T)
  )

agregado_mty <- left_join(
  ENOE_Monterrey,
  agregado_mty,
  by = "ID"
)

df_tasa <- rbind(agregado_mty, agregado_N) %>%
  mutate(
    tasa = (incidencia / poblacion) * 1e5,
  ) %>%
  group_by(
    ID
  ) %>%
  mutate(
    nudge = ifelse(tasa == max(tasa), 0.05, -0.05),
    year = str_extract(ID, "\\d{4}+"),
    # Extract quarter
    quarter = str_extract(ID, ".*(?=T-\\d{4}$)"),
    # Combine into a format that as.yearqtr can understand
    ID = as.yearqtr(paste0(year, "-", quarter))
  )

# Gráfica - Tasa de homicidios (Mty vs. Nacional) -------------------------

graf <- df_tasa %>%
  ggplot(
    aes(x = ID, y = tasa, group = Ciudad, colour = Ciudad)
  ) +
  geom_line(
    size = 2.5,
    show.legend = FALSE
  ) +
  geom_point(
    size = 4
  ) +
  labs(
    title = "Comparativa incidencia Delictiva Nacional y Monterrey",
    subtitle = "1T-2016 al 2T-2024",
    x = "Trimestre",
    y = "Tasa por cada\n 100 mil hab.",
    caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal"
  ) +
  geom_text_repel(
    aes(label = round(tasa, digits = 1), colour = Ciudad),
    fontface = "bold", family = "Montserrat",
    size = 9,
    hjust = -0.5,
    vjust = 0.5,
    angle = 90,
    direction = "y",
    show.legend = FALSE
  ) +
  scale_x_yearqtr(
    format = "%Y-T%q",
    n = 5
  ) +
  scale_y_continuous(
    limits = c(0, max(df_tasa$tasa) * 1.15)
  ) +
  scale_color_manual(
    values = c(
      "Ciudad de Monterrey" = "#E0963F",
      "Nacional" = "#53B8CF"
    )
  ) +
  tema_euzen(
    size_var = 45, lineheight_var = 0.6
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

ggsave(paste0(path_output, "incidencia.png"),
  width = 18, height = 10
)


# Delitos de alto impacto -------------------------------------------------------------------------


# delitos de alto impacto: homicidio doloso, lesiones dolosas por arma de
# fuego, feminicidio, violación, secuestro. Patrimonio: extorsion,
# robo de vehiculo, robo casa habitacion, robo a negocio con violencia,
# robo a transporte publico colectivo, robo a transeunte, robo a transportista

incidencia_mty_s <- incidencia_mty %>%
  mutate(
    alto_impacto = case_when(
      subtipo_de_delito %in% c(
        "Homicidio doloso", "Homicidio culposo", "Lesiones dolosas",
        "Lesiones culposas", "Feminicidio", "Secuestro", "Rapto",
        "Tráfico de menores", "Rapto",
        "Otros delitos que atentan contra la libertad personal",
        "Otros delitos que atentan contra la libertad y la seguridad sexual",
        "Violación simple", "Violación equiparada", "Robo a casa habitación",
        "Robo de vehículo automotor", "Robo a transportista",
        "Robo a transeúnte en vía pública",
        "Robo a transeúnte en espacio abierto al público",
        "Robo en transporte público colectivo","Robo a negocio",
        "Extorsión") ~ "alto impacto",
      .default = NA
    ) %>% factor(),
    delitos_que_atentan_contra_la_vida = case_when(
      subtipo_de_delito %in% c(
        "Homicidio doloso",
        "Homicidio culposo",
        "Lesiones dolosas",
        "Lesiones culposas",
        "Feminicidio",
        "Secuestro",
        "Rapto",
        "Tráfico de menores",
        "Rapto",
        "Otros delitos que atentan contra la libertad personal",
        "Otros delitos que atentan contra la libertad y la seguridad sexual",
        "Violación simple",
        "Violación equiparada"
      ) ~ "alto impacto",
      .default = NA
    ) %>%
      factor(),
    delitos_que_atentan_contra_el_patrimonio = case_when(
      subtipo_de_delito  %in% c("Robo a casa habitación" ,
        "Robo de vehículo automotor" ,
        "Robo a transportista" ,
        "Robo a transeúnte en vía pública" ,
        "Robo a transeúnte en espacio abierto al público" ,
        "Robo en transporte público colectivo" ,
        "Robo a negocio" , "Extorsión") ~ "alto impacto",
      .default = NA
    ) %>% 
      factor(),
    seguimiento_municipal = case_when(
      subtipo_de_delito %in% c("Abuso de confianza",
      "Robo a casa habitación",
      "Robo de autopartes",
      "Robo a transportista",
      "Robo a transeúnte en vía pública",
      "Robo a transeúnte en espacio abierto al público",
      "Robo en transporte público individual",
      "Robo en transporte público colectivo",
      "Robo en transporte individual",
      "Robo a institución bancaria",
      "Robo a negocio",
      "Robo de ganado",
      "Robo de maquinaria",
      "Otros robos",
      "Narcomenudeo",
      "Extorsion") ~ "Seguimiento municipal",
      .default = NA
    ) %>% 
      factor(),
    trim = zoo::as.yearqtr(ID1)
  )

# delitos de alto impacto general, vida y patrimonio

alto_impacto_mty <- incidencia_mty_s %>%
  filter(alto_impacto == "alto impacto") %>%
  group_by(ID, trim) %>%
  summarise(incidencia = sum(incidencia, na.rm = T))

## Gráfica ---------------------------------------------------------------

graf <- ggplot(
  alto_impacto_mty,
  aes(x = trim, y = incidencia, group = 1)
) +
  geom_line(size = 3, alpha = .7, color = "#EF645B") +
  geom_point(size = 4, color = "#EF645B") +
  geom_text_repel(
    aes(label = prettyNum(incidencia, ",")),
    fontface = "bold", family = "Montserrat", 
    size = 7.5, color = "#EF645B",
    hjust = -1.1, vjust = 0.5, angle = 90, segment.size = 0.2,
    show.legend = F
  ) +
  labs(x = "Trimestre", y = "incidencia", 
       title = "Evolución de incidencia Delictiva de Alto Impacto", 
       subtitle = "En Monterrey, 1T 2016 a 2T 2024",
       caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal\n Nota: Los delitos de alto impacto incluyen homicidio doloso y culposo, lesiones por arma de fuego, feminicidio, violación, secuestro,\n extorsión, robo de vehículo, robo a casa habitación, robo a negocio con violencia, robo a transporte público colectivo,\n robo a transeúnte en vía pública y robo a transportista."
       ) +
  scale_x_yearqtr(
    "Trimestre", 
    format = "%Y-T%q", 
    n = 8, 
    # limit = as.yearqtr(c("2016-01-01", "2023-09-01"))
    ) +
  scale_y_continuous(
    labels = ~prettyNum(., ","), 
                     limits = c(500, 3500)
    ) +
  tema_euzen(font_var = "Montserrat", size_var = 35, lineheight_var = 0.6) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) 


ggsave(paste0(path_output, "ensu_altoimpacto.png"), graf,
  width = 18, height = 10
)


# Delitos de alto impacto: vida y patrimonio ------------------------------


alto_impacto_vida <- incidencia_mty_s %>%
  filter(delitos_que_atentan_contra_la_vida == "alto impacto") %>%
  group_by(ID, trim) %>%
  summarise(incidencia = sum(incidencia, na.rm = T))

graf <- ggplot(
  alto_impacto_vida,
  aes(x = trim, y = incidencia, group = 1)
) +
  geom_line(size = 3, color = "#EF645B", alpha = 0.8) +
  geom_point(size = 4, color = "#EF645B") +
  geom_text_repel(
    aes(label = prettyNum(incidencia, ",")),
    fontface = "bold", family = "Montserrat", size = 8.5, color = "#EF645B",
    hjust = -1.1, vjust = 0.5, angle = 90, segment.size = 0.2,
    show.legend = F
  ) +
  labs(x = "Trimestre", y = "incidencia", 
       title = "Delitos de Alto Impacto que atentan contra la vida", 
       subtitle = "En Monterrey, 1T 2016 a 2T 2024") +
  labs(caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal") +
  tema_euzen(font_var = "Montserrat", size_var = 35) +
  guides(
    size = "none",
    col = "none",
    alpha = "none"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_x_yearqtr(
    "Trimestre",
    format = "%Y-T%q",
    n = 5
  ) +
  scale_y_continuous(
    name = "Incidencias",
    labels = ~prettyNum(., ","),
    limits = c(NA, max(alto_impacto_vida$incidencia)*1.2)
  )


ggsave(paste0(path_output, "ensu_vida.png"), graf,
  width = 18, height = 9
)


# Gráfica alto impacto patrimonio -----------------------------------------

alto_impacto_patrimonio <- incidencia_mty_s %>%
  filter(delitos_que_atentan_contra_el_patrimonio == "alto impacto") %>%
  group_by(ID, trim) %>%
  summarise(incidencia = sum(incidencia, na.rm = T))

graf <- ggplot(
  alto_impacto_patrimonio,
  aes(x = trim, y = incidencia, group = 1)
) +
  geom_line(size = 3, color = "#EF645B", alpha = 0.8) +
  geom_point(size = 4, color = "#EF645B") +
  geom_text_repel(
    aes(label = incidencia),
    fontface = "bold", family = "Montserrat", size = 8.5, color = "#EF645B",
    hjust = -1.1, vjust = 0.5, angle = 90, segment.size = 0.2,
    show.legend = F
  ) +
  labs(x = "Trimestre", 
       y = "incidencia", 
       title = "Delitos de Alto Impacto que atentan contra el patrimonio", subtitle = "En Monterrey, 1T 2016 a 2T 2024") +
  labs(caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal") +
  scale_size_manual(values = c(3.5, 1.5)) +
  scale_alpha_manual(values = c(0.8, 0.6)) +
  guides(
    size = "none",
    col = "none",
    alpha = "none"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_x_yearqtr(
    "Trimestre",
    format = "%Y-T%q",
    n = 5
  ) +
  scale_y_continuous(
    name = "Incidencias",
    labels = ~prettyNum(., ","),
    limits = c(NA, max(alto_impacto_patrimonio$incidencia)*1.2)
  ) +
  tema_euzen(font_var = "Montserrat", size_var = 35) 


ggsave(
  paste0(path_output, "ensu_patrimonio.png"),
  graf,
  width = 18, height = 9
)


# Delitos de seguimiento municipal: ---------------------------------------

# delitos de seguimiento municipal: Robo, abuso de confianza, extorsión y narcomenudeo

seguimiento_mty <- incidencia_mty_s %>%
  filter(seguimiento_municipal == "Seguimiento municipal") %>%
  group_by(trim) %>%
  summarise(incidencia = sum(incidencia, na.rm = T))

graf <- ggplot(
  seguimiento_mty,
  aes(x = trim, y = incidencia, group = 1)
) +
  geom_line(size = 3, alpha = .7, color = "#BF176A") +
  geom_point(size = 4, color = "#BF176A") +
  geom_text_repel(
    aes(label = comma(incidencia, scale = 1)),
    fontface = "bold", family = "Montserrat", size = 8.5, color = "#BF176A",
    hjust = -1.1, vjust = 0.5, 
    nudge_x = ifelse(
      seguimiento_mty$trim == "2016 Q4", 0.025, 0
    ), #Esto es para la gráfica de monterrey específicamente
    angle = 90, segment.size = 0.2,
    show.legend = F
  ) +
  labs(x = "Trimestre", y = "incidencia", title = "Evolución de incidencia de Delitos de Seguimiento Municipal:\n Robo, Abuso de Confianza, Extorsión y Narcomenudeo.", subtitle = "En Monterrey, 1T 2016 a 2T 2024") +
  labs(caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )+
  scale_x_yearqtr(
    "Trimestre",
    format = "%Y-T%q",
    n = 5
  ) +
  scale_y_continuous(
    name = "Incidencias",
    labels = ~prettyNum(., ","),
    limits = c(NA, max(seguimiento_mty$incidencia)*1.2)
  ) +
  tema_euzen(font_var = "Montserrat", size_var = 35) 


ggsave(paste0(path_output, "ensu_seguimientoMun.png"), graf,
  width = 18, height = 9
)

# Robos o asaltos ---------------------------------------------------------

# Robos o asaltos, consumo de alcohol en las calles, vandalismo, venta o consumo de drogas, disparos frecuentes con armas, pandillerismo,


## Dataframes --------------------------------------------------------------

robo <- incidencia_mty_s %>%
  filter(tipo_de_delito == "Robo") %>%
  group_by(trim) %>%
  summarise(incidencia = sum(incidencia, na.rm = T))

vandalismo <- incidencia_mty_s %>%
  filter(subtipo_de_delito == "Daño a la propiedad") %>%
  group_by(trim) %>%
  summarise(incidencia = sum(incidencia, na.rm = T))

disparos <- incidencia_mty_s %>%
  filter(modalidad == "Con arma de fuego") %>%
  group_by(trim) %>%
  summarise(incidencia = sum(incidencia, na.rm = T))

narcomenudeo <- incidencia_mty_s %>%
  filter(tipo_de_delito == "Narcomenudeo") %>%
  group_by(trim) %>%
  summarise(incidencia = sum(incidencia, na.rm = T))


## Gráfica robo ------------------------------------------------------------


graf <- robo %>% ggplot(
    aes(x = trim, y = incidencia, group = 1)
  ) +
    geom_line(size = 3, alpha = .7, color = "#EF645B") +
    geom_point(size = 4, color = "#EF645B") +
    geom_text_repel(
      aes(label = prettyNum(incidencia, ",")),
      fontface = "bold", family = "Montserrat", size = 8.5, 
      color = "black", 
      direction = "y",
      hjust = ifelse(robo$trim == "2016 Q2", -0.25, -0.75),
      nudge_y = case_when(
        robo$trim == "2016 Q4" ~ 50,
        robo$trim == "2017 Q4" ~ 50,
        .default = 0
      ),
      nudge_x = case_when(
        robo$trim == "2016 Q4" ~ 0.025,
        .default = 0
      ),
      angle = 90, 
      segment.size = 0.2
    ) +
    ggtitle(
      "Incidencia Robo",
      "Monterrey, 1T-2016 al 2T-2024"
    ) +
    labs(caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal") +
    scale_color_manual(values = c(rev(met.brewer(name = "OKeeffe1", n = 1)[-3:-4]))) +
    scale_x_yearqtr(
      "Trimestre",
      format = "%Y-T%q",
      n = 5
    ) +
    scale_y_continuous(
      name = "Incidencias",
      labels = ~prettyNum(., ","),
      limits = c(NA, max(robo$incidencia)*1.2)
    ) +
    tema_euzen(font_var = "Montserrat", size_var = 35) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
      )
  
  ggsave(
    "03_grafs/ensu_robo.png", plot = graf,
    width = 18, height = 9
  )


## Gráfica vandalismo ------------------------------------------------------

graf <- ggplot(
  vandalismo,
  aes(x = trim, y = incidencia)
) +
  geom_line(size = 3, alpha = .7, color = "#EF645B") +
  geom_point(size = 4, color = "#EF645B") +
  geom_text_repel(
    aes(label = prettyNum(incidencia, ",")),
    fontface = "bold", family = "Montserrat", size = 8.5, color = "black",
    hjust = -1.1,
    nudge_y = case_when(
      vandalismo$trim == "2016 Q1" ~ 25,
      .default = 0
    ),
    nudge_x = case_when(
      vandalismo$trim == "2020 Q2" ~ 0.025,
      .default = 0
    ),
    vjust = 0.5, angle = 90, segment.size = 0.2,
    show.legend = F
  ) +
  ggtitle(
    "Incidencia de vandalismo",
    "Monterrey, 1T-2016 al 2T-2024"
  ) +
  labs(caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal\n Nota:Se utilizan los delitos clasificados como Daño a la Propiedad como vandalismo.") +
    scale_x_yearqtr(
      "Trimestre",
      format = "%Y-T%q",
      n = 5
    ) +
    scale_y_continuous(
      name = "Incidencias",
      labels = ~prettyNum(., ","),
      limits = c(NA, max(vandalismo$incidencia)*1.2)
    ) +
    tema_euzen(font_var = "Montserrat", size_var = 35) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )

ggsave(paste0(path_output, "ensu_vandalismo.png"), graf,
  width = 18, height = 9
)


## Gráfica disparos --------------------------------------------------------

graf <- ggplot(
  disparos,
  aes(x = trim, y = incidencia)
) +
  geom_line(size = 3, alpha = .7, color = "#EF645B") +
  geom_point(size = 4, color = "#EF645B") +
  geom_text_repel(
    aes(label = prettyNum(incidencia, ",")),
    fontface = "bold", family = "Montserrat", 
    size = 8.5, color = "black",
    hjust = -2, 
    # nudge_y = case_when(
    #   vandalismo$trim == "2016 Q1" ~ 25,
    #   .default = 0
    # ),
    # nudge_x = case_when(
    #   vandalismo$trim == "2020 Q2" ~ 0.025,
    #   .default = 0
    # ),
    vjust = 0.5, angle = 90, segment.size = 0.2,
    show.legend = F
  ) +
  ggtitle(
    "Incidencia de delitos con arma de fuego",
    "Monterrey, 1T-2016 al 2T-2024"
  ) +
  labs(caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federall\n Nota:Se utilizan los delitos clasificados con modalidad de arma de fuego.") +
  scale_x_yearqtr(
    "Trimestre",
    format = "%Y-T%q",
    n = 5
  ) +
  scale_y_continuous(
    name = "Incidencias",
    labels = ~prettyNum(., ","),
    limits = c(NA, max(disparos$incidencia)*1.2)
  ) +
  tema_euzen(font_var = "Montserrat", size_var = 35) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

ggsave(paste0(path_output, "ensu_disparos.png"), graf,
  width = 18, height = 9
)


## Gráfica narcomenudeo ----------------------------------------------------
graf <- narcomenudeo %>% 
  ggplot(
  aes(x = trim, y = incidencia)
) +
  geom_line(size = 3, alpha = .7, color = "#EF645B") +
  geom_point(size = 4, color = "#EF645B") +
  geom_text_repel(
    aes(label = prettyNum(incidencia, ",")),
    fontface = "bold", family = "Montserrat", 
    size = 8.5, color = "black",
    hjust = -1, 
    # nudge_y = case_when(
    #   vandalismo$trim == "2016 Q1" ~ 25,
    #   .default = 0
    # ),
    # nudge_x = case_when(
    #   vandalismo$trim == "2020 Q2" ~ 0.025,
    #   .default = 0
    # ),
    vjust = 0.5, angle = 90, segment.size = 0.2,
    show.legend = F
  ) +
  ggtitle(
    "Incidencia de narcomenudeo",
    "Monterrey, 1T-2016 al 2T-2024"
  ) +
  labs(caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal") +
  scale_x_yearqtr(
    "Trimestre",
    format = "%Y-T%q",
    n = 5
  ) +
  scale_y_continuous(
    name = "Incidencias",
    labels = ~prettyNum(., ","),
    limits = c(NA, max(narcomenudeo$incidencia)*1.2)
  ) +
  tema_euzen(font_var = "Montserrat", size_var = 35) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )
  

ggsave(paste0(path_output, "ensu_narcomenudeo.png"), graf,
  width = 18, height = 9
)



# Otra cosa ---------------------------------------------------------------


names(trimGdl)
agre
trimNac1 <- trim %>% select(ID, ID1, `Tasa delictiva Nacional`)
trimNac1$var.id <- "Nacional"
trimNac1 <- rename(trimNac1, tasa = `Tasa delictiva Nacional`)
trimGdl <- trimGdl %>% select(ID, ID1, tasa, var.id)

trim1 <- rbind(trimNac1, trimGdl)

trim1$var.id <- as.factor(trim1$var.id)
pacman::p_load(zoo)
trim1$trim <- with(trim, zoo::as.yearqtr(ID1))

class(trim1$ID1)

ggplot(
  trim1,
  aes(
    x = trim,
    y = tasa, group = var.id, col = var.id
  )
) +
  geom_point(size = 4) +
  geom_line(size = 3, alpha = .7) +
  labs(
    title = "Comparativa incidencia Delictiva Nacional y Monterrey",
    subtitle = "1T-2016 al 2T-2024", x = "Trimestre", y = "Tasa por cada\n 100 mil hab.", caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal"
  ) +
  geom_text_repel(
    data = trim1,
    aes(x = trim, y = tasa, label = tasa),
    fontface = "bold", family = "Montserrat",
    size = 15, hjust = -0.8, vjust = 0.8, segment.size = 0.08, angle = 90
  ) +
  tema_euzen(font_var = "Montserrat", size_var = 55, lineheight_var = 0.4) +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) +
  # ylim(100, 500) +
  scale_color_manual(values = c("#E0963F", "#53B8CF")) +
  scale_x_yearqtr("Trimestre", format = "%Y-T%q", n = 8, limit = as.yearqtr(c("2016-01-01", "2023-09-01"))) -> graf
graf
#
ggsave(paste0(path_output, "incidencia1.png"), graf,
  width = 19, height = 10
)

## facet
alto_impacto_mty1 <- merge(alto_impacto_mty, alto_impacto_vida, by = c("ID", "ID1"))
alto_impacto_mty1 <- merge(alto_impacto_mty1, alto_impacto_patrimonio, by = c("ID", "ID1"))

ggplot(alto_impacto_mty1, aes(x = trim) +
  geom_line(data = alto_impacto_mty1, aes(y = incidencia.x, group = 1), size = 3, alpha = .7, color = "#EF645B", show.legend = T) +
  geom_point(data = alto_impacto_mty1, aes(y = incidencia.x), size = 4, color = "#EF645B", show.legend = T) +
  geom_line(data = alto_impacto_mty1, aes(y = incidencia.y, group = 1), size = 3, alpha = .7, color = "#EF692D", show.legend = T) +
  geom_point(data = alto_impacto_mty1, aes(y = incidencia.y), size = 4, color = "#EF692D", show.legend = T) +
  geom_line(data = alto_impacto_mty1, aes(y = incidencia, group = 1), size = 3, alpha = .7, color = "#EF688A", show.legend = T) +
  geom_point(data = alto_impacto_mty1, aes(y = incidencia), size = 4, color = "#EF688A", show.legend = T) +
  labs(x = "Trimestre", y = "incidencia", title = "Evolución de incidencia Delictiva de Alto Impacto", subtitle = "En Monterrey, 1T 2016 a 2T 2024") +
  labs(caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal\n Nota: Los delitos de alto impacto incluyen homicidio doloso y culposo, lesiones por arma de fuego, feminicidio, violación, secuestro,\n extorsión, robo de vehículo, robo a casa habitación, robo a negocio con violencia, robo a transporte público colectivo,\n robo a transeúnte en vía pública y robo a transportista.") +
  tema_euzen(font_var = "Montserrat", size_var = 50, lineheight_var = 0.4) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_x_discrete("Trimestre")) -> graf


ggsave(paste0(path_output, "ensu_altoComp1.png"), graf,
  width = 18, height = 9
)
names(incidencia_mty_s)


# Alto impacto ------------------------------------------------------------

alto_impacto_mty$tipo <- "Alto Impacto"
alto_impacto_vida$tipo <- "Contra la vida"
alto_impacto_patrimonio$tipo <- "Contra el patrimonio"

alto_impacto2 <- rbind(
  alto_impacto_mty, 
  alto_impacto_vida, 
  alto_impacto_patrimonio
  )

p <- alto_impacto2 %>% 
  ggplot(
    aes(trim, y = incidencia, colour = tipo, group = tipo)
  ) +
  geom_line(
    size = 3, alpha = 0.7,
            show.legend = FALSE) +
  geom_point(size = 4) +
  facet_wrap("tipo", ncol = 1) +
  geom_text_repel(
    aes(
      label = prettyNum(incidencia, ",")
      ),
    fontface = "bold", family = "Montserrat", 
    size = 7.5, color = "#828282", alpha = 0.8,
    hjust = -1, vjust = 0.5, angle = 90, 
    direction = "y",
    show.legend = F
  )  + 
  scale_x_yearqtr(
    "Trimestre",
    format = "%Y-T%q",
    n = 5
  ) +
  scale_y_continuous(
    name = "Incidencias",
    labels = ~prettyNum(., ","),
    limits = c(NA, max(alto_impacto2$incidencia)*1.25)
  ) +
  scale_color_manual(values = c("#EF645B", "#EF692D", "#EF688A")) +
  facet_wrap("tipo", scales = "free_y", ncol = 1) +
  labs(x = "Trimestre", y = "incidencia", title = "Evolución de incidencia Delictiva de Alto Impacto", subtitle = "En Monterrey, 1T 2016 a 2T 2024") +
  labs(caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal\n Nota: Los delitos de alto impacto incluyen homicidio doloso y culposo, lesiones por arma de fuego, feminicidio, violación, secuestro,\n extorsión, robo de vehículo, robo a casa habitación, robo a negocio con violencia, robo a transporte público colectivo,\n robo a transeúnte en vía pública y robo a transportista.") +
  tema_euzen(font_var = "Montserrat", size_var = 35) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )  

ggsave(paste0(path_output, "ensu_altoComp4.png"), p,
  width = 18, height = 10
)


ggplot(alto_impacto2, aes(x = trim, y = incidencia, group = tipo, col = tipo)) +
  geom_line(size = 3, alpha = .7) +
  geom_point(size = 4) +
  facet_wrap(tipo) +
  labs(x = "Trimestre", y = "incidencia", title = "Evolución de incidencia Delictiva de Alto Impacto", subtitle = "En Monterrey, 1T 2016 a 2T 2024") +
  labs(caption = "Fuente: Secretariado de Seguridad Pública del Gobierno Federal\n Nota: Los delitos de alto impacto incluyen homicidio doloso y culposo, lesiones por arma de fuego, feminicidio, violación, secuestro,\n extorsión, robo de vehículo, robo a casa habitación, robo a negocio con violencia, robo a transporte público colectivo,\n robo a transeúnte en vía pública y robo a transportista.") +
  tema_euzen(font_var = "Montserrat", size_var = 55, lineheight_var = 0.4) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_x_yearqtr("Trimestre", format = "%Y-T%q", n = 8, limit = as.yearqtr(c("2016-01-01", "2023-09-01"))) +
  scale_y_continuous(labels = comma_format(), limits = c(100, 8800)) +
  scale_color_manual(values = c("#EF645B", "#EF692D", "#EF688A")) +
  # geom_text_repel( data = alto_impacto2 %>% filter(tipo == "Alto Impacto"),
  #   aes(label = comma(incidencia, scale = 1/1000, suffix = "K", accuracy = 0.1)),
  #   fontface = "bold", family = "Montserrat", size = 15, color = "#EF645B",
  #   hjust = -1.1, vjust =  0.5, angle = 90, segment.size  = 0.2,
  #   show.legend = F) +
  geom_text_repel(
    data = alto_impacto2 %>% filter(tipo == "Contra la vida"),
    aes(label = comma(incidencia, scale = 1 / 1000, suffix = "K", accuracy = 0.1)),
    fontface = "bold", family = "Montserrat", size = 15, color = "#EF688A",
    hjust = 1, vjust = 0.5, angle = 90, segment.size = 0.2,
    show.legend = F
  ) +
  geom_text_repel(
    data = alto_impacto2 %>% filter(tipo == "Contra el patrimonio"),
    aes(label = comma(incidencia, scale = 1 / 1000, suffix = "K", accuracy = 0.1)),
    fontface = "bold", family = "Montserrat", size = 15, color = "#EF692D",
    hjust = 0.8, vjust = 1.5, angle = 90, segment.size = 0.5,
    show.legend = F
  ) -> graf
