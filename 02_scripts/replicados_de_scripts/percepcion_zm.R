# Setup -------------------------------------------------------------------
pacman::p_load(
  tidyverse,
  srvyr,
  janitor,
  zoo,
  MetBrewer,
  scales,
  ggrepel,
  sf,
  sp
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

# change name and direction of the file
mun_shp <- read_sf("01_input/MUNICIPIO.shp") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  rename(cve_ent = ENTIDAD, cve_mun = MUNICIPIO, nom_mun = NOMBRE) %>%
  filter(cve_mun %in% c("41", "98", "99", "102", "120")) %>%
  mutate(
    cve_ent = as.factor(cve_ent),
    cve_mun = as.factor(cve_mun),
    cve_mun = case_when(
      cve_mun == "41" ~ "039",
      cve_mun == "98" ~ "097",
      cve_mun == "99" ~ "098",
      cve_mun == "102" ~ "101",
      cve_mun == "120" ~ "120"
    )
  )


# Limpieza ----------------------------------------------------------------

path_ensu_0923 <- "01_input/ENSU/bases/ensu_bd_septiembre_2023_dbf/ENSU_CB_0923.dbf"

base_mapa_2 <- foreign::read.dbf(path_ensu_0923) %>%
  clean_names() %>%
  filter(
    cve_ent == "14",
    cve_mun %in% c(
      "039", "097",
      "098", "101",
      "120"
    )
  ) %>%
  select(
    cve_ent, cve_mun, nom_mun,
    upm_dis, est_dis, fac_sel,
    bp1_1
  ) %>%
  mutate(
    nom_mun = recode(nom_mun,
      "TONALA" = "TONALÁ",
      "TLAJOMULCO DE ZUNIGA" = "TLAJOMULCO DE ZÚÑIGA"
    ),
    nom_mun = str_wrap(nom_mun, 9)
  ) %>%
  srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>%
  filter(!is.na(bp1_1)) %>%
  group_by(cve_ent, cve_mun, nom_mun, bp1_1) %>%
  summarise(
    por = survey_mean(vartype = "cv"),
    total = survey_total(vartype = "cv")
  ) %>%
  filter(bp1_1 == 2) %>%
  left_join(mun_shp,
    by = c("cve_ent", "cve_mun"),
    suffix = c("_base", "_mapa")
  ) %>%
  st_as_sf() %>%
  rename(nom_mun = nom_mun_base)

# Mapa --------------------------------------------------------------------

mapa <- ggplot() +
  geom_sf(
    data = base_mapa,
    aes(
      geometry = geometry,
      fill = por
    ),
    size = 0.75, col = "darkgray",
    alpha = 0.7
  ) +
  geom_sf_text(
    data = base_mapa,
    aes(label = paste0(str_to_title(nom_mun), "\n(", percent(por, accuracy = 0.1), ")", "\n")),
    fontface = "bold", family = "Montserrat", size = 25, lineheight = .4,
    # vjust = ifelse(grepl("san pedro", base_mapa$nom_mun, ignore.case = T), 1.125, 0.7),
    hjust = 0.5
  ) +
  ggtitle(
    "% de población mayor de 18 años que se siente insegura",
    "ZM de Guadalajara, 3T-2023"
  ) +
  labs(caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  coord_sf() +
  tema_euzen(size_var = 75, font_var = "Roboto", is.map = T) +
  scale_color_gradient(
    low = "#ffeda0", high = "#f03b20",
    aesthetics = "fill", labels = percent
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )
mapa

ggsave(paste0(path_output, "ensu_perc_zm_guad2.png"), mapa,
  width = 12, height = 14
)
