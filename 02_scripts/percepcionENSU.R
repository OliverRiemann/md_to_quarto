dir.create("01_input")
dir.create("02_scripts")
dir.create("03_grafs")
dir.create("04_temp")

pacman::p_load(
  tidyverse, scales, purrr, lubridate, janitor, zoo, cowplot,
  ggrepel, numform, leaflegend, htmltools, ggpol, doBy, viridis,
  foreign, reldist, cartography, RPostgreSQL, yaml, sf, grid,
  gridExtra, leaflet, rpostgis, googlesheets4, sp, treemapify,
  ggbeeswarm, ggthemes, shapefiles, stargazer, biscale,
  ggalluvial, htmltools, htmlwidgets, stringr, reshape2, gt,
  wordcloud, SnowballC, tm, cluster, grid, survey,
  MetBrewer, srvyr, readxl, sysfonts, showtext
)

Sys.setlocale(locale = "es_ES.UTF-8")

source("tema_euzen.R")

path_output <- "03_grafs/"
path_temp <- "04_temp/"

font_add_google(name = "Poppins", family = "Poppins")
font_add_google(name = "Roboto", family = "Roboto")
font_add_google("Montserrat")
showtext_auto()

options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "adjust")

# change name and direction of the file
mun_shp <- read_sf("01_input/MUNICIPIO.shp")

mun_shp <- rename(mun_shp, cve_ent = ENTIDAD, cve_mun = MUNICIPIO, nom_mun = NOMBRE)

mun_shp <- mun_shp %>% filter(cve_mun == "41" | cve_mun == "98" | cve_mun == "99" | cve_mun == "102" | cve_mun == "120")

mun_shp$cve_ent <- as.factor(mun_shp$cve_ent)
mun_shp$cve_mun <- as.factor(mun_shp$cve_mun)

files <- list.files(
  path = "01_input", pattern = "CB.*\\.dbf",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

######################################################### PERCEPCION DE INSEGURIDAD #########################################################

# Percepcion Monterrey vs Nacional----

#* Limpiar bases----
ensu_full <- map(files, function(x) {
  # x <- files[33]

  print(x)

  fecha_var <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")


  if (grepl("1216|0317|0617|0917|1217|0318|0920|0618|0918|1218|0319|0619|0919|1219|0320|1220|0321|0621|0921|1221|0322|0622|0922|1222|0323|0623|0923|1223|0324", x, ignore.case = T)) {
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
      clean_names() %>%
      rename(
        CVE_ENT = cve_ent,
        CVE_MUN = cve_mun
      ) -> temp
  }

  temp2 <- temp %>%
    mutate(
      fac_sel = as.numeric(fac_sel),
      var_id = "nacional",
      var_mty = case_when(
        CVE_ENT == "19" & CVE_MUN == "039" ~ "Monterrey",
        grepl("16$|17$|0318", fecha_var, ignore.case = T) & cd == 34 ~ "Monterrey",
        T ~ "otro"
      )
    ) %>%
    select(
      var_id, var_mty, upm_dis,
      est_dis, fac_sel, bp1_1
    ) %>%
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>%
    filter(!is.na(bp1_1))

  vars <- list(list("var_id", "bp1_1"), list("var_mty", "bp1_1"))

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
      var_id = coalesce(var_id, var_mty),
      var_id = case_when(
        var_id == "Monterrey" ~ "Monterrey",
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
    select(-matches("cv|var_mty")) %>%
    filter(bp1_1 == "2") -> data
})

df_percp2 <- ensu_full %>%
  reduce(full_join) %>%
  mutate(trim = zoo::as.yearqtr(fecha)) %>%
  dplyr::ungroup() %>%
  group_by(fecha) %>%
  mutate(
    nudge = ifelse(
      por == max(por),
      0.1, -0.1
    )
  )

# Serie de tiempo----
graf <- ggplot(
  df_percp2,
  aes(x = trim, y = por, col = var_id)
) +
  geom_line(
    aes(group = var_id),
    alpha = 0.6, linewidth = 1.5,
    show.legend = FALSE
  ) +
  geom_point(alpha = 0.8, size = 2.5) +
  ggtitle(
    " % de la población mayor de 18 años que se siente insegura",
    "1T-2016 a 2T-2024"
  ) +
  labs(
    x = "Trimestre",
    caption = "Fuente: Encuesta Nacional de Seguridad Pública 
    Urbana - INEGI\nNota: 1) Se omite el valor del segundo trimestre de 2020, 
    por la pandemia ocasionada por el COVID-19"
  ) +
  scale_color_manual(values = c(
    wesanderson::wes_palette(name = "FantasticFox1", n = 5)[1],
    wesanderson::wes_palette(name = "FantasticFox1", n = 5)[3])
    ) +
  scale_x_yearqtr(
    format = "%Y-T%q", n = 8, 
    limits = as.yearqtr(
      c("2016-01-01", "2024-06-01")
      )
    ) +
  scale_y_continuous("% de población",
    labels = scales::percent_format(accuracy = 1L),
    limits = c(
      0.4,
      max(df_percp2$por)*1.15
    )
  ) +
  geom_text_repel(
    aes(label = percent(por, accuracy = 0.1)),
    angle = 90, 
    size = 7.5,
    fontface = "bold", 
    family = "Montserrat",
    direction = "y",
    nudge_y = df_percp2$nudge,
    show.legend = F, 
    force = 2
  ) +
  tema_euzen(
    size_var = 35, 
    font_var = "Montserrat", 
    lineheight_var = .6
  ) +
  theme(
    panel.grid = element_line(color = col_grid),
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave(
  plot = graf,
  filename = "ensu_perc2.png",
  path = "03_grafs/",
  width = 16, height = 8
)


# Percepcion en la ZMG ---
# "41", "98",
# "99", "102",
# "120"

mun_shp <- mun_shp %>%
  mutate(cve_mun = case_when(
    cve_mun == "41" ~ "039",
    cve_mun == "98" ~ "097",
    cve_mun == "99" ~ "098",
    cve_mun == "102" ~ "101",
    cve_mun == "120" ~ "120"
  ))

foreign::read.dbf("01_input/ENSU/bases/ensu_bd_septiembre_2023_dbf/ENSU_CB_0923.dbf") %>%
  clean_names() %>%
  filter(
    cve_ent == "19",
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
  mutate(nom_mun = recode(nom_mun,
    "TONALA" = "TONALÁ",
    "TLAJOMULCO DE ZUNIGA" = "TLAJOMULCO DE ZÚÑIGA"
  )) %>%
  srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>%
  filter(!is.na(bp1_1)) %>%
  group_by(cve_ent, cve_mun, nom_mun, bp1_1) %>%
  summarise(
    por = survey_mean(vartype = "cv"),
    total = survey_total(vartype = "cv")
  ) %>%
  filter(bp1_1 == 2) %>%
  left_join(mun_shp, by = c("cve_ent", "cve_mun")) %>%
  st_as_sf() %>%
  rename(nom_mun = nom_mun.x) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") -> base_mapa
names(mun_shp)
base_mapa$nom_mun <- str_wrap(base_mapa$nom_mun, 9)

#* Mapa----
ggplot() +
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
    "ZM de Monterrey, 2T-2024"
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
  ) -> mapa
mapa

ggsave(paste0(path_output, "ensu_perc_zm_guad.png"), mapa,
  width = 12, height = 14
)

## Lugar donde se siente inseguro  GDL vs NACIONAL
#
# ### Limpiar bases----
# files_temp <- files[grep("0623", files, ignore.case = T)];
#
# ensu_full <- map(files_temp, function(x) {
#
#   print(x)
#
#   fecha <- str_remove(x, "/Users/monicamolina/Documents/DATOS_ENSU/CB/ENSU_CB_")
#
#   foreign::read.dbf(x) %>%
#     clean_names() %>%
#     mutate(
#       var_id = "nacional",
#       var_cd = ifelse(cve_ent == "19" &
#                           cve_mun %in% c("039"),
#                         "Monterrey", "otro")
#     ) -> temp
#
#   design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
#
#   vars <- c("var_id", "var_cd")
#
#   funion <- function(z) {
#
#     as_tibble(svyby(~bp1_2_01 + bp1_2_02 + bp1_2_03 +
#                       bp1_2_04 + bp1_2_05 + bp1_2_06 +
#                       bp1_2_07 + bp1_2_08 + bp1_2_09 +
#                       bp1_2_10 + bp1_2_11 + bp1_2_12,
#                     by = as.formula(paste0("~", z)), design, svytotal, vartype = "cvpct"))
#
#   }
#
#   output <- map(vars, funion)
#
#   output %>%
#     reduce(full_join) %>%
#     select(-matches("cv")) %>%
#     pivot_longer(cols = matches("bp1"),
#                  names_to = "var",
#                  values_to = "total") %>%
#     mutate(var_id = coalesce(var_cd, var_id),
#            resp = str_sub(var, nchar(var)),
#            var = substr(var, 1, nchar(var) - 1)) %>%
#     filter(var_id != "otro",
#            resp != "3") %>%
#     select(-matches("guad")) %>%
#     group_by(var_id, var) %>%
#     mutate(
#       tot = sum(total, na.rm = T),
#       por = total / tot,
#       fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2),
#                             substr(fecha, start = 3, stop = 4), sep = "-"),
#                       format = "%d-%m-%y"),
#       var = str_wrap(case_when(
#         var == "bp1_2_01" ~ "Casa",
#         var == "bp1_2_02" ~ "Trabajo",
#         var == "bp1_2_03" ~ "Calles que habitualmente usa",
#         var == "bp1_2_04" ~ "Escuela",
#         var == "bp1_2_05" ~ "Mercado",
#         var == "bp1_2_06" ~ "Centro comercial",
#         var == "bp1_2_07" ~ "Banco",
#         var == "bp1_2_08" ~ "Cajeros automáticos",
#         var == "bp1_2_09" ~ "Transporte público",
#         var == "bp1_2_10" ~ "Automóvil",
#         var == "bp1_2_11" ~ "Carretera",
#         var == "bp1_2_12" ~ "Parque o centro recreativo"
#       ), 10)
#     ) %>%
#     ungroup() %>%
#     filter(resp == "2") -> data
#
# })
#
# ensu_full %>%
#   reduce(full_join) %>%
#   mutate(
#     var_id = str_to_title(var_id)
#   ) -> df_graf
#
# #
# #* Barras de sensacion de inseguridad por tipo----
# df_graf %>%
#   filter(grepl("Monterrey", var_id, ignore.case = T)) %>%
#   mutate(rank = rank(-por, ties.method = "first"),
#          cut_var = case_when(
#            rank >= min(rank) & rank < median(rank) ~ "primeros",
#            rank >= median(rank) & rank <= max(rank) ~ "ultimos",
#          )) %>%
#   select(var, cut_var) -> aux_orden
#
# df_temp <- left_join(df_graf, aux_orden)
#
# axis_lim <- max(df_temp$por + 0.05)
#
# funico <- unique(df_temp$cut_var)
#
# for (i in funico) {
#
#   # i = funico[1]
#
#   print(i)
#
#   df_temp %>%
#     filter(cut_var == i) -> bar_temp
#
#   bar_temp %>%
#     filter(grepl("Monterrey", var_id, ignore.case = T)) %>%
#     arrange(-por) %>%
#     pull(var) %>%
#     unique() -> orden_var
#
#   bar_temp$var <- factor(bar_temp$var, levels = orden_var)
#
#   ggplot(
#     bar_temp,
#     aes(x = por, y = fct_rev(as.factor(var)), fill = fct_rev(var_id), group = fct_rev(var_id))
#   ) +
#     geom_bar(aes(),
#              stat = "identity", position = "dodge2") +
#     geom_text(aes(label = percent(por, accuracy = 0.1)),
#               fontface = "bold", family = "Montserrat", size = 22, hjust = -0.5,
#               position = position_dodge2(width = 0.9)) +
#     ggtitle("% de población mayor de 18 años que se\nsiente insegura en espacio público",
#             "Nacional vs. Monterrey, 2T-2023") +
#     labs(y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
#     tema_euzen(size_var = 60, font_var = "Roboto") +
#     scale_alpha_manual(values = c(0.6, 0.9)) +
#     scale_fill_manual(values = c(rev(met.brewer(name = "OKeeffe1", n = 2)[-3:-4]), "grey20")) +
#     scale_x_continuous("% de población", labels = scales::percent_format(accuracy = 1L), limits = c(0, axis_lim)) +
#     guides(fill = guide_legend(reverse = T),
#            alpha = guide_legend(reverse = T)) +
#     theme(
#       legend.title = element_blank(),
#       legend.position = "bottom"
#     ) -> graf
#
#   ggsave(paste0(path_output, "ensu_sens_inseg_", i,".png"), graf,
#          width = 11, height = 10.5)
#
# }


## Lugar donde se siente inseguro  GDL trimestres

### Limpiar bases----
files_temp <- files[grep("0923|24", files, ignore.case = T)]
ensu_full <- map(files_temp, function(x) {
  x <- files_temp[1]
  print(x)

  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  foreign::read.dbf(x) %>%
    clean_names() %>%
    mutate(
      var_id = ifelse(cve_ent == "19" &
        cve_mun %in% c("039"),
      "Monterrey", "otro"
      )
    ) -> temp

  design <- svydesign(
    ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp
    )

  vars <- c("var_id")

  funion <- function(z) {
    as_tibble(svyby(
      ~ bp1_2_01 + bp1_2_02 + bp1_2_03 +
        bp1_2_04 + bp1_2_05 + bp1_2_06 +
        bp1_2_07 + bp1_2_08 + bp1_2_09 +
        bp1_2_10 + bp1_2_11 + bp1_2_12,
      by = as.formula(paste0("~", z)), design,
      svytotal, vartype = "cvpct"
    ))
  }

  output <- map(vars, funion)

  output %>%
    reduce(full_join) %>%
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
    filter(
      var_id != "otro",
      resp != "3"
    ) %>%
    group_by(var_id, var) %>%
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
      ), 10)
    ) %>%
    ungroup() %>%
    filter(resp == "2") -> data
})

ensu_full %>%
  reduce(full_join) %>%
  mutate(
    var_id = str_to_title(var_id),
    trim = as.yearmon(fecha)
  ) -> df_graf

write_rds(df_graf, "04_temp/df_percep_lugares_publicos.rds")
# Barras de sensación de inseguridad por tipo----

df_graf %>%
  filter(fecha == "2024-06-01") %>%
  mutate(
    rank = rank(-por, ties.method = "first"),
    cut_var = case_when(
      rank >= min(rank) & rank < median(rank) ~ "primeros",
      rank >= median(rank) & rank <= max(rank) ~ "ultimos",
    )
  ) %>%
  select(var, cut_var) -> aux_orden

# Sensación de inseguridad por tipo ---------------------------------------


df_temp <- left_join(df_graf, aux_orden)

funico <- unique(df_temp$cut_var)

for (i in funico) {
  # i = funico[1]

  print(i)

  df_temp %>%
    filter(cut_var == i) -> bar_temp

  bar_temp %>%
    filter(fecha == "2024-06-01") %>%
    arrange(-por) %>%
    pull(var) %>%
    unique() -> orden_var

  bar_temp$var <- factor(bar_temp$var, levels = orden_var)

  graf <- ggplot(
    bar_temp,
    aes(x = por, y = fct_rev(as.factor(var)), fill = as.factor(trim))
  ) +
    geom_bar(aes(),
      stat = "identity", position = "dodge2"
    ) +
    geom_text(aes(label = percent(por, accuracy = 0.1)),
      fontface = "bold", family = "Montserrat", size = 10, hjust = -0.1,
      position = position_dodge2(width = 0.9)
    ) +
    ggtitle(
      "% de población mayor de 18 años que se\nsiente insegura en espacio público",
      "Monterrey, 2T-2024"
    ) +
    labs(y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
    tema_euzen(size_var = 40, font_var = "Roboto", lineheight_var = .6) +
    scale_alpha_manual(values = c(0.6, 0.9)) +
    scale_fill_manual(values = rev(met.brewer(name = "Peru1", n = 4))) +
    scale_x_continuous("% de población",
      labels = scales::percent_format(accuracy = 1L),
      limits = c(0, max(bar_temp$por) * 1.05)
    ) +
    guides(
      fill = guide_legend(reverse = T),
      alpha = guide_legend(reverse = T),
    ) +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
    )

  ggsave(paste0(path_output, "ensu_sens_inseg_", i, ".png"), graf,
    width = 12, height = 13
  )
}

# Situaciones alrededor de vivienda ---------------------------------------


#* Limpiar bases----

ensu_full <- map(files, function(x) {
  print(x)

  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  if (grepl("0316|0616|0916", x, ignore.case = T)) {
    foreign::read.dbf(x) %>%
      clean_names() %>%
      rename(
        cve_ent = ent,
        cve_mun = mun,
      ) %>%
      mutate(
        upm_dis = as.numeric(upm_dis),
        fac_sel = as.numeric(fac_sel),
        est_dis = as.numeric(est_dis)
      ) -> temp
  } else {
    foreign::read.dbf(x) %>%
      clean_names() -> temp
  }

  temp %>%
    mutate(
      var_id = "nacional",
      var_cd = case_when(
        cve_ent == "19" & cve_mun == "039" ~ "Monterrey",
        grepl("16$|17$|0318", fecha_var, ignore.case = T) & cd == 34 ~ "Monterrey",
        T ~ "otro"
      )
    ) -> temp

  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)

  vars <- c("var_id", "var_cd")

  funion <- function(z) {
    as_tibble(svyby(
      ~ bp1_4_1 + bp1_4_2 + bp1_4_3 +
        bp1_4_4 + bp1_4_5 + bp1_4_6,
      by = as.formula(paste0("~", z)), design, svymean, vartype = "cvpct"
    ))
  }

  output <- map(vars, funion)

  output %>%
    reduce(full_join) %>%
    select(-matches("cv")) %>%
    pivot_longer(
      cols = matches("bp1"),
      names_to = "var",
      values_to = "por"
    ) %>%
    mutate(
      var_id = coalesce(var_cd, var_id),
      resp = str_sub(var, nchar(var)),
      var = substr(var, 1, nchar(var) - 1),
      fecha = as.Date(
        paste("01", substr(fecha, start = 1, stop = 2),
          substr(fecha, start = 3, stop = 4),
          sep = "-"
        ),
        format = "%d-%m-%y"
      ),
      var = case_when(
        var == "bp1_4_1" ~ "Vandalismo",
        var == "bp1_4_2" ~ "Consumo de alcohol en las calles",
        var == "bp1_4_3" ~ "Robos o asaltos",
        var == "bp1_4_4" ~ "Pandillerismo",
        var == "bp1_4_5" ~ "Venta o consumo de drogas",
        var == "bp1_4_6" ~ "Disparos frecuentes con armas"
      )
    ) %>%
    filter(
      var_id != "otro",
      resp == "1"
    ) %>%
    select(-matches("var_cd")) -> data
})

ensu_full %>%
  reduce(full_join) %>%
  mutate(
    var_id = str_to_title(var_id)
  ) %>%
  filter(grepl("Monterrey", var_id, ignore.case = T)) -> df_atesti

write_rds(df_atesti, "04_temp/df_atestiguacion.rds")
#* Serie de tiempo Nacional vs Monterrey, situaciones cercanas----
df_atesti %>%
  filter(grepl("Monterrey", var_id, ignore.case = T)) %>%
  group_by(var) %>%
  arrange(-por, .by_group = T) %>%
  filter(fecha == "2023-09-01") %>% # slice(1)
  ungroup() %>%
  mutate(
    rank = rank(-por, ties.method = "first")
  ) %>%
  select(var, rank) %>%
  arrange(rank) -> aux_corte

df_temp <- left_join(df_atesti, aux_corte)

funico <- unique(df_temp$var)

for (i in funico) {
  # i = funico[1]

  print(i)

  df_temp %>%
    filter(var == i) %>%
    mutate(trim = as.yearqtr(fecha)) -> temp


  ggplot(
    temp,
    aes(x = trim, y = por, col = var_id, group = 1)
  ) +
    geom_line(size = 3, alpha = .7) +
    geom_point(size = 4) +
    geom_text_repel(
      aes(label = percent(por, accuracy = 0.1)),
      fontface = "bold", family = "Montserrat", size = 10,
      hjust = -1, vjust = 0.5, angle = 90, segment.size = 0.05,
      show.legend = F, color = "grey20"
    ) +
    facet_wrap(~ reorder(var, -por),
      ncol = 2
    ) +
    ggtitle(
      "Atestiguación de delitos y conductas antisociales",
      "Monterrey, 2T-2024"
    ) +
    labs(caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
    tema_euzen(font_var = "Poppins", size_var = 40) +
    scale_size_manual(values = c(3.5, 1.5)) +
    scale_alpha_manual(values = c(0.8, 0.6)) +
    scale_color_manual(values = c(
      rev(met.brewer(name = "OKeeffe1", n = 1)[-3:-4]),
      "grey20"
    )) +
    scale_x_yearqtr("Trimestre", format = "%Y-T%q", n = 9) +
    scale_y_continuous("% de población",
      labels = scales::percent_format(accuracy = 1L),
      limits = c(0, .9)
    ) + # limits = c(min(temp$por), max(temp$por + 0.025))
    guides(
      size = "none",
      col = "none",
      alpha = "none"
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    ) -> graf

  ggsave(paste0(path_output, "ensu_atesti_", aux_corte$rank[aux_corte$var == i], i, ".png"), graf,
    width = 18, height = 9
  )
}

## PERCEPCIÓN VS ATESTIGUACIÓN

df_perc_ates <- df_atesti %>%
  group_by(var_id, fecha) %>%
  summarise(mean_ates = mean(por)) %>%
  left_join(df_percp, by = c("var_id", "fecha")) %>%
  select(-var) %>%
  pivot_longer(cols = c(mean_ates, por), names_to = "var", values_to = "value") %>%
  mutate(var = recode(var, "mean_ates" = "Atestiguación de delitos (promedio)", "por" = "Percepción de inseguridad")) %>%
  select(-c(bp1_1, total))


# Cambios de habitos GDL vs NACIONAL----
#
# #* Limpiar bases----
# files_temp <- files[grep("0623", files, ignore.case = T)];
#
# ensu_full <- map(files_temp, function(x) {
#
#   print(x)
#
#   fecha <- str_remove(x, "/Users/monicamolina/Documents/DATOS_ENSU/CB/ENSU_CB_")
#
#   foreign::read.dbf(x) %>%
#     clean_names() %>%
#     mutate(
#       var_id = "nacional",
#       var_cd = ifelse(cve_ent == "19" &
#                           cve_mun %in% c("039"),
#                         "Monterrey", "otro")
#     ) -> temp
#
#   design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
#
#   vars <- c("var_id", "var_cd")
#
#   funion <- function(z) {
#
#     as_tibble(svyby(~bp1_5_1 + bp1_5_2 + bp1_5_3 + bp1_5_4 + bp1_5_5,
#                     by = as.formula(paste0("~", z)), design, svytotal, vartype = "cvpct"))
#
#   }
#
#   output <- map(vars, funion)
#
#   output %>%
#     reduce(full_join) %>%
#     select(-matches("cv")) %>%
#     pivot_longer(cols = matches("bp1"),
#                  names_to = "var",
#                  values_to = "total") %>%
#     mutate(var_id = coalesce(var_cd, var_id),
#            resp = str_sub(var, nchar(var)),
#            var = substr(var, 1, nchar(var) - 1)) %>%
#     filter(var_id != "otro",
#            resp != "3") %>%
#     select(-matches("guad")) %>%
#     group_by(var_id, var) %>%
#     mutate(
#       tot = sum(total, na.rm = T),
#       por = total / tot,
#       fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2),
#                             substr(fecha, start = 3, stop = 4), sep = "-"),
#                       format = "%d-%m-%y"),
#       var = str_wrap(case_when(
#         var == "bp1_5_1" ~ "Llevar cosas de valor",
#         var == "bp1_5_2" ~ "Caminar de noche en alrededores de su vivienda",
#         var == "bp1_5_3" ~ "Visitar parientes o amigos",
#         var == "bp1_5_4" ~ "Permitir que menores salgan de su vivienda",
#         var == "bp1_5_5" ~ "Otro"
#       ), 15)
#     ) %>%
#     ungroup() %>%
#     filter(resp == "1") -> data
#
# })
#
# ensu_full %>%
#   reduce(full_join) %>%
#   mutate(
#     var_id = str_to_title(var_id)
#   )  -> df_temp
#
# df_temp %>%
#   filter(grepl("guad", var_id, ignore.case = T)) %>%
#   arrange(-por) %>%
#   pull(var) %>%
#   unique() -> orden_var
#
# df_temp$var <- factor(df_temp$var, levels = orden_var)
#
# df_temp %>%
#   filter(var != "Otro") -> df_temp
#
# ggplot(
#   df_temp,
#   aes(x = as.factor(var), y = por, fill = var_id, group = var_id)
# ) +
#   geom_bar(
#     stat = "identity", position = "dodge2") +
#   geom_text(aes(label = percent(por, accuracy = 0.1)),
#             fontface = "bold", family = "Montserrat", size = 18, vjust = -0.5,
#             position = position_dodge2(width = 0.9)) +
#   ggtitle("Cambio de habitos por temor a la delincuencia\nNacional vs. Monterrey",
#           subtitle="% de personas que dejaron de realizar dicha actividad, 2T-2023") +
#   labs(x = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
#   tema_euzen(size_var = 60, font_var = "Roboto", lineheight_var = 0.4) +
#   scale_alpha_manual(values = c(0.9, 0.6)) +
#   scale_fill_manual(values = c(met.brewer(name = "OKeeffe1", n = 2)[-3:-4], "grey20")) +
#   scale_y_continuous("", labels = scales::percent_format(accuracy = 1L)) +
#   theme(
#     legend.title = element_blank(),
#     legend.position = "bottom"
#   ) -> graf
#
# graf

# ggsave(paste0(path_output, "ensu_cam_ruti.png"), graf,
#        width = 10, height = 12)


# Cambios de habitos trimestre----

#* Limpiar bases----
files_temp <- files[grep("0922|0623|0624", files, ignore.case = T)]
ensu_full <- map(files_temp, function(x) {
  print(x)

  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  foreign::read.dbf(x) %>%
    clean_names() %>%
    mutate(
      var_cd = ifelse(cve_ent == "19" &
        cve_mun %in% c("039"),
      "Monterrey", "otro"
      )
    ) -> temp

  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)

  vars <- c("var_cd")

  funion <- function(z) {
    as_tibble(svyby(~ bp1_5_1 + bp1_5_2 + bp1_5_3 + bp1_5_4 + bp1_5_5,
      by = as.formula(paste0("~", z)), design, svytotal, vartype = "cvpct"
    ))
  }

  output <- map(vars, funion)

  output %>%
    reduce(full_join) %>%
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
    filter(
      var_cd != "otro",
      resp != "3"
    ) %>%
    group_by(var_cd, var) %>%
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
      var = str_wrap(case_when(
        var == "bp1_5_1" ~ "Llevar cosas de valor",
        var == "bp1_5_2" ~ "Caminar de noche en alrededores de su vivienda",
        var == "bp1_5_3" ~ "Visitar parientes o amigos",
        var == "bp1_5_4" ~ "Permitir que menores salgan de su vivienda",
        var == "bp1_5_5" ~ "Otro"
      ), 15)
    ) %>%
    ungroup() %>%
    filter(
      resp == "1",
      var != "Otro"
    ) -> data
})

ensu_full %>%
  reduce(full_join) %>%
  mutate(
    trim = factor(as.yearmon(fecha))
  ) -> df_temp


df_temp %>%
  filter(fecha == "2024-06-01") %>%
  arrange(-por) %>%
  pull(var) %>%
  unique() -> orden_var

df_temp$var <- factor(df_temp$var, levels = orden_var)

ggplot(
  df_temp,
  aes(x = as.factor(var), y = por, fill = trim, group = trim)
) +
  geom_bar(
    stat = "identity", position = "dodge2"
  ) +
  geom_text(aes(label = percent(por, accuracy = 0.1)),
    fontface = "bold", family = "Montserrat", size = 7, vjust = -0.5,
    position = position_dodge2(width = 1)
  ) +
  ggtitle("Cambio de hábitos por temor a la delincuencia\nMonterrey",
    subtitle = "% de personas que dejaron de realizar dicha actividad, 2T-2024"
  ) +
  labs(x = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 35, font_var = "Roboto", lineheight_var = .6) +
  scale_alpha_manual(values = c(0.9, 0.6)) +
  scale_fill_manual(values = rev(c(met.brewer(name = "OKeeffe1", n = 3)[-3:-4], "grey20"))) +
  scale_y_continuous("", labels = scales::percent_format(accuracy = 1L)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) -> graf
graf
# esta es la trimestral
ggsave(paste0(path_output, "ensu_cam_ruti.png"), graf,
  width = 13, height = 14
)

# Conflictos sociales por ciudad----

#* Limpiar bases----
files_temp <- files[grep("0624", files, ignore.case = T)]
foreign::read.dbf(files_temp) %>%
  clean_names() %>%
  filter(
    cve_ent == "19",
    cve_mun %in% c(
      "039", "019", "006",
      "026", "021", "046",
      "048"
    )
  ) %>%
  mutate(
    prob_veci = case_when(
      bp2_2_01 == "1" | bp2_2_03 == "1" | bp2_2_04 == "1" |
        bp2_2_06 == "1" ~ "1",
      T ~ "0"
    ),
    prob_ani = case_when(
      bp2_2_09 == "1" ~ "1",
      T ~ "0"
    ),
    prob_estac = case_when(
      bp2_2_05 == "1" ~ "1",
      T ~ "0"
    ),
    prob_auto = case_when(
      bp2_2_15 == "1" ~ "1",
      T ~ "0"
    ),
    prob_chism = case_when(
      bp2_2_08 == "1" ~ "1",
      T ~ "0"
    ),
    prob_trans = case_when(
      bp2_2_02 == "1" ~ "1",
      T ~ "0"
    ),
    prob_borra = case_when(
      bp2_2_10 == "1" ~ "1",
      T ~ "0"
    ),
    prob_graf = case_when(
      bp2_2_16 == "1" ~ "1",
      T ~ "0"
    )
  ) -> temp

design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)

as_tibble(svyby(
  ~ prob_veci + prob_ani + prob_estac +
    prob_auto + prob_chism + prob_trans +
    prob_borra + prob_graf,
  by = ~nom_mun, design, svymean, vartype = "cvpct", na.rm = T
)) -> output

output %>%
  select(-matches("cv")) %>%
  pivot_longer(
    cols = matches("prob"),
    names_to = "var",
    values_to = "total"
  ) %>%
  mutate(
    resp = str_sub(var, nchar(var)),
    var = substr(var, 1, nchar(var) - 1),
    var = str_wrap(case_when(
      var == "prob_veci" ~ "Problemas con vecinos",
      var == "prob_ani" ~ "Problemas relacionados con animales domésticos",
      var == "prob_estac" ~ "Problemas de estacionemiento",
      var == "prob_auto" ~ "Problemas con autoridades relacionadas con seguridad publica",
      var == "prob_chism" ~ "Chismes o malos entendidos",
      var == "prob_trans" ~ "Conflictos en transporte público o privado",
      var == "prob_borra" ~ "Molestias por borrachos, drogadictos o pandillas",
      var == "prob_graf" ~ "Grafiti o pintas a su casa"
    ), 20)
  ) %>%
  filter(resp == "1") -> df_graf
summary(df_graf$nom_mun)

df_graf <- df_graf %>%
  mutate(
    nom_mun = case_when(
      nom_mun == "MONTERREY" ~ "Monterrey",
      nom_mun == "SAN PEDRO GARZA GARCIA" ~ "San Pedro Garza García",
      nom_mun == "APODACA" ~ "Apodaca",
      nom_mun == "GUADALUPE" ~ "Guadalupe",
      nom_mun == "GENERAL ESCOBEDO" ~ "General Escobedo",
      nom_mun == "SAN NICOLAS DE LOS GARZA" ~ "San Nicolás De Los Garza",
      nom_mun == "SANTA CATARINA" ~ "Santa Catarina",
      .default = NA
    ),
    nom_mun = factor(nom_mun,
      levels = c(
        "Santa Catarina", "San Nicolás De Los Garza",
        "General Escobedo", "Guadalupe", "Apodaca",
        "San Pedro Garza García", "Monterrey"
      )
    )
  )


levels(df_graf$nom_mun) <- str_wrap(levels(df_graf$nom_mun), 20)
#* Heatmap de conflictos----
ggplot(
  df_graf,
  aes(x = reorder(var, -total), y = nom_mun, fill = total)
) +
  geom_tile(color = "black") +
  geom_text(aes(label = scales::percent(total, accuracy = 0.1)),
    fontface = "bold", size = 8, family = "Montserrat"
  ) +
  ggtitle(
    " % de población que experimento un conflicto, por motivo que genero conflicto",
    "Ciudades de ZM de Monterrey, 2T 2024"
  ) +
  labs(x = "Motivos", y = "", caption = "Fuente: Encuesta Nacional de Seguridad Publica Urbana - INEGI") +
  tema_euzen(size_var = 35, font_var = "Roboto", lineheight_var = .8) +
  scale_fill_continuous(
    low = "#ffeda0", high = "#f03b20", name = "% de personas",
    labels = percent, breaks = c(0.1, 0.4, 0.7)
  ) +
  guides(fill = "none") +
  theme(axis.text.x = element_text(size = 20)) -> graf
#  scale_x_yearqtr(format = "T%q-%y", n = 6)
ggsave(paste0(path_output, "ensu_conflict.png"), graf,
  width = 18, height = 9
)


# Conflicto MTY - Serie de tiempo -----------------------------------------

files_temp <- files[(grep("18\\.|19\\.|20\\.|21\\.|22\\.|23\\.|24",
  files,
  ignore.case = T))]

ensu_full <-  map(files_temp, function(x) {
  # x <- files_temp[19]

  print(x)

  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  temp <- foreign::read.dbf(x) %>%
    clean_names() %>%
    mutate(
      var_cd = ifelse(
        cve_ent == "19" & cve_mun %in% c("039"),
        "Monterrey", "otro"
        ),
      prob_veci = case_when(
        bp2_2_01 == "1" | bp2_2_03 == "1" | bp2_2_04 == "1" |
          bp2_2_06 == "1" ~ "1",
        T ~ "0"
      ),
      prob_ani = case_when(
        bp2_2_09 == "1" ~ "1",
        T ~ "0"
      ),
      prob_estac = case_when(
        bp2_2_05 == "1" ~ "1",
        T ~ "0"
      ),
      prob_auto = case_when(
        bp2_2_15 == "1" ~ "1",
        T ~ "0"
      ),
      prob_chism = case_when(
        bp2_2_08 == "1" ~ "1",
        T ~ "0"
      ),
      prob_trans = case_when(
        bp2_2_02 == "1" ~ "1",
        T ~ "0"
      ),
      prob_borra = case_when(
        bp2_2_10 == "1" ~ "1",
        T ~ "0"
      ),
      prob_graf = case_when(
        bp2_2_16 == "1" ~ "1",
        T ~ "0"
      )
    )

  design <- svydesign(
    ids = ~upm_dis, 
    weights = ~fac_sel, 
    strata = ~est_dis, 
    data = temp)

  output <- svyby(
    ~ prob_veci + prob_ani + prob_estac +
      prob_auto + prob_chism + prob_trans +
      prob_borra + prob_graf, by = ~var_cd, 
    design = design, 
    svymean, vartype = "cvpct", 
    na.rm = T) %>% 
      as_tibble()

  df_graf <- output %>%
    filter(var_cd == "Monterrey") %>% 
    select(-matches("cv")) %>%
    pivot_longer(
      cols = matches("prob"),
      names_to = "var",
      values_to = "total"
    ) %>%
    mutate(
      fecha = as.Date(
        paste("01", substr(fecha, start = 1, stop = 2),
          substr(fecha, start = 3, stop = 4),
          sep = "-"
        ),
        format = "%d-%m-%y"
      ),
      resp = str_sub(var, nchar(var)),
      var = substr(var, 1, nchar(var) - 1),
      var = str_wrap(case_when(
        var == "prob_veci" ~ "Problemas con vecinos",
        var == "prob_ani" ~ "Problemas relacionados con animales domésticos",
        var == "prob_estac" ~ "Problemas de estacionamiento",
        var == "prob_auto" ~ "Problemas con autoridades relacionadas con seguridad pública",
        var == "prob_chism" ~ "Chismes o malos entendidos",
        var == "prob_trans" ~ "Conflictos en transporte público o privado",
        var == "prob_borra" ~ "Molestias por borrachos, drogadictos o pandillas",
        var == "prob_graf" ~ "Grafiti o pintas a su casa"
      ), 20)
    ) %>%
    filter(resp == "1")
})

df_graf <- ensu_full %>%
  reduce(rbind) %>%
  mutate(
    nom_mun = str_to_title(var_cd),
    trim = as.yearmon(fecha),
    # alphavar = case_when(
    #   var %in% c("Problemas con\nvecinos", "Problemas de\nestacionemiento", 
    #              "Problemas\nrelacionados con\nanimales domesticos") ~ "Color",
    #   T ~ "NoColor"
    # )
  )

df_color <- df_graf %>%
  filter(
    fecha == max(fecha)
    ) %>% 
  mutate(
    alphavar = case_when(
      total >= median(total) ~ "Color",
      .default = "NoColor"
    )
  ) %>% 
  select(
    var, alphavar
  )

df_graf <- left_join(df_graf, df_color, by = "var")

write_rds(df_graf, "04_temp/df_problemas.rds")

## Gráfica -----------------------------------------------------------------

graf <- ggplot(
  df_graf,
  aes(x = trim, y = total, color = reorder(var, -total), alpha = alphavar)
) +
  geom_line(size = 3, show.legend = FALSE) +
  geom_point(size = 5) +
  geom_text_repel(
    data = df_graf %>%
      filter(fecha == as.Date("2024-06-01")),
    aes(label = scales::percent(total, accuracy = 0.1)),
    show.legend = F,
    size = 10,
    min.segment.length = 0.25,
    box.padding = 0.575,
    force = 10,
    family = "Montserrat",
    fontface = "bold",
    segment.color = "black", # Color de las líneas
    segment.size = 0.5,
    direction = "both",
    nudge_x = 0.075
  ) +
  ggtitle(
    " % de población que experimentó un conflicto, por motivo que generó conflicto",
    "Monterrey, 2T 2024"
  ) +
  labs(x = "Trimestre", 
       y = "", 
       caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI"
       ) +
  scale_fill_continuous(
    low = "#ffeda0", high = "#f03b20", name = "% de personas",
    labels = percent, breaks = c(0.1, 0.4, 0.7)
  ) +
  scale_color_manual(values = met.brewer("Paquin"), "") +
  scale_alpha_manual(values = c(1, .5)) +
  scale_y_continuous(label = percent) +
  guides(alpha = "none") +
  scale_x_yearqtr(
    format = "T%q-%y", n = 9,
    limits = c(
      as.yearqtr("2018 Q1"),
      as.yearqtr("2024 Q4")
    )
  ) +
  tema_euzen(size_var = 30, font_var = "Montserrat") +
  theme(
    legend.position = "bottom"
  )

ggsave(paste0(path_output, "ensu_conflicto_mty.png"), graf,
  width = 18, height = 10
)

# Problemas principales ---------------------------------------------------


#* Limpiar bases----
files_temp <- files[grep("0623|1223|0624", files, ignore.case = T)]

ensu_full <- map(files_temp, function(x) {
  print(x)

  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  foreign::read.dbf(x) %>%
    clean_names() %>%
    mutate(
      var_cd = ifelse(cve_ent == "19" &
        cve_mun %in% c("039"),
      "Monterrey", "otro"
      )
    ) -> temp

  design <- svydesign(
    ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)

  as_tibble(svyby(
    ~ bp3_1_01 + bp3_1_02 + bp3_1_03 +
      bp3_1_04 + bp3_1_05 + bp3_1_06 +
      bp3_1_07 + bp3_1_08 + bp3_1_09 +
      bp3_1_10 + bp3_1_11 + bp3_1_12 +
      bp3_1_13 + bp3_1_14,
    by = ~var_cd, design, svymean, vartype = "cvpct", na.rm = T
  )) -> output

  output %>%
    select(-matches("cv")) %>%
    pivot_longer(
      cols = matches("bp3"),
      names_to = "var",
      values_to = "total"
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
        var == "bp3_1_01" ~ "Fallas y fugas en suministro de agua",
        var == "bp3_1_02" ~ "Deficiencias en red de drenaje",
        var == "bp3_1_03" ~ "Coladeras tapadas",
        var == "bp3_1_04" ~ "Falta de tratamiento de aguas residuales",
        var == "bp3_1_05" ~ "Alumbrado público insuficiente",
        var == "bp3_1_06" ~ "Recolección de basura ineficiente",
        var == "bp3_1_07" ~ "Mercados en mal estado",
        var == "bp3_1_08" ~ "Embotellamientos frecuentes",
        var == "bp3_1_09" ~ "Problemas de salud por mal manejo de rastros",
        var == "bp3_1_10" ~ "Baches en calles y avenidas",
        var == "bp3_1_11" ~ "Parques y jardínes descuidados",
        var == "bp3_1_12" ~ "Delincuencia",
        var == "bp3_1_13" ~ "Transporte ineficiente",
        var == "bp3_1_14" ~ "Hospitales saturados o ineficientes"
      ), 20)
    ) %>%
    filter(
      var_cd != "otro",
      resp == "1"
    ) -> data
})

df_graf <- ensu_full %>%
  reduce(full_join) %>%
  mutate(
    fecha = zoo::as.yearmon(fecha, format = "%b/%Y")
  )

write_rds(df_graf, "04_temp/df_problemas_principales.rds")

# df_graf %>%
#   spread(fecha, total) %>%
#   mutate(dif=(`mar 2022`-`mar 2021`)*100) %>%
#   arrange(-dif) -> difproblema

#* Barras de ZM de Monterrey----
ggplot(
  df_graf,
  aes(x = total, y = reorder(var, total), fill = (as.factor(fecha)), group = (as.factor(fecha)))
) +
  geom_bar(
    aes(),
    stat = "identity", position = "dodge2"
  ) +
  geom_text(aes(label = percent(total, accuracy = 0.1)),
    fontface = "bold", family = "Montserrat", size = 4.5, hjust = -0.1,
    position = position_dodge2(width = 0.9)
  ) +
  ggtitle(
    "Principales problemas en su ciudad",
    "Monterrey"
  ) +
  labs(y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 20, font_var = "Roboto", lineheight_var = .8) +
  scale_fill_manual(values = met.brewer(name = "Peru1", n = 3)) +
  scale_x_continuous("% de población", labels = scales::percent_format(accuracy = 1L), limits = c(0, max(df_graf$total + 0.1))) +
  guides(
    fill = guide_legend(reverse = T),
    alpha = guide_legend(reverse = T)
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.title.y = element_text(size = 0.7)
  ) -> graf

graf

ggsave(paste0(path_output, "ensu_prob_P.png"), graf,
  width = 9, height = 10
)

# Principales problemas SERIE DE TIEMPO

files_temp <- files[(grep("1218|19\\.|20\\.|21\\.|22\\.|23\\.|24", files, ignore.case = T))]
ensu_full <- map(files_temp, function(x) {
  print(x)

  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  foreign::read.dbf(x) %>%
    clean_names() %>%
    mutate(
      var_cd = ifelse(cve_ent == "19" &
        cve_mun %in% c("039"),
      "Monterrey", "otro"
      )
    ) -> temp

  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)

  as_tibble(svyby(
    ~ bp3_1_01 + bp3_1_02 + bp3_1_03 +
      bp3_1_04 + bp3_1_05 + bp3_1_06 +
      bp3_1_07 + bp3_1_08 + bp3_1_09 +
      bp3_1_10 + bp3_1_11 + bp3_1_12 +
      bp3_1_13 + bp3_1_14,
    by = ~var_cd, design, svymean, vartype = "cvpct", na.rm = T
  )) -> output

  output %>%
    select(-matches("cv")) %>%
    pivot_longer(
      cols = matches("bp3"),
      names_to = "var",
      values_to = "total"
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
        var == "bp3_1_01" ~ "Fallas y fugas en suministro de agua",
        var == "bp3_1_02" ~ "Deficiencias en red de drenaje",
        var == "bp3_1_03" ~ "Coladeras tapadas",
        var == "bp3_1_04" ~ "Falta de tratamiento de aguas residuales",
        var == "bp3_1_05" ~ "Alumbrado publico insuficiente",
        var == "bp3_1_06" ~ "Recolección de basura ineficiente",
        var == "bp3_1_07" ~ "Mercados en mal estado",
        var == "bp3_1_08" ~ "Embotellamientos frecuentes",
        var == "bp3_1_09" ~ "Problemas de salud por mal manejo de rastros",
        var == "bp3_1_10" ~ "Baches en calles y avenidas",
        var == "bp3_1_11" ~ "Parques y jardines descuidados",
        var == "bp3_1_12" ~ "Delincuencia",
        var == "bp3_1_13" ~ "Transporte ineficiente",
        var == "bp3_1_14" ~ "Hospitales saturados o ineficientes"
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
    fecha = zoo::as.yearqtr(fecha, format = "%b/%Y")
  ) -> df_graf

write_rds(df_graf, "04_temp/ensu_problemas_serie_35.rds")
# usar el código de Graficas_corregidas(Orden).R
#
# #* Serie de ZM de Monterrey----
# ggplot(
#   df_graf,
#   aes(x = fecha, y = reorder(var, total), fill = total)
# )  +
#   geom_tile(data = df_percp %>%
#               filter(trim == "2023 Q3"),
#             color = "black") +
#   geom_tile(color = "black") +
#   geom_text(aes(label = scales::percent(total, accuracy = 1)),
#             fontface = "bold", size = 7, family = "Montserrat") +
#   ggtitle("% de la población que identificó como principal problema",
#           "Monterrey") +
#   labs(x = "Trimestre", y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
#   tema_euzen(font_var = "Roboto", size_var = 25, lineheight_var = .3) +
#   scale_x_yearqtr(format = "T%q-%y", n = 6) +
#   scale_fill_gradientn(colors = met.brewer("Morgenstern"))+
#   guides(fill = "none") -> graf
# graf
#
# # ggsave(paste0(path_output, "ensu_problemas_serie.png"), graf,
# #        width = 18, height = 10)
#

# Expectativa de seguridad en los proximos 12 meses----

#* Limpiar bases----
files_temp <- files[(grep("19\\.|20\\.|21\\.|22\\.|23\\.|24\\.", files, ignore.case = T))]
ensu_full <- map(files_temp, function(x) {
  print(x)

  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  temp <- foreign::read.dbf(x) %>%
    clean_names() %>%
    mutate(
      var_id = "nacional",
      var_cd = ifelse(cve_ent == "19" &
        cve_mun %in% c("039"),
      "Monterrey", "otro"
      ),
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
      est_dis, fac_sel, bp1_3
    ) %>%
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>%
    filter(!is.na(bp1_3))

  data <- temp %>%
    filter(var_cd == "Monterrey") %>%
    group_by(fecha, var_cd, bp1_3) %>%
    summarise(
      por = survey_mean(vartype = "cv"),
      total = survey_total(vartype = "cv")
    )
})

df_graf <- ensu_full %>%
  reduce(full_join) %>%
  mutate(
    var_id = str_to_title(var_cd),
    bp1_3 = factor(case_when(
      bp1_3 == "1" ~ "Mejorará o seguirá igual de bien",
      bp1_3 == "2" ~ "Mejorará o seguirá igual de bien",
      bp1_3 == "3" ~ "Empeorará o seguirá igual de mal",
      bp1_3 == "4" ~ "Empeorara o seguirá igual de mal",
      bp1_3 == "9" ~ "Ns/Nc"
    ), levels = c(
      "Mejorará o seguirá igual de bien",
      "Empeorará o seguirá igual de mal",
      "Ns/Nc"
    )),
    fecha = zoo::as.yearmon(fecha, format = "%b/%Y"),
    var_colr = ifelse(grepl("Ns/Nc", bp1_3, ignore.case = T), "transparent", "color")
  ) %>%
  group_by(fecha, var_id, bp1_3, var_colr) %>%
  summarise(por = sum(por)) %>%
  filter(bp1_3 != "Ns/Nc") %>%
  mutate(
    trim = as.yearqtr(fecha),
    # var_id = fct_reorder(var_id, -por, tail, n = 1, .desc = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  group_by(fecha) %>%
  mutate(
    nudge = ifelse(
      por == max(por),
      0.1, -0.1
    )
  )

levels(df_graf$bp1_3) <- str_wrap(levels(df_graf$bp1_3), 14)

names(df_graf)

#* Stacked bar Zm Monterrey vs nacional----
graf <- ggplot(
  df_graf,
  aes(x = trim, y = por, color = bp1_3)
) +
  geom_line(size = 2, alpha = .8,
            show.legend = FALSE) +
  geom_point(size = 3.5) +
  geom_text_repel(
    data = df_graf %>%
      filter(trim == max(trim)),
    aes(label = percent(por, accuracy = 0.1)),
    angle = 90, size = 10, fontface = "bold",
    family = "Montserrat",
    direction = "y",
    nudge_y = df_graf$nudge,
    show.legend = F, force = 2
  ) +
  ggtitle(
    "Expectativas sobre la delincuencia en los proximos 12 meses",
    "Monterrey, 2T-24"
  ) +
  labs(y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 45, font_var = "Roboto", lineheight_var = .5) +
  scale_alpha_manual(values = c(0.9, 0.6)) +
  scale_color_manual(values = rev(met.brewer(name = "Juarez", n = 2))) +
  # scale_color_manual(values = c("white", "transparent")) +
  scale_x_yearqtr("Trimestre", format = "%qT-%y", n = 5) +
  scale_y_continuous("% de población",
                     labels = scales::percent_format(accuracy = 1L),
                     limits = c(
                       0,
                       max(df_graf$por)*1.2
                     )
  ) +
  guides(fill = guide_legend(reverse = T)) +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  ) 


ggsave(paste0(path_output, "ensu_expect_hist.png"), graf,
  width = 18, height = 10
)

# Barras

files_temp <- files[grep("0923|0623|0624", files, ignore.case = T)]
ensu_full <- map(files_temp, function(x) {
  print(x)

  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")

  foreign::read.dbf(x) %>%
    clean_names() %>%
    mutate(
      var_id = "nacional",
      var_cd = ifelse(cve_ent == "19" &
        cve_mun %in% c("039"),
      "Monterrey", "otro"
      ),
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
      est_dis, fac_sel, bp1_3
    ) %>%
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>%
    filter(!is.na(bp1_3)) -> temp

  temp %>%
    group_by(fecha, var_id, bp1_3) %>%
    summarise(
      por = survey_mean(vartype = "cv"),
      total = survey_total(vartype = "cv")
    ) -> nac

  temp %>%
    filter(var_cd == "Monterrey") %>%
    group_by(fecha, var_cd, bp1_3) %>%
    summarise(
      por = survey_mean(vartype = "cv"),
      total = survey_total(vartype = "cv")
    ) -> estat

  plyr::rbind.fill(nac, estat) %>%
    mutate(
      var_id = coalesce(var_id, var_cd)
    ) %>%
    select(-c(var_cd)) -> data
})

write_rds(ensu_full, "04_temp/ensu_expect_45.rds")

ensu_full %>%
  reduce(full_join) -> expect

expect %>%
  mutate(
    var_id = str_to_title(var_id),
    bp1_3P = factor(case_when(
      bp1_3 == "1" ~ "Mejorará o seguira igual de bien",
      bp1_3 == "2" ~ "Mejorará o seguirá igual de bien",
      bp1_3 == "3" ~ "Empeorara o seguirá igual de mal",
      bp1_3 == "4" ~ "Empeorará o seguirá igual de mal",
      bp1_3 == "9" ~ "Ns/Nc"
    ), levels = c(
      "Mejorara o seguirá igual de bien",
      "Empeorará o seguirá igual de mal",
      "Ns/Nc"
    )),
    fecha = zoo::as.yearmon(fecha, format = "%b/%Y"),
    var_colr = ifelse(grepl("Ns/Nc", bp1_3, ignore.case = T), "transparent", "color")
  ) -> expect

expect %>%
  mutate(
    bp1_3P = factor(case_when(
      is.na(bp1_3P) ~ "Mejorará o seguira igual de bien",
      T ~ bp1_3P
    ))
  ) -> expect


expect %>%
  group_by(fecha, var_id, bp1_3P, var_colr) %>%
  summarise(por = sum(por)) -> df_graf

levels(df_graf$bp1_3P) <- str_wrap(levels(df_graf$bp1_3P), 14)

#* Stacked bar Zm Monterrey vs nacional----
ggplot(
  df_graf,
  aes(x = por, y = fct_rev(as.factor(fecha)), fill = fct_rev(bp1_3P), group = fct_rev(bp1_3P))
) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    data = df_graf %>% filter(por > 0.02), aes(
      label = percent(por, accuracy = 0.1),
      col = var_colr
    ),
    fontface = "bold", family = "Montserrat", size = 6,
    position = position_stack(vjust = 0.5), show.legend = F
  ) +
  facet_wrap(~var_id, nrow = 2) +
  ggtitle(
    "Expectativas sobre la delincuencia \n en los próximos 12 meses",
    "Nacional vs. Monterrey, 3T-23"
  ) +
  labs(y = "Trimestres", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 25, font_var = "Roboto", lineheight_var = .8) +
  scale_alpha_manual(values = c(0.9, 0.6)) +
  scale_fill_manual(values = rev(c(met.brewer(name = "OKeeffe1", n = 2)[-3:-4], "grey20"))) +
  scale_color_manual(values = c("white", "transparent")) +
  scale_x_continuous("% de población", labels = scales::percent_format(accuracy = 1L)) +
  guides(fill = guide_legend(reverse = T)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) -> graf
graf

ggsave(paste0(path_output, "ensu_expect.png"), graf,
  width = 10, height = 11
)
