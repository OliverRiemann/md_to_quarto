#ENSU CAPITALES ZMG

pacman::p_load(tidyverse, scales, purrr, lubridate, janitor, zoo, cowplot, ggrepel, numform, leaflegend, htmltools, ggpol, doBy, viridis, foreign, reldist, cartography, 
               RPostgreSQL, yaml, sf, grid, gridExtra, leaflet, rpostgis, googlesheets4, sp, treemapify, ggbeeswarm, ggthemes, shapefiles, stargazer, rgeos, biscale,
               ggalluvial, htmltools, htmlwidgets, stringr, reshape2, gt, wordcloud, SnowballC, tm, cluster, rgdal, grid, survey, MetBrewer, srvyr, readxl, naturalsort, zoo)

Sys.setlocale(locale = "es_ES.UTF-8")

source("~/Desktop/BD-ELECCIONES/01. SCRIPTS/tema_euzen.R")

path_output <- "03_grafs/"
path_temp <- "04_temp/"

options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "adjust")

######################################################### PERCEPCION DE INSEGURIDAD #########################################################

# Percepcion Guadalajara vs Nacional----

#* Cargar bases----
files <- list.files(path = "~/Documents/DATOS_ENSU/CB", pattern = "*.dbf", full.names = T, recursive = T)

files_temp <- files[(grep("0321|0621|0921|1221|0322|0622|0922|1222|0323", files, ignore.case = T))];

#* Limpiar bases----
lapply(files_temp, function(x) {
  
   #x = files_temp[1]
  
  print(x)
  
fecha_var <- factor(gsub(".dbf", "", str_remove(x, "/Users/monicamolina/Documents/DATOS_ENSU/CB/ENSU_CB_")), levels = c("0321","0621","0921","1221","0322","0622","0922","1222","0323"))
fecha_var <- as.character(fecha_var)

  if (grepl("15\\.", x, ignore.case = T)) {
    
    foreign::read.dbf(x) %>%
      clean_names() %>% 
      rename(cve_ent = ent,
             fac_sel = factor,
             est_dis = edis,
             bp1_1 = p1) -> temp
    
  } else if (grepl("0316|0616|0916", x, ignore.case = T)) {
    
    foreign::read.dbf(x) %>%
      clean_names() %>% 
      rename(cve_ent = ent, 
             cve_mun= mun) -> temp
    
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
      
      var_gdl = case_when(
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
        T ~ "Otro")
    ) %>% 
    select(var_id, var_gdl, upm_dis, 
           est_dis, fac_sel, bp1_1) %>% 
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>% 
    filter(!is.na(bp1_1)) -> temp2
  
  vars <- list(list("var_id", "bp1_1"), list("var_gdl", "bp1_1"))
  
  lapply(vars, function(v) {
    
    print(v[[1]])
    
    temp2 %>% 
      group_by_at(.vars = unlist(v)) %>%
      summarise(por = survey_mean(vartype = "cv"), 
                total = survey_total(vartype = "cv"),
                .groups = "drop") %>% 
      mutate(var = v[[1]])
    
  }) -> output
  
  output %>% 
    reduce(full_join) %>% 
    mutate(
      var_id = coalesce(var_id, var_gdl),
      bp1_1 = as.numeric(bp1_1),
      fecha = as.Date(paste("01", substr(fecha_var, start = 1, stop = 2), 
                            substr(fecha_var, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y")
    ) %>% 
    select(-matches("cv|var_gdl")) %>% 
    filter(bp1_1 == "2") -> data
  
}) -> ensu_full  

ensu_full %>% 
  reduce(full_join) %>% 
  filter(var_id!="Otro") %>% 
  mutate(
    trim = as.yearqtr(fecha) ,
    var_id = fct_reorder2(var_id, -por, fecha, tail, n = 1, .desc = TRUE)
    ) -> df_percp

class(df_percp$fecha)
summary(df_percp$fecha)
#ordernar por fecha
  df_percp %>%
    filter(fecha == "2023-03-01") -> df_percp_23
 
 df_percp_23 %>%
   mutate(
     var_id = fct_reorder(var_id, -por, tail, n = 1, .desc = TRUE)
    ) -> df_percp_23
 # 
#* Serie de tiempo Top Min y Máx ----
ggplot(
  df_percp,
  aes(x = trim, y = var_id, fill = por)
) +
  geom_tile(color = "black") +
  geom_text(aes(label = scales::percent(por, accuracy = 0.1)),
            fontface = "bold", size = 6, family = "Montserrat") +
  ggtitle("% de la población mayor de 18 años que se siente insegura",
          "Ciudades capitales, 1T 2021 - 4T 2022") +
  labs(x = "Trimestre", y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 25, font_var = "Roboto") +
  scale_x_yearqtr(format = "%qT-%y") +
  scale_fill_gradientn(colors = met.brewer("Morgenstern"))+
  guides(fill = "none") +
  theme( 
    axis.text.y = element_text(hjust = 1)
  ) -> graf

graf

ggsave(paste0(path_output, "ensu_percep_capitales.png"), graf,
       width = 18, height = 10)

## Comparativo ZMGDL, ZMM, ZMPueb

#* Limpiar bases----
lapply(files, function(x) {
  
  # x = files_temp[1]
  
  print(x)
  
  fecha_var <- gsub(".dbf", "", str_remove(x, "/Users/monicamolina/Documents/DATOS_ENSU/CB/ENSU_CB_")) 
  
  if (grepl("15\\.", x, ignore.case = T)) {
    
    foreign::read.dbf(x) %>%
      clean_names() %>% 
      rename(cve_ent = ent,
             fac_sel = factor,
             est_dis = edis,
             bp1_1 = p1) -> temp
    
  } else if (grepl("0316|0616|0916", x, ignore.case = T)) {
    
    foreign::read.dbf(x) %>%
      clean_names() %>% 
      rename(cve_ent = ent, 
             cve_mun= mun) -> temp
    
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
      
      var_gdl = case_when(
        cd %in%  as.character(60:63) ~ "ZM Guadalajara",
        cd == "59" ~ "Guadalajara",
        cve_ent == "21" & cve_mun =="114" ~ "Puebla",
        cd == "36" ~ "ZM Puebla",
        cd %in% as.character(66:70) ~ "ZM Monterrey",
        cd == 64 ~ "Monterrey",
        cd %in% as.character(71:86) ~ "Ciudad de México",
        T ~ "Otro")
    ) %>% 
    select(var_id, var_gdl, upm_dis, 
           est_dis, fac_sel, bp1_1) %>% 
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>% 
    filter(!is.na(bp1_1)) -> temp2

vars <- list(list("var_id", "bp1_1"), list("var_gdl", "bp1_1"))
  
  lapply(vars, function(v) {
    
    print(v[[1]])
    
    temp2 %>% 
      group_by_at(.vars = unlist(v)) %>%
      summarise(por = survey_mean(vartype = "cv"), 
                total = survey_total(vartype = "cv"),
                .groups = "drop") %>% 
      mutate(var = v[[1]])
    
  }) -> output
  
  output %>% 
    reduce(full_join) %>% 
    mutate(
      var_id = coalesce(var_id, var_gdl),
      bp1_1 = as.numeric(bp1_1),
      fecha = as.Date(paste("01", substr(fecha_var, start = 1, stop = 2), 
                            substr(fecha_var, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y")
    ) %>% 
    select(-matches("var_gdl")) %>% 
    filter(bp1_1 == "2") -> data
  
}) -> ensu_full

ensu_full %>% 
  reduce(full_join) %>% 
  filter(var_id!="Otro") %>% 
  mutate(
    trim = as.yearqtr(fecha),
    alphavar = case_when(var_id %in% c("Ciudad de México", "Guadalajara", "Monterrey") ~ "Color",
                         T ~ "NoColor")
  ) -> df_percp


#* Heatmap de conflictos----
ggplot(
  df_percp,
  aes(x = trim, y = por, color = var_id, alpha = alphavar)) +
  geom_line(size = 3) +
  geom_point(size = 5) +
  geom_text(data = df_percp %>% 
              filter(fecha == as.Date("2023-03-01")),
            aes(label = paste0(scales::percent(por, accuracy = 1), " - ", var_id)),
            fontface = "bold", size = 5, family = "Montserrat", show.legend = F, nudge_x = .1, hjust = 0) +
  ggtitle("% de la población mayor de 18 años que se siente insegura",
          "Ciudades y sus Zonas Metropolitanas, 1T 2021 - 4T 2022") +
  labs(x = "Trimestre", y = "", 
       caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI
       Nota: Las Zonas Metropolitanas (ZM) no incluyen sus respectivas capitales") +
  tema_euzen(size_var = 25, font_var = "Roboto") +
  scale_x_yearqtr(format = "%qT-%y") +
  scale_color_manual(values = met.brewer("Signac", 10)[-c(5,6)] )+
  scale_alpha_manual(values = c(1, .3)) +
  expand_limits(x = as.yearqtr(as.Date(c("2018-03-01", "2023-11-01")))) +
  guides(color = "none", alpha = "none") -> graf
graf
ggsave(paste0(path_output, "ensu_percep_zm.png"), graf,
       width = 16, height = 9)

# Serie de tiempo capitales
files_temp <- files[(grep("18\\.|19\\.|20\\.|21\\.|22\\.23\\.", files, ignore.case = T))];


#* Limpiar bases----
lapply(files_temp, function(x) {
  
  # x = files_temp[1]
  
  print(x)
  
  fecha_var <- gsub(".dbf", "", str_remove(x, "/Users/monicamolina/Documents/DATOS_ENSU/CB/ENSU_CB_")) 
  
  if (grepl("15\\.", x, ignore.case = T)) {
    
    foreign::read.dbf(x) %>%
      clean_names() %>% 
      rename(cve_ent = ent,
             fac_sel = factor,
             est_dis = edis,
             bp1_1 = p1) -> temp
    
  } else if (grepl("0316|0616|0916", x, ignore.case = T)) {
    
    foreign::read.dbf(x) %>%
      clean_names() %>% 
      rename(cve_ent = ent, 
             cve_mun= mun) -> temp
    
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
      
      var_gdl = case_when(
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
        T ~ "Otro")
    ) %>% 
    select(var_id, var_gdl, cd, upm_dis, 
           est_dis, fac_sel, bp1_1) %>% 
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>% 
    filter(!is.na(bp1_1)) -> temp2
  
  vars <- list(list("var_id", "bp1_1"), list("var_gdl", "bp1_1"))
  
  lapply(vars, function(v) {
    
    print(v[[1]])
    
    temp2 %>% 
      group_by_at(.vars = unlist(v)) %>%
      summarise(por = survey_mean(vartype = "cv"), 
                total = survey_total(vartype = "cv"),
                .groups = "drop") %>% 
      mutate(var = v[[1]])
    
  }) -> output
  
  output %>% 
    reduce(full_join) %>% 
    mutate(
      var_id = coalesce(var_id, var_gdl),
      bp1_1 = as.numeric(bp1_1),
      fecha = as.Date(paste("01", substr(fecha_var, start = 1, stop = 2), 
                            substr(fecha_var, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y")
    ) %>% 
    select(-matches("cv|var_gdl")) %>% 
    filter(bp1_1 == "2") -> data
  
}) -> ensu_full

ensu_full %>% 
  reduce(full_join) %>% 
  filter(var_id!="Otro") %>% 
  mutate(
    trim = as.yearqtr(fecha)
  ) -> df_percp

df_percp %>% 
  group_by(var_id) %>% 
  filter(fecha=="2022-09-01") %>% 
  ungroup() %>%
  mutate(var_label=paste0(var_id, " (", percent(por, .1), ")"),
         rank=rank(por),
         por=case_when(var_id=="Zacatecas" ~ por+.01,
                       var_id=="Colima" ~ por+.02,
                       var_id=="Toluca" ~ por+.02,
                       var_id=="Chilpancingo" ~ por-.02,
                       var_id=="San Luis Potosí" ~ por-.04,
                       T ~ por)
  ) %>%
  filter(rank %in% c(seq(1,5), seq(27,32), 16)) -> temp

temp %>%
  pull(por, var_label) -> last_labels

df_percp %>%
  dplyr::semi_join(temp, by=c("var_id")) -> df_graf

#* Heatmap de conflictos----
ggplot(
  df_graf,
  aes(x = trim, y = por, col = reorder(var_id, por))) +
  geom_line(aes(group = var_id), size = 1.5, alpha = .4) +
  geom_line(data=df_graf %>% filter(var_id %in% c("Guadalajara", "Nacional")),
            aes(group = var_id), size = 1.5) +
  geom_point(data=df_graf %>% filter(var_id %in% c("Guadalajara", "Nacional")), size = 3) +
  geom_point(alpha = 0.5, size = 3) +
  ggtitle("% de la población mayor de 18 años que se siente insegura",
          "Los mejores y peores 5 estados") +
  labs(x = "Trimestre", 
       caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 40, font_var = "Roboto") +
  scale_x_yearqtr(format = "T%q-%y", n  = 10) +
  scale_color_manual("", values = c(rev(met.brewer("Paquin")), "#a30ecc"))+
  scale_y_continuous("% de población", labels = scales::percent, limits = c(0, 1),
                     sec.axis = sec_axis(~ ., breaks = last_labels, labels = names(last_labels))) + 
  guides(color = guide_legend(nrow = 2)) +
  theme(
    axis.text.y = element_text(size = 30),
    legend.position = "bottom"
  ) -> graf
graf
ggsave(paste0(path_output, "ensu_percep_minmax.png"), graf,
       width = 16, height = 8)
