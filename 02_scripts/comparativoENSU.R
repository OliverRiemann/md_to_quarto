#Ensu comparativo 

dir.create("01_input") 
dir.create("02_scripts")
dir.create("03_grafs")
dir.create("04_temp")

pacman::p_load(tidyverse, scales, purrr, lubridate, janitor, cowplot, ggrepel, numform, leaflegend, htmltools, ggpol, doBy, viridis, foreign, reldist, cartography, 
               RPostgreSQL, yaml, sf, grid, gridExtra, leaflet, rpostgis, googlesheets4, sp, treemapify, ggbeeswarm, ggthemes, shapefiles, stargazer, rgeos, biscale,
               ggalluvial, htmltools, htmlwidgets, stringr, reshape2, gt, wordcloud, SnowballC, tm, cluster, rgdal, grid, survey, MetBrewer, srvyr, readxl, zoo)

Sys.setlocale(locale = "es_ES.UTF-8")

source("../../01. SCRIPTS/tema_euzen.R")

path_output <- "03_grafs/"
path_temp <- "04_temp/"

options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "adjust")

######################################################### PERCEPCION DE INSEGURIDAD #########################################################

# Percepcion Guadalajara vs Nacional----

#* Cargar bases----
files <- list.files(path = "01_input/DATOS_ENSU/CB", pattern = "*.dbf", full.names = T, recursive = T)

files_temp <- files[(grep("18\\.|19\\.|20\\.|21\\.|22\\.|23\\.", files, ignore.case = T))];

#* Limpiar bases----
lapply(files_temp, function(x) {
  
  # x = files[1]
  
  print(x)
  
  fecha_var <- gsub(".dbf", "", str_remove(x, "01_input/DATOS_ENSU/CB/ENSU_CB_")) 
  
  if (grepl("1216|0317|0617|0917|1217|0318|0920|0618|0918|1218|0319|0619|0919|1219|0320|1220|0321|0621|0921|1221|0322|0622|0922|1222|0323|0623|0923", x, ignore.case = T)) {
    
    foreign::read.dbf(x) %>%
      clean_names() %>%
      rename(CVE_ENT = cve_ent, 
             CVE_MUN = cve_mun,
             ) -> temp
    
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
        cd == "37" ~ "Queretaro",
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
  filter(fecha=="2023-09-01") %>% 
  ungroup() %>%
  mutate(var_label=paste0(var_id, " (", percent(por, .1), ")"),
         rank=rank(por),
         por=case_when(var_id=="Zacatecas" ~ por +.02,
                       var_id=="Toluca" ~ por +.04,
                       var_id=="Chilpancingo" ~ por+.01,
                       var_id=="Guadalajara" ~ por+0.01,
                       var_id=="Puebla" ~ por-0.02,
                       var_id=="Tepic" ~ por+0.02,
                       var_id=="Durango" ~ por+.03,
                       var_id=="Mérida" ~ por,
                       var_id=="Saltillo" ~ por-0.02,
                       var_id=="La Paz" ~ por-0.04,
                       T ~ por)
  ) %>%
  filter(rank %in% c(seq(1,5), seq(28,32), 17)) -> temp

temp %>%
  pull(por, var_label) -> last_labels

df_percp %>%
  dplyr::semi_join(temp, by=c("var_id")) -> df_graf

#* Heatmap de conflictos----
ggplot(
  df_graf,
  aes(x = trim, y = por, col = reorder(var_id, por))) +
  geom_line(aes(group = var_id), size = 1.5, alpha = .3) +
  geom_line(data=df_graf %>% filter(var_id %in% c("Guadalajara", "Nacional")),
            aes(group = var_id), size = 1.7) +
  geom_point(data=df_graf %>% filter(var_id %in% c("Guadalajara", "Nacional")), size = 3) +
  geom_point(alpha = 0.5, size = 3) +
  ggtitle("% de la población mayor de 18 años que se siente insegura",
          "Los mejores y peores 5 estados") +
  labs(x = "Trimestre", 
       caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 23, font_var = "Roboto") +
  scale_x_yearqtr(format = "T%q-%y", n  = 6) +
  scale_color_manual("", values = c(rev(met.brewer("Cassatt2")), "#a37ecc", "#a34ecc"))+
  scale_y_continuous("% de población", labels = scales::percent, limits = c(0, 1),
                     sec.axis = sec_axis(~ ., breaks = last_labels, labels = names(last_labels))) + 
  guides(color = guide_legend(nrow = 2)) +
  theme(
    axis.text.y = element_text(size = 14.5),
    legend.position = "bottom"
  ) -> graf
graf
ggsave(paste0(path_output, "ensu_perc_comp.png"), graf,
       width = 16, height = 8.5)

# Atestiguación----

#* Limpiar bases----
# 
# files_temp <- files[(grep("18\\.|19\\.|20\\.|21\\.|22\\.|23\\.", files, ignore.case = T))];
# 
# ensu_full <- lapply(files_temp, function(x) {
#   
#   print(x)
#   
#   fecha <- str_remove(x, "01_input/DATOS_ENSU/CB/ENSU_CB_") 
#   
#   if (grepl("0316|0616|0916", x, ignore.case = T)) {
#     
#     foreign::read.dbf(x) %>%
#       clean_names() %>% 
#       rename(cve_ent = ent,
#              cve_mun = mun,
#       ) %>%
#       mutate(upm_dis= as.numeric(upm_dis),
#              fac_sel= as.numeric(fac_sel),
#              est_dis= as.numeric(est_dis)) -> temp
#     
#   } else {
#     
#     foreign::read.dbf(x) %>%
#       clean_names() -> temp
#     
#   }
#   
#   temp %>%
#     mutate(
#       var_id = "Nacional",
#       var_guad = case_when(
#         cd %in% c("65", "66", "67", "68", "69", "70") ~ "ZM Monterrey",
#         cd == "64" ~ "Monterrey",
#         cd %in% c("61", "62","60", "63") ~ "ZM Guadalajara",
#         cd == "59" ~ "Guadalajara",
#         cd == "03" ~ "Tijuana", 
#         cd == "26" ~ "Toluca",
#         cd == "36" ~ "Puebla",
#         T ~ "Otro")
#     ) -> temp
#   
#   design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
#   
#   vars <- c("var_id", "var_guad")
#   
#   funion <- function(z) {
#     
#     as_tibble(svyby(~bp1_4_1 + bp1_4_2 + bp1_4_3 +
#                       bp1_4_4 + bp1_4_5 + bp1_4_6, 
#                     by = as.formula(paste0("~", z)), design, svymean, vartype = "cvpct"))
#     
#   }
#   
#   output <- lapply(vars, funion)
#   
#   output %>% 
#     reduce(full_join) %>% 
#     select(-matches("cv")) %>% 
#     pivot_longer(cols = matches("bp1"),
#                  names_to = "var", 
#                  values_to = "por") %>% 
#     mutate(var_id = coalesce(var_guad, var_id),
#            resp = str_sub(var, nchar(var)),
#            var = substr(var, 1, nchar(var) - 1),
#            fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
#                                  substr(fecha, start = 3, stop = 4), sep = "-"), 
#                            format = "%d-%m-%y"),
#            var = case_when(
#              var == "bp1_4_1" ~ "Vandalismo",
#              var == "bp1_4_2" ~ "Consumo de alcohol en las calles",
#              var == "bp1_4_3" ~ "Robos o asaltos",
#              var == "bp1_4_4" ~ "Pandillerismo",
#              var == "bp1_4_5" ~ "Venta o consumo de drogas",
#              var == "bp1_4_6" ~ "Disparos frecuentes con armas"
#            )) %>% 
#     filter(var_id != "Otro",
#            resp == "1") %>% 
#     select(-matches("guad"))  -> data
#   
# })
# 
# ensu_full %>% 
#   reduce(full_join) %>% 
#   group_by(var_id, fecha) %>%
#   summarise(por_mean=mean(por)) -> df_atesti
# 
# df_atesti %>% 
#   group_by(var_id) %>% 
#   filter(fecha=="2023-06-01") %>% 
#   mutate(var_label=paste0(var_id, "(", percent(por_mean, .1), ")"),
#          por_mean=case_when(#var_id=="Guadalajara" ~ por+.022,
#            # var_id=="ZM Guadalajara" ~ por_mean + .015,
#            # var_id=="ZM Monterrey" ~ por_mean - .02,
#            # var_id=="Puebla" ~ por+.01,
#            # var_id=="Monterrey" ~ por-.01,
#            # var_id=="Tijuana" ~ por+.02,
#            # var_id=="Nacional" ~ por+.01,
#            T ~ por_mean)
#   ) %>%
#   pull(por_mean, var_label) -> last_labels
# 
# ggplot(
#   df_atesti,
#   aes(x = fecha, y = por_mean, col = var_id)
# ) +
#   geom_line(size = 1.5) +
#   geom_point(alpha = 0.6, size = 3) +
#    geom_vline(xintercept = as.Date("2020-12-01"), 
#               linetype = "dotdash", size = 1, alpha = 0.75, col = "darkgray") +
#   # geom_text_repel(
#   #   aes(label = percent(por, accuracy = 0.1)),
#   #   fontface = "bold", family = "Montserrat", size = 4.5, hjust = -1.1, vjust =  0.5, angle = 90, segment.size  = 0.2,
#   #   show.legend = F) +
#   labs(title = "Atestiguación de delitos y conductas antisociales (promedio)",
#        subtitle = "Guadalajara (1T-2016 al 2T-2023)",
#        caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
#   scale_color_manual(values = met.brewer("Cross")) +
#   scale_x_date("", date_breaks = "3 month", labels = date_format("%b\n%y")) +
#   scale_y_continuous("% de población", labels = scales::percent, limits = c(min(df_atesti$por_mean)-.1, max(df_atesti$por_mean)+.1),
#                      sec.axis = sec_axis(~ ., breaks = last_labels, labels = names(last_labels))) +
#   tema_euzen() +
#   theme(
#     axis.text.y = element_text(size = 11),
#     legend.position = "none",
#   ) -> graf
# graf
# ggsave(paste0(path_output, "ensu_atesti_comp.png"), graf,
#        width = 18, height = 9)
