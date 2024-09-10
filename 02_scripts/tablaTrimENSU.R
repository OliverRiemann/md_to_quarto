pacman::p_load(tidyverse, scales, lubridate, janitor, cowplot, ggrepel, numform, leaflegend, htmltools, ggpol, doBy, viridis, foreign, reldist, cartography, 
               RPostgreSQL, yaml, sf, grid, gridExtra, leaflet, rpostgis, googlesheets4, sp, treemapify, ggbeeswarm, ggthemes, shapefiles, stargazer, rgeos, biscale,
               ggalluvial, htmltools, htmlwidgets, stringr, reshape2, gt, wordcloud, SnowballC, tm, cluster, rgdal, grid, survey, MetBrewer, srvyr, readxl)

Sys.setlocale(locale = "es_ES.UTF-8")

source("../../01. SCRIPTS/tema_euzen.R")

path_output <- "03_grafs/"
path_temp <- "04_temp/"

options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "adjust")

######################################################### PERCEPCION DE INSEGURIDAD #########################################################

# Percepcion ZMG

#* Cargar bases----
#* 
files <- list.files(path = "01_input/DATOS_ENSU/CB", pattern = "*.dbf", full.names = T, recursive = T)

#* Limpiar bases----
(files_temp <- files[(grep("0623|0324|0624", files, ignore.case = T))])

ensu_full <- map(files_temp, function(x) {
  
  print(x)
  
  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")
  
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
             cve_mun=mun) -> temp
    
  } else {
    
    foreign::read.dbf(x) %>%
      clean_names() -> temp
    
  }
  
  temp %>%
    clean_names() %>% 
    mutate(
      var_id = "nacional",
      var_zm = ifelse(cve_ent == "19" & 
                        cve_mun %in% c("039", "098", 
                                       "101", "120", 
                                       "097"),
                      "zm_guadalajara", "otro"),
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y")
    ) %>% 
    select(fecha, var_id, var_zm, cve_ent, 
           cve_mun, nom_mun, upm_dis, 
           est_dis, fac_sel, bp1_1) %>% 
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>% 
    filter(!is.na(bp1_1)) -> temp
  
   temp %>% 
    group_by(fecha, var_id, bp1_1) %>% 
    summarise(por = survey_mean(vartype = "cv"), 
              total = survey_total(vartype = "cv")) %>% 
    filter(bp1_1 ==  2) -> nac
  
  temp %>% 
    filter(var_zm == "zm_guadalajara") %>% 
    group_by(fecha, var_zm, bp1_1) %>% 
    summarise(por = survey_mean(vartype = "cv"), 
              total = survey_total(vartype = "cv")) %>% 
    filter(bp1_1 ==  2) -> estat
  
  temp %>% 
    filter(var_zm == "zm_guadalajara") %>% 
    group_by(fecha, nom_mun, bp1_1) %>% 
    summarise(por = survey_mean(vartype = "cv"), 
              total = survey_total(vartype = "cv")) %>% 
    filter(bp1_1 ==  2) -> mun
  
  data <- plyr::rbind.fill(nac, estat) %>% 
    plyr::rbind.fill(mun) %>% 
    mutate(
      var_id = coalesce(var_id, var_zm, nom_mun)
    ) %>% 
    select(-c(var_zm, nom_mun))
  
  return(data)
  
})

df_graf <- ensu_full %>% 
  reduce(full_join) %>% 
  mutate(
    var_id = case_when(
      var_id == "zm_guadalajara" ~ "ZM Guadalajara",
      var_id == "nacional" ~ "Nacional",
      T ~ var_id
    )) %>%
  mutate(var_id = str_to_title(var_id),
         var_id=recode(var_id,
                       "Tonala"="Tonalá", 
                       "Tlajomulco De Zuniga"="Tlajomulco De Zuñiga",
                       "Zm Guadalajara"="ZM Guadalajara")
  )


#* Tabla de percepción de trimestre pasado y año pasado nac vs zm----
df_graf %>% 
  filter(fecha == "2023-06-01" | fecha == "2023-09-01" | fecha == "2022-09-01") %>% 
  select(fecha:var_id, total, por) %>% 
  pivot_wider(names_from = fecha,
              values_from = c(total:por)) %>% 
  transmute(
    `Región` = var_id,
    `Sep 2022` = paste0(percent(`por_2022-09-01`, accuracy = 0.1), " (", prettyNum(`total_2022-09-01`, big.mark = ","), ")"),
    `Jun 2023` = paste0(percent(`por_2023-06-01`, accuracy = 0.1), " (", prettyNum(`total_2023-06-01`, big.mark = ","), ")"),
    `Sep 2023` = paste0(percent(`por_2023-09-01`, accuracy = 0.1), " (", prettyNum(`total_2023-09-01`, big.mark = ","), ")"),
    `Diferencia % con trimestre anterior` = paste0(round((`por_2023-09-01` - `por_2023-06-01`) * 100, digits = 1), " pp"),
    `Diferencia % con año anterior` = paste0(round((`por_2023-09-01` - `por_2022-09-01`) * 100, digits = 1), " pp")
  ) %>%
  write_csv("04_temp/tabla_perc.csv")

# Tabla percepción inseguridad por lugar 

# Limpiar bases----

ensu_full <- map(files_temp, function(x) {
  
  print(x)
  
  fecha <- str_extract(x, "(?<=ENSU_CB_)(\\d+)")
  
  foreign::read.dbf(x) %>%
    clean_names() %>% 
    mutate(
      var_id = "nacional",
      var_cd = ifelse(cve_ent == "19" & 
                          cve_mun %in% c("039"),
                        "Monterrey", "otro")
    ) -> temp
  
  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
  
  vars <- c("var_id", "var_cd")
  
  funion <- function(z) {
    
    as_tibble(svyby(~bp1_2_01 + bp1_2_02 + bp1_2_03 +
                      bp1_2_04 + bp1_2_05 + bp1_2_06 +
                      bp1_2_07 + bp1_2_08 + bp1_2_09 +
                      bp1_2_10 + bp1_2_11 + bp1_2_12, 
                    by = as.formula(paste0("~", z)), 
                    design, svytotal, vartype = "cvpct")
              )
    
  }
  
  output <- map(vars, funion)
  
  output %>% 
    reduce(full_join) %>% 
    select(-matches("cv")) %>% 
    pivot_longer(cols = matches("bp1"),
                 names_to = "var", 
                 values_to = "total") %>% 
    mutate(var_id = coalesce(var_cd, var_id),
           resp = str_sub(var, nchar(var)),
           var = substr(var, 1, nchar(var) - 1)) %>% 
    filter(var_id != "otro", 
           resp != "3") %>% 
    select(-matches("guad")) %>% 
    group_by(var_id, var) %>% 
    mutate(
      tot = sum(total, na.rm = T),
      por = total / tot,
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y"),
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
    var_id = str_to_title(var_id)
  ) -> df_graf

df_graf %>% 
  filter(grepl("Monterrey", var_id, ignore.case = T)) %>% 
  select(fecha, total, var, por) %>% 
  pivot_wider(names_from = fecha, values_from = c(total, por)) %>% 
  mutate(
    `Lugar` = var,
    `Jun 2023` = paste0(percent(`por_2023-06-01`, accuracy = 0.1), " (", prettyNum(`total_2023-06-01`, big.mark = ","), ")"),
    `Mar 2024` = paste0(percent(`por_2024-03-01`, accuracy = 0.1), " (", prettyNum(`total_2024-03-01`, big.mark = ","), ")"),
    `Jun 2024` = paste0(percent(`por_2024-06-01`, accuracy = 0.1), " (", prettyNum(`total_2024-06-01`, big.mark = ","), ")"),
    `Diferencia % con trimestre anterior` = paste0(round((`por_2024-06-01` - `por_2024-03-01`) * 100, digits = 1), " pp"),
    `Diferencia % con año anterior` = paste0(round((`por_2024-06-01` - `por_2023-06-01`) * 100, digits = 1), " pp"),
    dif_trim = `por_2024-06-01` - `por_2024-03-01`,
    dif_anu = `por_2024-06-01` - `por_2023-06-01`,
    .keep = "none"
  ) %>% 
  arrange(dif_anu) %>%
  select(-dif_trim, -dif_anu) %>%
  write_csv("04_temp/tabla_lugares.csv")

#cambio rutina
ensu_full <- map(files_temp, function(x) {
  
  print(x)
  
  fecha <- gsub(".dbf", "", str_remove(x, "01_input/DATOS_ENSU/CB/ENSU_CB_")) 
  
  foreign::read.dbf(x) %>%
    clean_names() %>% 
    mutate(
      var_id = "nacional",
      var_cd = ifelse(cve_ent == "19" & 
                          cve_mun %in% c("039"),
                        "Monterrey", "otro")
    ) -> temp
  
  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
  
  vars <- c("var_id", "var_cd")
  
  funion <- function(z) {
    
    as_tibble(svyby(~bp1_5_1 + bp1_5_2 + bp1_5_3 + bp1_5_4 + bp1_5_5, 
                    by = as.formula(paste0("~", z)), design, svytotal, vartype = "cvpct"))
    
  }
  
  output <- map(vars, funion)
  
  output %>% 
    reduce(full_join) %>% 
    select(-matches("cv")) %>% 
    pivot_longer(cols = matches("bp1"),
                 names_to = "var", 
                 values_to = "total") %>% 
    mutate(var_id = coalesce(var_cd, var_id),
           resp = str_sub(var, nchar(var)),
           var = substr(var, 1, nchar(var) - 1)) %>% 
    filter(var_id != "otro", 
           resp != "3") %>% 
    select(-matches("guad")) %>% 
    group_by(var_id, var) %>% 
    mutate(
      tot = sum(total, na.rm = T),
      por = total / tot,
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y"),
      var = str_wrap(case_when(
        var == "bp1_5_1" ~ "Llevar cosas de valor",
        var == "bp1_5_2" ~ "Caminar de noche en alrededores de su vivienda",
        var == "bp1_5_3" ~ "Visitar parientes o amigos",
        var == "bp1_5_4" ~ "Permitir que menores salgan de su vivienda",
        var == "bp1_5_5" ~ "Otro"
      ), 15)
    ) %>% 
    ungroup() %>% 
    filter(resp == "1") -> data
  
})

ensu_full %>% 
  reduce(full_join) %>% 
  mutate(
    var_id = str_to_title(var_id)
  ) -> df_graf

#* Tabla de Guadalajara----
df_graf %>% 
  filter(grepl("guad", var_id, ignore.case = T),
         !(grepl("otro", var, ignore.case = T))) %>% 
  select(var, total, por, fecha) %>% 
  pivot_wider(names_from = fecha,
              values_from = c(total, por))  %>% 
  transmute(
    `Hábito` = var,
    `Sept 2022` = paste0(percent(`por_2022-09-01`, accuracy = 0.1), " (", prettyNum(`total_2022-09-01`, big.mark = ","), ")"),
    `Jun 2023` = paste0(percent(`por_2023-06-01`, accuracy = 0.1), " (", prettyNum(`total_2023-06-01`, big.mark = ","), ")"),
    `Sept 2023` = paste0(percent(`por_2023-09-01`, accuracy = 0.1), " (", prettyNum(`total_2023-09-01`, big.mark = ","), ")"),
    `Diferencia % con trimestre anterior` = paste0(round((`por_2023-09-01` - `por_2023-06-01`) * 100, digits = 1), " pp"),
    `Diferencia % con año anterior` = paste0(round((`por_2023-09-01` - `por_2022-09-01`) * 100, digits = 1), " pp"),
    dif_trim = `por_2023-09-01` - `por_2023-06-01`,
    dif_anu = `por_2023-09-01` - `por_2022-09-01`
  ) %>% 
  arrange(dif_anu) %>%
  select(-c(dif_trim, dif_anu)) %>%
  write_csv("04_temp/tabla_camhab.csv")


