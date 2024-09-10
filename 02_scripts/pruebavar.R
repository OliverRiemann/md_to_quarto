CB23 <- foreign::read.dbf("/Users/monicamolina/Documents/DATOS_ENSU/ENSU_CB_0323.dbf")
CB22 <- read.dbf("/Users/monicamolina/Documents/DATOS_ENSU/CB/ENSU_CB_0322.dbf")
names(CB23)

CB23 <- CB23 %>% 
  clean_names() %>% 
  rename(
  bp1_6_1 = bp1_6_01,
  bp1_6_2 = bp1_6_02,
  bp1_6_3 = bp1_6_03,
  bp1_6_4 = bp1_6_04,
  bp1_6_5 = bp1_6_05,
  bp1_6_6 = bp1_6_06
)

foreign::write.dbf(CB23, "/Users/monicamolina/Documents/DATOS_ENSU/CB/ENSU_CB_0323.dbf")
#################################################### VICTIMAS #################################################### 

# Cargar bases----
files <- list.files(path = "/Users/monicamolina/Documents/DATOS_ENSU/CB", pattern = "*.dbf", full.names = T, recursive = T)

files_temp <- files[(grep("1218|0619|1219|0920|1220|0621|1221|0622|1222", files, ignore.case = T))];
viv_files <- list.files(path = "/Users/monicamolina/Documents/DATOS_ENSU/VIV", pattern="*.dbf", full.names = T, recursive = T)

# Numero de victimas----

#* Limpiar bases----
ensu_full <- lapply(files_temp, function(x) {
  
  print(x)
  
  fecha <- gsub(".dbf", "", str_remove(x, "/Users/monicamolina/Documents/DATOS_ENSU/CB/ENSU_CB_"))  
  
  viv_temp <- viv_files[(grep(fecha, viv_files, ignore.case = T))];
  
})
  
  left_join(
    foreign::read.dbf(x) %>%
      clean_names(),
    foreign::read.dbf(viv_temp) %>% 
      clean_names() %>% 
      select(cve_ent, upm, viv_sel, fac_viv)
  ) %>% 
    mutate(
      var_id = "Nacional",
      var_guad = case_when(
        cd %in% c("65", "66", "67", "68", "69", "70") ~ "ZM Monterrey",
        cd == "64" ~ "Monterrey",
        cd %in% c("61", "62","60", "63") ~ "ZM Guadalajara",
        cd == "59" ~ "Guadalajara",
        cd == "03" ~ "Tijuana", 
        cd == "26" ~ "Toluca",
        cd == "36" ~ "Puebla",
        T ~ "Otro"),
      vict_var = ifelse(bp1_6_1 == "1" | bp1_6_2 == "1" | bp1_6_3 == "1" |
                          bp1_6_4 == "1" | bp1_6_5 == "1" | bp1_6_6 == "1", "1", "0"),
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y")
    ) %>% 
    select(fecha, var_id, var_guad, cve_ent, 
           cve_mun, nom_mun, upm_dis, 
           est_dis, fac_viv, vict_var) %>% 
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_viv) -> temp 
  
  temp %>% 
    group_by(fecha, var_id, vict_var) %>% 
    summarise(por = survey_mean(vartype = "cv"), 
              total = survey_total(vartype = "cv")) -> nac
  
  temp %>% 
    filter(var_guad != "Otro") %>% 
    group_by(fecha, var_guad, vict_var) %>% 
    summarise(por = survey_mean(vartype = "cv"), 
              total = survey_total(vartype = "cv")) -> estat
  
  plyr::rbind.fill(nac, estat) %>% 
    mutate(
      var_id = coalesce(var_id, var_guad),
      vict_var = str_wrap(case_when(
        vict_var == "1" ~ "Hogares con víctima",
        vict_var == "0" ~ "Hogares sin víctima"
      ), 15)
    ) %>% 
    select(-c(var_guad)) -> data
  
})

ensu_full %>% 
  reduce(full_join) %>% 
  mutate(
    fecha = zoo::as.yearmon(fecha, format = "%b/%Y")
  ) %>%
  filter(var_id !="otro",
         vict_var == "Hogares con\nvíctima")-> df_vict

df_vict %>% 
  group_by(var_id) %>% 
  filter(fecha=="2022-12-01") %>% 
  mutate(var_label=paste0(var_id, "(", percent(por, .1), ")"),
         por=case_when(var_id=="ZM Guadalajara" ~ por+.01, 
                       var_id=="Guadalajara" ~ por+.01, 
                       
                       T ~ por)
  ) %>%
  pull(por, var_label) -> last_labels

# Serie de tiempo----
ggplot(
  df_vict,
  aes(x = fecha, y = por, col = var_id)
) +
  geom_line(aes(group = var_id), size = 1.5) +
  geom_point(alpha = 0.6, size = 3) +
  ggtitle("% de hogares víctima de al menos un delito",
          "Ciudades seleccionadas, 4T-2022") +
  labs(x = "Trimestre", 
       caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI
       Nota: 1) Incluye los delitos robo total o parcial de vehículo,robo o asalto en calle o transport epúblico,
       robo en casa habitación, robo en forma distinta a las anteriores yextorsión.") +
  tema_euzen(size_var = 40, font_var = "Roboto") +
  scale_color_manual("", values = met.brewer("Cross")) +
  scale_y_continuous("% de población", labels = scales::percent, limits = c(min(df_vict$por)-.1, max(df_vict$por)+.1),
                     sec.axis = sec_axis(~ ., breaks = last_labels, labels = names(last_labels))) + 
  guides(color = guide_legend(nrow = 2)) +
  theme(
    axis.text.y = element_text(size = 30),
    legend.position = "bottom"
  ) -> graf

ggsave(paste0(path_output, "ensu_vict_comp.png"), graf,
       width = 18, height = 8)

read.dbf("/Users/monicamolina/Documents/DATOS_ENSU/CB/ENSU_CB_0922.dbf")