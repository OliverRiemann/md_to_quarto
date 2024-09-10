# Setup -------------------------------------------------------------------

# Setup -------------------------------------------------------------------
pacman::p_load(
  tidyverse,
  srvyr,
  survey,
  janitor,
  zoo,
  MetBrewer,
  scales
)



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
# Situaciones alrededor de vivienda----

#* Limpiar bases----

ensu_full <- map(files, function(x) {
  
  print(x)
  
  fecha <- str_remove(x, "01_input/DATOS_ENSU/CB/ENSU_CB_") 
  
  if (grepl("0316|0616|0916", x, ignore.case = T)) {
    
    foreign::read.dbf(x) %>%
      clean_names() %>% 
      rename(cve_ent = ent,
             cve_mun = mun,
      ) %>%
      mutate(upm_dis= as.numeric(upm_dis),
             fac_sel= as.numeric(fac_sel),
             est_dis= as.numeric(est_dis)) -> temp
    
  } else {
    
    foreign::read.dbf(x) %>%
      clean_names() -> temp
    
  }
  
  temp %>%
    mutate(
      var_id = "nacional",
      var_guad = case_when(
        cve_ent == "14" & cve_mun == "039" ~ "Guadalajara", 
        grepl("16$|17$|0318", fecha, ignore.case = T) & cd == 24 ~ "Guadalajara",
        T ~ "otro")
    ) -> temp
  
  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
  
  vars <- c("var_id", "var_guad")
  
  funion <- function(z) {
    
    as_tibble(svyby(~bp1_4_1 + bp1_4_2 + bp1_4_3 +
                      bp1_4_4 + bp1_4_5 + bp1_4_6, 
                    by = as.formula(paste0("~", z)), design, svymean, vartype = "cvpct"))
    
  }
  
  output <- map(vars, funion)
  
  output %>% 
    reduce(full_join) %>% 
    select(-matches("cv")) %>% 
    pivot_longer(cols = matches("bp1"),
                 names_to = "var", 
                 values_to = "por") %>% 
    mutate(var_id = coalesce(var_guad, var_id),
           resp = str_sub(var, nchar(var)),
           var = substr(var, 1, nchar(var) - 1),
           fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                                 substr(fecha, start = 3, stop = 4), sep = "-"), 
                           format = "%d-%m-%y"),
           var = case_when(
             var == "bp1_4_1" ~ "Vandalismo",
             var == "bp1_4_2" ~ "Consumo de alcohol en las calles",
             var == "bp1_4_3" ~ "Robos o asaltos",
             var == "bp1_4_4" ~ "Pandillerismo",
             var == "bp1_4_5" ~ "Venta o consumo de drogas",
             var == "bp1_4_6" ~ "Disparos frecuentes con armas"
           )) %>% 
    filter(var_id != "otro",
           resp == "1") %>% 
    select(-matches("guad"))  -> data
  
})

ensu_full %>% 
  reduce(full_join) %>%
  mutate(
    var_id = str_to_title(var_id)) %>% 
  filter(grepl("Guadalajara", var_id, ignore.case = T)) -> df_atesti

#* Serie de tiempo Nacional vs Guadalajara, situaciones cercanas----
df_atesti %>% 
  filter(grepl("Guadalajara", var_id, ignore.case = T)) %>% 
  group_by(var) %>% 
  arrange(-por, .by_group = T) %>% 
  filter(fecha=="2023-09-01") %>%  #slice(1)
  ungroup() %>% 
  mutate(
    rank = rank(-por, ties.method = "first")) %>% 
  select(var, rank) %>%
  arrange(rank) -> aux_corte

df_temp <- left_join(df_atesti, aux_corte)

funico <- unique(df_temp$var)

for (i in funico) {
  
  # i = funico[1]
  
  print(i)
  
  df_temp %>% 
    filter(var == i) %>%
    mutate(trim = as.yearqtr(fecha))-> temp
  
  
  ggplot(
    temp,
    aes(x = trim, y = por, col = var_id, group = 1)
  ) +
    geom_line( size = 3, alpha = .7) +
    geom_point(size = 4) +
    geom_text_repel(
      aes(label = percent(por, accuracy = 0.1)),
      fontface = "bold", family = "Montserrat", size = 15,
      hjust = -1, vjust =  0.5, angle = 90, segment.size  = 0.1,
      show.legend = F, color = "grey20") +
    facet_wrap(~reorder(var, -por), 
               ncol = 2) +
    ggtitle("Atestiguación de delitos y conductas antisociales",
            "Guadalajara, 3T-2023") +
    labs(caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
    tema_euzen(font_var = "Poppins", size_var = 50) +
    scale_size_manual(values = c(3.5, 1.5)) +
    scale_alpha_manual(values = c(0.8, 0.6)) +
    scale_color_manual(values = c(rev(met.brewer(name = "OKeeffe1", n = 1)[-3:-4]), "grey20")) +
    scale_x_yearqtr("Trimestre", format = "%Y-T%q", n  = 9) +
    scale_y_continuous("% de población", labels = scales::percent_format(accuracy = 1L), limits = c(.1, .9)) + #limits = c(min(temp$por), max(temp$por + 0.025))
    guides(size = "none",
           col = "none", 
           alpha = "none") +
    theme(legend.position = "bottom",
          legend.title = element_blank(), strip.text = 
    ) -> graf
  
  ggsave(paste0(path_output, "ensu_atesti_", aux_corte$rank[aux_corte$var==i],i, ".png"), graf,
         width = 18, height = 9)
  
}
