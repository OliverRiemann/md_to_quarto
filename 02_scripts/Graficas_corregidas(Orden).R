pacman::p_load(tidyverse, scales, purrr, lubridate, janitor, zoo, cowplot, ggrepel, numform, leaflegend, htmltools, ggpol, doBy, viridis, foreign, reldist, cartography, 
               RPostgreSQL, yaml, sf, grid, gridExtra, leaflet, rpostgis, googlesheets4, sp, treemapify, ggbeeswarm, ggthemes, shapefiles, stargazer, rgeos, biscale,
               ggalluvial, htmltools, htmlwidgets, stringr, reshape2, gt, wordcloud, SnowballC, tm, cluster, rgdal, grid, survey, MetBrewer, srvyr, readxl, naturalsort, zoo)

Sys.setlocale(locale = "es_ES.UTF-8")

source("../../01. SCRIPTS/tema_euzen.R")
source("../../01. SCRIPTS/tema_euzen_plus.R")

df_percp <- base::readRDS("04_temp/ensu_percep_capitales_10.rds") 

k <- df_percp %>% filter(trim == as.yearqtr("2024 Q2")) %>% arrange(por) %>% 
  select(var_id, por) %>%  rename(por_2 = por)
df_percp <- full_join(df_percp, k) %>% arrange(por_2)


ggplot(
  df_percp,
  aes(x = trim, y = reorder(var_id, (por_2)), fill = por)) +
  # geom_tile(data = df_percp %>%
  #             filter(trim == "2023 Q2"),
  #           color = "black")        #No se resalta genera nuevo sistema de cordenadas
  geom_tile(color = "black", show.legend = F) +
  #geom_tile(color = "red3", y = "Monterrey", x = as.yearqtr("2023 Q2"), fill = NULL) +
  # geom_tile(aes(linewidth = ifelse(trim == as.yearqtr("2023 Q2"),
  #                                                   0.5, 0.25),
  #                                color = ifelse(trim == as.yearqtr("2023 Q2"),
  #                                                   "black", "red3"))) +
  geom_text(aes(label = scales::percent(por, accuracy = 0.1)), size = 7.5, family = "Montserrat", alpha = 0.6) +
  ggtitle("% de la población mayor de 18 años que se siente insegura",
          "Ciudades capitales, 1T 2021 - 3T 2023") +
  labs(x = "Trimestre", y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI",
       fill = NULL) +
  tema_euzen(size_var = 30, font_var = "Roboto") +
  scale_x_yearqtr(format = "%qT-%y", 
                  breaks = as.yearqtr(c("2021 Q1", "2021 Q3",
                                        "2022 Q1", "2022 Q3",
                                        "2023 Q1", "2023 Q3",
                                        "2024 Q1"))
                  #limits = c(as.yearqtr(c("2021 Q1", "2023 Q2"))), expand = c(0.6, 0.6)
                  ) +
  scale_fill_gradientn(colors = met.brewer("Morgenstern"))+
  #guides(fill = "none") +
  theme( 
    axis.text.y = element_text(hjust = 1)
  ) -> graf

graf

ggsave(filename = "ensu_percep_capitales_Rev_OATC.png", plot =  graf, path = "03_grafs/",
       width = 18, height = 10)


#________########

ensu_problemas_serie_35 <- readRDS("04_temp/ensu_problemas_serie_35.rds")
ensu_problemas_serie_35 -> df_graf
sort(unique(df_graf$fecha))
k <- df_graf %>% filter(fecha == as.yearqtr("2024 Q2" )) %>% arrange(total) %>% 
  select(var, total) %>%  rename(total_2 = total)
df_graf <- full_join(df_graf, k) %>% arrange(total_2)



ggplot(
  df_graf,
  aes(x = fecha, y = reorder(var, total_2), fill = total)
)  +
  geom_tile(color = "black") +
  geom_text(aes(label = scales::percent(total, accuracy = 1)), size = 6.5, family = "Montserrat", alpha = 0.8) +
  ggtitle("% de la población que identificó como principal problema",
          "Monterrey") +
  labs(x = "Trimestre", y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(font_var = "Roboto", size_var = 24, lineheight_var = 0.7) +
  scale_x_yearqtr(format = "T%q-%y", n = 6) +
  scale_fill_gradientn(colors = met.brewer("Morgenstern"))+
  guides(fill = "none") -> graf
graf

ggsave("ensu_problemas_serie_REV.png",path = "03_grafs/",
        width = 18, height = 10)



ensu_problemas_serie_35 <- readRDS("Desktop/ensu_problemas_serie_35.rds")

ensu_expect_45 <- readRDS("04_temp/ensu_expect_45.rds")

ensu_full <- ensu_expect_45

df_graf <- ensu_full %>% 
  reduce(full_join) %>% 
  mutate(
    var_id = str_to_title(var_id),
    bp1_3 = factor(case_when(
      bp1_3 == "1" ~ "Mejorará o seguirá igual de bien",
      bp1_3 == "2" ~ "Mejorará o seguirá igual de bien",
      bp1_3 == "3" ~ "Empeorará o seguirá igual de mal",
      bp1_3 == "4" ~ "Empeorará o seguirá igual de mal",
      bp1_3 == "9" ~ "Ns/Nc"
    ), levels = c("Mejorará o seguirá igual de bien",
                  "Empeorará o seguirá igual de mal",
                  "Ns/Nc")),
    fecha = zoo::as.yearmon(fecha, format = "%b/%Y"),
    var_colr = ifelse(grepl("Ns/Nc", bp1_3, ignore.case = T), "transparent", "color")
  ) %>%
  group_by(fecha, var_id, bp1_3, var_colr) %>%
  summarise(por=sum(por))

levels(df_graf$bp1_3) <- str_wrap(levels(df_graf$bp1_3), 14)

#* Stacked bar Zm Monterrey vs nacional----
graf <- ggplot(
  df_graf,
  aes(x = por, y = fct_rev(as.factor(fecha)), 
      fill = fct_rev(bp1_3), 
      group = fct_rev(bp1_3))
) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = df_graf %>% 
              filter(por > 0.05),
            aes(label = percent(por, accuracy = 0.1), colour = var_colr),
            fontface = "bold", family = "Montserrat", size = 16, 
            position = position_stack(vjust = 0.5), show.legend = F
            )+
  facet_wrap(~var_id, nrow = 2) +
  ggtitle("Expectativas sobre la delincuencia en los próximos 12 meses",
          "Nacional vs. Monterrey, 2T-24") +
  labs(y = "Trimestres", 
       caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 50, font_var = "Roboto", lineheight_var = .3) +
  scale_alpha_manual(values = c(0.9, 0.6)) +
  scale_fill_manual(values = met.brewer(name = "Signac", n = 5)[c(-2, -4)]) +
  scale_color_manual(values = c("white", "transparent")) +
  scale_x_continuous("% de población", labels = scales::percent_format(accuracy = 1L)) +
  guides(fill = guide_legend(reverse = T)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
graf

ggsave(filename = "ensu_expect.png", 
       plot = graf,
       path = "03_grafs/",
       width = 8, height = 9)
