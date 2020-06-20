library(dplyr)
library(ggplot2)
library(gganimate)
library(lubridate)

shp_vitoria <- rgdal::readOGR(dsn = './bairros/', 
                              layer='bairros', verbose = FALSE)

plot(shp_vitoria@data)
class(shp_vitoria@data)

shp_vitoria@data$id = rownames(shp_vitoria@data)
shp_vitoria.points = ggplot2::fortify(shp_vitoria, region = "id")
shp_vitoria.df = dplyr::inner_join(shp_vitoria.points, 
                                   shp_vitoria@data, 
                                   by = "id")

## Lendo dados de Vitória
path_file <- "https://bi.static.es.gov.br/covid19/MICRODADOS.csv"

# Tentando encontrar o encoding do arquivo
readr::guess_encoding("https://bi.static.es.gov.br/covid19/MICRODADOS.csv")

# Lendo dados
dados <- read.csv(path_file, sep = ";", fileEncoding = "ISO-8859-1")

dados_vitoria_dia <- dplyr::filter(dados, Municipio == "VITORIA") %>%
  janitor::clean_names() %>%
  mutate(datas_info = as.Date(data))   %>%
  filter(datas_info < today()) %>%
  mutate(mortes = ifelse(evolucao == "Óbito pelo COVID-19", 1, 0)) %>%
  group_by(bairro, datas_info) %>%
  summarise(n_casos = n(), 
            n_mortes = sum(mortes)) %>%
  tidyr::complete(tidyr::nesting(bairro), 
                  datas_info = seq.Date(min(datas_info),
                                        lubridate::today() - 1, 
                                        by = "day"), 
                  fill = list(n_casos = 0, n_mortes = 0)) %>%
  mutate(datas_character = paste0(day(datas_info), "/", 
                                  month(datas_info), "/", 
                                  year(datas_info))) %>%
  mutate(casos_acumulados = cumsum(n_casos), 
         mortes_acumuladas = cumsum(n_mortes))

data_plot_dia <- shp_vitoria.df %>%
  left_join(dados_vitoria_dia, by = c('BAIRRO' = 'bairro')) %>%
  filter(!is.na(datas_info))

x_position <- max(data_plot_dia$long) - 0.90 * diff(range(data_plot_dia$long))
y_position <- min(data_plot_dia$lat) + 0.90 * diff(range(data_plot_dia$lat))

g <- ggplot(data_plot_dia) + theme_bw() +
  geom_path(data = shp_vitoria.df, aes(long, lat, group = BAIRRO), 
            size = 0.2) + 
  geom_text(aes(x = x_position, y = y_position,
                 group = datas_character, label = datas_character), 
            family = "Palatino") +
  geom_polygon(aes(x = long, y = lat, group = BAIRRO,  fill = n_casos)) +
  geom_path(aes(long, lat, group = BAIRRO), color = "black", size = 0.1) + 
  coord_equal() + 
  theme_void() +
  scale_fill_viridis_c("Número de casos") 


animacao_casos <- g + 
  transition_time(datas_info) 

animation <- animate(animacao_casos, renderer = gifski_renderer(),
                     width = 600, height = 600, duration = 60)

anim_save("animacao_casos2.gif", animation)


## Casos acumulados
g <- ggplot(data_plot_dia) + theme_bw() +
  geom_path(data = shp_vitoria.df, aes(long, lat, group = BAIRRO), 
            size = 0.2, colour = 'grey80') + 
  geom_polygon(aes(x = long, y = lat, group = BAIRRO,  
                   fill = casos_acumulados)) +
  geom_text(aes(x = x_position, y = y_position,
                group = datas_character, label = datas_character), 
            family = "Palatino") +
  coord_equal() + 
  theme_void() + 
  scale_fill_viridis_c("Número de casos") +
  theme(axis.text = element_blank())

animacao_casos <- g + 
  transition_time(datas_info) + 
  ggtitle('Data = {frame_time}')

animation <- animate(animacao_casos, renderer = gifski_renderer(),
                     width = 600, height = 600, duration = 60)


anim_save("animacao_casos_acumulados.gif", animation)


## Considerando somente pessoas negras
dados$raca_cor <- forcats::fct_recode(dados$RacaCor, 
                                      Brancas = "Branca",         
                                      Negras = "Parda",
                                      Negras = "Preta")

dados_vitoria_dia_negros <- dplyr::filter(dados, Municipio == "VITORIA", 
                                          raca_cor == "Negras") %>%
  janitor::clean_names() %>%
  mutate(datas_info = as.Date(data)) %>%
  filter(datas_info < lubridate::today()) %>%
  mutate(mortes = ifelse(evolucao == "Óbito pelo COVID-19", 1, 0)) %>%
  group_by(bairro, datas_info) %>%
  summarise(n_casos = n(), 
            n_mortes = sum(mortes)) %>%
  tidyr::complete(tidyr::nesting(bairro), 
                  datas_info = seq.Date(min(datas_info),
                                        lubridate::today() - 1, 
                                        by = "day"), 
                  fill = list(n_casos = 0, n_mortes = 0)) %>%
  mutate(datas_character = paste0(day(datas_info), "/", 
                                  month(datas_info), "/", 
                                  year(datas_info))) %>%
  mutate(casos_acumulados = cumsum(n_casos), 
         mortes_acumuladas = cumsum(n_mortes))

data_plot_dia_negros <- shp_vitoria.df %>%
  left_join(dados_vitoria_dia_negros, by = c('BAIRRO' = 'bairro')) %>%
  filter(!is.na(datas_info))

g <- ggplot(data_plot_dia_negros) + theme_bw() +
  geom_path(data = shp_vitoria.df, aes(long, lat, group = BAIRRO), 
            size = 0.2) + 
  geom_polygon(aes(x = long, y = lat, group = BAIRRO,  fill = n_casos)) +
  geom_text(aes(x = x_position, y = y_position,
                group = datas_character, label = datas_character), 
            family = "Palatino") +
  geom_path(aes(long, lat, group = BAIRRO), color = "black", size = 0.1) + 
  coord_equal() + 
  theme_void() + 
  scale_fill_viridis_c("Número de casos") +
  theme(axis.text = element_blank())

animacao_casos <- g + 
  transition_time(datas_info) 

animation <- animate(animacao_casos, renderer = gifski_renderer(),
                     width = 600, height = 600, duration = 60)

anim_save("animacao_casos_negros.gif", animation)


## Casos acumulados
g <- ggplot(data_plot_dia_negros) + theme_bw() +
  geom_path(data = shp_vitoria.df, aes(long, lat, group = BAIRRO), 
            size = 0.2, colour = 'grey80') + 
  geom_polygon(aes(x = long, y = lat, group = BAIRRO,  
                   fill = casos_acumulados)) +
  geom_text(aes(x = x_position, y = y_position,
                group = datas_character, label = datas_character)
  coord_equal() + 
  theme_void() + 
  scale_fill_viridis_c("Número de casos") +
  theme(axis.text = element_blank())

animacao_casos <- g + 
  transition_time(datas_info) 

animation <- animate(animacao_casos, renderer = gifski_renderer(),
                     width = 600, height = 600, duration = 60)


anim_save("animacao_casos_acumulados_negros.gif", animation)


