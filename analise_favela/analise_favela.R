library(dplyr)
library(ggplot2)
library(gganimate)
library(lubridate)
library(forcats)

## arrumar Joana D'Arc

shp_vitoria <- rgdal::readOGR(dsn = '../analise_mapas/bairros/', 
                              layer='bairros', verbose = FALSE)

shp_vitoria@data$id = rownames(shp_vitoria@data)
shp_vitoria.points = ggplot2::fortify(shp_vitoria, region = "id")
shp_vitoria.df = dplyr::inner_join(shp_vitoria.points, 
                                   shp_vitoria@data, 
                                   by = "id")

## Lendo dados de Vitória
path_file <- "https://bi.static.es.gov.br/covid19/MICRODADOS.csv"

# Tentando encontrar o encoding do arquivo
# readr::guess_encoding("https://bi.static.es.gov.br/covid19/MICRODADOS.csv")

# Lendo dados
dados <- read.csv(path_file, sep = ";", fileEncoding = "ISO-8859-1")

dados_favelas <- read.csv("favelas.csv")

dados_vitoria_dia <- dplyr::filter(dados, Municipio == "VITORIA") %>%
  janitor::clean_names() %>%
  mutate(datas_info = as.Date(data),
         bairro = fct_recode(bairro,
                    "JOANA D'ARC" = "JOANA DARC")) %>%
  left_join(dados_favelas) %>%
  filter(datas_info < today(), 
         bairro != "Não Encontrado") %>% # 10 casos de bairro não encontrado
  mutate(mortes = ifelse(evolucao == "Óbito pelo COVID-19", 1, 0), 
         favela = ifelse(is.na(Indicador), "Não", "Sim")) %>%
  group_by(favela, bairro, datas_info) %>%
  summarise(n_casos = n(), 
            n_mortes = sum(mortes)) %>%
  tidyr::complete(tidyr::nesting(favela, bairro), 
                  datas_info = seq.Date(min(datas_info),
                                        lubridate::today() - 1, 
                                        by = "day"), 
                  fill = list(n_casos = 0, n_mortes = 0)) %>%
  mutate(datas_character = paste0(day(datas_info), "/", 
                                  month(datas_info), "/", 
                                  year(datas_info))) %>%
  mutate(casos_acumulados = cumsum(n_casos), 
         mortes_acumuladas = cumsum(n_mortes)) %>%
  mutate(letalidade = 100 * mortes_acumuladas/casos_acumulados)

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
  facet_wrap(~ favela) + 
  geom_polygon(aes(x = long, y = lat, group = BAIRRO,  fill = letalidade)) +
  geom_path(aes(long, lat, group = BAIRRO), color = "black", size = 0.1) +
  scale_fill_viridis_c("Letalidade") +
  theme(legend.position = 'bottom') + 
  coord_equal() + 
  theme_void() 


animacao_casos <- g + 
  transition_time(datas_info) 

animation <- animate(animacao_casos, renderer = gifski_renderer(),
                     width = 600, height = 600, duration = 60)

anim_save("animacao_letalidade.gif", animation)


## Fazendo gráfico somente de favelas vs não favelas
dados_favelas_dia <- dplyr::filter(dados, Municipio == "VITORIA") %>%
  janitor::clean_names() %>%
  mutate(datas_info = as.Date(data),
         bairro = fct_recode(bairro,
                             "JOANA D'ARC" = "JOANA DARC")) %>%
  left_join(dados_favelas) %>%
  filter(datas_info < today(), 
         bairro != "Não Encontrado") %>% # 10 casos de bairro não encontrado
  mutate(mortes = ifelse(evolucao == "Óbito pelo COVID-19", 1, 0), 
         favela = ifelse(is.na(Indicador), "Não", "Sim")) %>%
  group_by(favela, datas_info) %>%
  summarise(n_casos = n(), 
            n_mortes = sum(mortes)) %>%
  tidyr::complete(tidyr::nesting(favela), 
                  datas_info = seq.Date(min(datas_info),
                                        lubridate::today() - 1, 
                                        by = "day"), 
                  fill = list(n_casos = 0, n_mortes = 0)) %>%
  tidyr::unnest() %>%
  mutate(datas_character = paste0(day(datas_info), "/", 
                                  month(datas_info), "/", 
                                  year(datas_info))) %>%
  mutate(datas_character2 = fct_relevel(datas_character, 
                                        as.character(datas_character)), 
         casos_acumulados = cumsum(n_casos), 
         mortes_acumuladas = cumsum(n_mortes)) %>%
  mutate(letalidade = round(100 * mortes_acumuladas/casos_acumulados, 1))

g <- ggplot(dados_favelas_dia) + theme_bw() +
  geom_line(aes(x = datas_info, y = letalidade, colour = favela)) +
  theme_minimal() + ylab("Letalidade (%)") + xlab("") + 
  scale_colour_manual(name = "Favela", values = c("#00AFBB", "#E7B800")) + 
  scale_x_date(date_labels = c("Junho", "Abril", "Maio")) + 
  labs(title = "Variação da letalidade da Covid-19 no Espírito Santo", 
       subtitle = "Letalidade = % de pessoas que morreram dentre aquelas com a doença")

animacao_favela <- g + transition_reveal(datas_info)

anim_save("animacao_letalidade_favela.gif", animacao_favela)

## Gráfico de barras 
g <- ggplot(dados_favelas_dia_plot) + theme_bw() +
  geom_bar(aes(x = favela, y = letalidade, fill = favela), 
           stat = "identity") +
  theme_minimal() + ylab("Letalidade (%)") + xlab("Favela") + 
  theme(legend.position = 'none') +
  scale_fill_manual(name = "Favela", 
                    values = c("#00AFBB", "#E7B800")) 

g + transition_reveal(datas_info) 

dados_ultimo_dia <- filter(dados_favelas_dia, datas_info == today() - 1)

g <- ggplot(dados_ultimo_dia) +
  geom_bar(aes(x = favela, y = letalidade, fill = favela), 
           stat = "identity", width = 0.5) +
  geom_text(aes(x = favela, y = letalidade, label = letalidade), 
            vjust = -1, size = 20) + 
  ylim(c(0, 1.15*max(dados_ultimo_dia$letalidade))) +
  theme_minimal() + ylab("Letalidade (%)") + xlab("Favela") + 
  theme(legend.position = 'none') +
  scale_fill_manual(name = "Favela", 
                    values = c("#00AFBB", "#E7B800")) 


g <- g + 
  theme(title = element_text(size = 40),
        axis.text = element_text(size = 30),
        strip.text = element_text(size = 30),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))


ggsave('comparacao_favela.svg', g, width = 15, height = 15)
ggsave('comparacao_favela.png', g, width = 15, height = 15)
