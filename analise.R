## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
## carregando libaries
library(tidyverse)
library(readxl)
library(janitor)

## lendo dados
dados <- read_excel(path = "microdados.xls", na = '-')

## limpando nome das variáveis
dados <- clean_names(dados) 
# a função clean_names() para primeiro ajuste dos nomes das variaveis


## Redefinindo todas variáveis como fatores
dados <- as.data.frame(unclass(dados))

## Deixando a variável data como classe 'Date'
dados <- dados %>% 
  mutate(datas_info = as.Date(dados$data)) %>%
  mutate(data = as.Date(dados$data))


## Criando categoria Cor/Raça negra
dados$raca_cor <- fct_recode(dados$raca_cor,
                    Brancas = "Branca"         
                    Negras = "Parda",
                    Negras = "Preta")

dados$raca_cor <- fct_recode(dados$raca_cor,
                             # Brancas = "Branca"         
                             Negras = "Negra")
                             # Negras = "Preta")

## Reordenando casos
dados$raca_cor <- fct_relevel(dados$raca_cor, "Amarela", "Branca", "Negra",
                             "Indigena", "Ignorado")

## Criando variável indicadora se negro ou não
dados$negro <- fct_recode(dados$raca_cor,
                    Sim = "Negra",
                    Nao = "Amarela", 
                    Nao = "Branca",
                    Nao = "Indigena",
                    Nao = "Ignorado")



## Mudando categorização da variável sexo
dados$sexo <- fct_recode(dados$sexo,
                         Masculino = "M",
                         Feminino = "F",
                         Ignorado = "I")

## Criando faixas etárias
dados$grupo_etario <- fct_recode(dados$faixa_etaria,
                         Jovens = "0 a 4 anos",
			 Jovens = "05 a 9 anos",
			 Jovens = "10 a 19 anos",
			 Jovens = "20 a 29 anos",
			 Adultos = "30 a 39 anos",
			 Adultos = "40 a 49 anos",
			 Adultos = "50 a 59 anos",
			 Idosos = "60 a 69 anos",
			 Idosos = "70 a 79 anos",
			 Idosos = "80 a 89 anos",
			 Idosos = "90 anos ou mais")

## Deixando a base de dados somente brancxs e negrxs
dados <- dados %>% 
  filter(raca_cor == "Negra" | raca_cor == "Branca")


## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  group_by(raca_cor) %>%
  summarise(n_cat = n()) %>%
  mutate(perc = paste0("(", round(100 * n_cat/sum(n_cat), 1), "%)") %>%
           gsub("[.]", ",", .))


g <- ggplot(filter(dadosResumo)) + theme_minimal()

g + geom_bar(aes(x = raca_cor, y = n_cat, fill = raca_cor), stat = 'identity') +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  geom_text(aes(x = raca_cor, y = n_cat, label = n_cat),
            vjust = -2.5) +
  geom_text(aes(x = raca_cor, y = n_cat, label = perc),
            vjust = -1, color = 'grey50') +
  ylim(c(0, max(dadosResumo$n_cat) * 1.12)) + 
  labs(x = "", 
       y = "", 
       title = "Infectados pela COVID-19, por raça/cor",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")



## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(raca_cor) %>%
  summarise(n_cat = n()) %>%
  mutate(perc = paste0("(", round(100 * n_cat/sum(n_cat), 1), "%)") %>%
           gsub("[.]", ",", .))

g <- ggplot(dadosResumo) + theme_minimal()

g + geom_bar(aes(x = raca_cor, y = n_cat, fill = raca_cor), stat = 'identity') +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  geom_text(aes(x = raca_cor, y = n_cat, label = n_cat),
            vjust = -2.5) +
  geom_text(aes(x = raca_cor, y = n_cat, label = perc),
            vjust = -1, color = 'grey50') +
  ylim(c(0, max(dadosResumo$n_cat)*1.12)) + 
  labs(x = "", 
       y = "", 
       title = "Óbitos pela COVID-19, por raça/cor",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")


## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  filter(sexo != "Ignorado") %>%
  group_by(sexo, raca_cor) %>%
  summarise(n_cat = n()) %>%
  mutate(perc = paste0("(", round(100 * n_cat/sum(n_cat), 1), "%)") %>%
           gsub("[.]", ",", .))

g <- ggplot(filter(dadosResumo),
            aes(x = sexo, y = n_cat, group = raca_cor)) + theme_minimal()

g + geom_bar(aes(fill = raca_cor), stat = 'identity', position = 'dodge') +
  theme(legend.position = "bottom") +
  scale_fill_viridis_d(name = "Raça/Cor") +
  geom_text(aes(label = n_cat),
            position = position_dodge(0.9),
            vjust = -2.5) +
  geom_text(aes(label = perc),
            position = position_dodge(0.9),
            vjust = -1, color = 'grey50') +
  ylim(c(0, max(dadosResumo$n_cat)*1.12)) + 
  labs(x = "", 
       y = "", 
       title = "Infectados pela COVID-19, por raça/cor e sexo",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")


## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  filter(sexo != "Ignorado") %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(sexo, raca_cor) %>%
  summarise(n_cat = n()) %>%
  mutate(perc = paste0("(", round(100 * n_cat/sum(n_cat), 1), "%)") %>%
           gsub("[.]", ",", .))

g <- ggplot(filter(dadosResumo),
            aes(x = sexo, y = n_cat, group = raca_cor)) + theme_minimal()

g + geom_bar(aes(fill = raca_cor), stat = 'identity', position = 'dodge') +
  theme(legend.position = "bottom") +
  scale_fill_viridis_d(name = "Raça/Cor") +
  geom_text(aes(label = n_cat),
            position = position_dodge(0.9),
            vjust = -2.5) +
  geom_text(aes(label = perc),
            position = position_dodge(0.9),
            vjust = -1, color = 'grey50') +
  ylim(c(0, max(dadosResumo$n_cat)*1.12)) + 
  labs(x = "", 
       y = "", 
       title = "Óbitos pela COVID-19, por raça/cor e sexo",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")


## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  group_by(grupo_etario, raca_cor) %>%
  summarise(n_cat = n()) %>%
  mutate(perc = paste0("(", round(100 * n_cat/sum(n_cat), 1), "%)") %>%
           gsub("[.]", ",", .))

g <- ggplot(filter(dadosResumo),
            aes(x = grupo_etario, y = n_cat, group = raca_cor)) + theme_minimal()

g + geom_bar(aes(fill = raca_cor), stat = 'identity', position = 'dodge') +
  theme(legend.position = "bottom") +
  scale_fill_viridis_d(name = "Raça/Cor") +
  geom_text(aes(label = n_cat),
            position = position_dodge(0.9),
            vjust = -2.5) +
  geom_text(aes(label = perc),
            position = position_dodge(0.9),
            vjust = -1, color = 'grey50') +
  ylim(c(0, max(dadosResumo$n_cat)*1.12)) + 
  labs(x = "", 
       y = "", 
       title = "Infectados pela COVID-19, por raça/cor e faixa etária",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")


## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(grupo_etario, raca_cor) %>%
  summarise(n_cat = n()) %>%
  mutate(perc = paste0("(", round(100 * n_cat/sum(n_cat), 1), "%)") %>%
           gsub("[.]", ",", .))

g <- ggplot(filter(dadosResumo),
            aes(x = grupo_etario, y = n_cat, group = raca_cor)) + theme_minimal()

g + geom_bar(aes(fill = raca_cor), stat = 'identity', position = 'dodge') +
  theme(legend.position = "bottom") +
  scale_fill_viridis_d(name = "Raça/Cor") +
  geom_text(aes(label = n_cat),
            position = position_dodge(0.9),
            vjust = -2.5) +
  geom_text(aes(label = perc),
            position = position_dodge(0.9),
            vjust = -1, color = 'grey50') +
  ylim(c(0, max(dadosResumo$n_cat)*1.12)) + 
  labs(x = "", 
       y = "", 
       title = "Óbitos pela COVID-19, por raça/cor e faixa etária",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")


## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  group_by(grupo_etario, sexo, raca_cor) %>%
  filter(sexo != "Ignorado") %>%	
  summarise(n_cat = n()) %>%
  mutate(perc = paste0("(", round(100 * n_cat/sum(n_cat), 1), "%)") %>%
           gsub("[.]", ",", .))

g <- ggplot(filter(dadosResumo),
            aes(x = sexo, y = n_cat, group = raca_cor)) + theme_minimal() +
  facet_wrap(~ grupo_etario, scales = "free", ncol = 1)

g + geom_bar(aes(fill = raca_cor), stat = 'identity', position = 'dodge') +
  geom_text(aes(label = n_cat),
            position = position_dodge(0.9),
            vjust = -2.5, size = 2) +
  geom_text(aes(label = perc),
            position = position_dodge(0.9),
            vjust = -1, color = 'grey50', 
	    size = 2) +
  theme(legend.position = "bottom") +
  scale_fill_viridis_d(name = "Raça/Cor") +
  labs(x = "", 
       y = "", 
       title = "Infectados pela COVID-19, por raça/cor e sexo por faixa etária",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")


## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  filter(sexo != "Ignorado") %>%	
  group_by(grupo_etario, sexo, raca_cor) %>%
  summarise(n_cat = n()) %>%
  mutate(perc = paste0("(", round(100 * n_cat/sum(n_cat), 1), "%)") %>%
           gsub("[.]", ",", .))

g <- ggplot(filter(dadosResumo),
            aes(x = sexo, y = n_cat, group = raca_cor)) + theme_minimal() + 
  facet_wrap(~ grupo_etario, scales = 'free', ncol = 1)

g + geom_bar(aes(fill = raca_cor), stat = 'identity', position = 'dodge') +
  theme(legend.position = "bottom") +
  scale_fill_viridis_d(name = "Raça/Cor") +
  geom_text(aes(label = n_cat),
            position = position_dodge(0.9),
            vjust = -2.5, size = 2) +
  geom_text(aes(label = perc),
            position = position_dodge(0.9),
            vjust = -1, color = 'grey50', size = 2) +
  labs(x = "", 
       y = "", 
       title = "Óbitos pela COVID-19, por raça/cor e faixa etária",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")


## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  filter(data < lubridate::today()) %>%
  group_by(raca_cor, data) %>%
  summarise(n_por_dia = n()) %>%
  mutate(total_acumulado = cumsum(n_por_dia))

g <- ggplot(dadosResumo,
            aes(x = data, y = total_acumulado, group = raca_cor)) + theme_minimal()

png("evolucao.png", width = 1000, height = 1000)
g + geom_line(aes(color = raca_cor), size = 2) +
  scale_color_manual(name = "Pessoas", 
                     values = c("#00AFBB", "#E7B800")) +
  scale_x_date(breaks = as.Date(c("2020-04-01", "2020-05-01", "2020-06-01")),
               labels = c("Abril", "Maio", "Junho")) +
  theme(title = element_text(size = 40),
        axis.text = element_text(size = 30),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20)) +
  labs(x = "", 
       y = "", 
       title = "Evolução da infecção da COVID-19 \nno Espírito Santo",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")
dev.off()


## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  filter(data < lubridate::today()) %>%
  group_by(raca_cor, data) %>%
  summarise(n_por_dia = n()) %>%
  mutate(total_acumulado = cumsum(n_por_dia))

g <- ggplot(dadosResumo,
            aes(x = data, y = total_acumulado, group = raca_cor)) + theme_minimal()

png("obitos.png", width = 1000, height = 1000)
g + geom_line(aes(color = raca_cor), size = 2) +
  scale_color_manual(name = "Pessoas", 
                     values = c("#00AFBB", "#E7B800")) +
  scale_x_date(breaks = as.Date(c("2020-04-01", "2020-05-01", "2020-06-01")),
               labels = c("Abril", "Maio", "Junho")) +
  theme(title = element_text(size = 40),
        axis.text = element_text(size = 30),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20)) +
  labs(x = "", 
       y = "", 
       title = "Evolução dos óbitos pela COVID-19 \nno Espírito Santo",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")
dev.off()


## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  filter(data < lubridate::today()) %>%
  filter(sexo != "Ignorado") %>%	
  group_by(sexo, raca_cor, data) %>%
  summarise(n_por_dia = n()) %>%
  mutate(total_acumulado = cumsum(n_por_dia))

g <- ggplot(dadosResumo,
            aes(x = data, y = total_acumulado, group = raca_cor)) + theme_minimal() + 
  facet_wrap(~ sexo, ncol = 1, scales = "free")

g + geom_line(aes(color = raca_cor)) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(name = "Raça/Cor") +
  labs(x = "", 
       y = "", 
       title = "Evolução da infecção da COVID-19, por raça/cor e sexo",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")


## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  filter(sexo != "Ignorado") %>%	
  filter(data < lubridate::today()) %>%
  group_by(sexo, raca_cor, data) %>%
  summarise(n_por_dia = n()) %>%
  mutate(total_acumulado = cumsum(n_por_dia))

g <- ggplot(dadosResumo,
            aes(x = data, y = total_acumulado, group = raca_cor)) + theme_minimal() + 
  facet_wrap(~ sexo, ncol = 1, scales = "free")

g + geom_line(aes(color = raca_cor)) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(name = "Raça/Cor") +
  labs(x = "", 
       y = "", 
       title = "Evolução dos óbitos por COVID-19, por raça/cor e sexo",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")


## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  filter(data < lubridate::today()) %>%
  group_by(grupo_etario, raca_cor, data) %>%
  summarise(n_por_dia = n()) %>%
  mutate(total_acumulado = cumsum(n_por_dia))

g <- ggplot(dadosResumo,
            aes(x = data, y = total_acumulado, group = raca_cor)) + theme_minimal() + 
  facet_wrap(~ grupo_etario, ncol = 1, scales = "free")

g + geom_line(aes(color = raca_cor)) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(name = "Raça/Cor") +
  labs(x = "", 
       y = "", 
       title = "Evolução da infecção da COVID-19, por raça/cor e faixa etária",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")


## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  filter(data < lubridate::today()) %>%
  group_by(grupo_etario, raca_cor, data) %>%
  summarise(n_por_dia = n()) %>%
  mutate(total_acumulado = cumsum(n_por_dia))

g <- ggplot(dadosResumo,
            aes(x = data, y = total_acumulado, group = raca_cor)) + theme_minimal() + 
  facet_wrap(~ grupo_etario, ncol = 1, scales = "free")

g + geom_line(aes(color = raca_cor)) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(name = "Raça/Cor") +
  labs(x = "", 
       y = "", 
       title = "Evolução dos óbitos por COVID-19, por raça/cor e faixa etaria",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")


## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  filter(data < lubridate::today()) %>%
  filter(sexo != "Ignorado") %>%	
  group_by(sexo, grupo_etario, raca_cor, data) %>%
  summarise(n_por_dia = n()) %>%
  mutate(total_acumulado = cumsum(n_por_dia))

g <- ggplot(dadosResumo,
            aes(x = data, y = total_acumulado, group = raca_cor)) + theme_minimal() + 
  facet_grid(grupo_etario ~ sexo, scales = "free_y")

g + geom_line(aes(color = raca_cor)) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(name = "Raça/Cor") +
  labs(x = "", 
       y = "", 
       title = "Evolução da infecção da COVID-19, por raça/cor, sexo e faixa etária",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")


## ------------------------------------------------------------------------
dadosResumo <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  filter(data < lubridate::today()) %>%
  filter(sexo != "Ignorado") %>%	
  filter(grupo_etario != "Jovens") %>% # Ignorando jovens somente para esse gráfico	
  group_by(sexo, grupo_etario, raca_cor, data) %>%
  summarise(n_por_dia = n()) %>%
  mutate(total_acumulado = cumsum(n_por_dia))

g <- ggplot(dadosResumo,
            aes(x = data, y = total_acumulado, group = raca_cor)) + theme_minimal() + 
  facet_grid(grupo_etario ~ sexo, scales = "free")

g + geom_line(aes(color = raca_cor)) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(name = "Raça/Cor") +
  labs(x = "", 
       y = "", 
       title = "Evolução dos óbitos por COVID-19, por raça/cor, sexo e faixa etaria",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")


## ------------------------------------------------------------------------

## Considerando 
min_data <- min(dados$datas_info)
max_data <- max(dados$datas_info)

dados_infectados <- dados %>%
  group_by(raca_cor, datas_info) %>%
  summarise(n_por_dia = n()) %>% 
  complete(nesting(raca_cor), datas_info = seq.Date(min_data, max_data, by = "day"), 
           fill = list(n_por_dia = 0)) %>%
  # filter(datas_info <= "2020-06-01") %>%
  mutate(total_acumulado = cumsum(n_por_dia))

dados_mortes <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(raca_cor, datas_info) %>%
  summarise(morte_por_dia = n()) %>% 
  complete(nesting(raca_cor), datas_info = seq.Date(min_data, max_data, by = "day"), 
           fill = list(morte_por_dia = 0)) %>%
  # filter(datas_info <= "2020-06-01") %>%
  mutate(mortes_acumuladas = cumsum(morte_por_dia))

dados_taxa_mortalidade <- inner_join(dados_infectados, dados_mortes) %>%
  mutate(taxa_mortalidade = 
           ifelse(total_acumulado == 0, 0, 
                  100 * mortes_acumuladas / total_acumulado)) %>%
  filter(datas_info > "2020-04-15") %>%
  filter(datas_info < lubridate::today())

g <- ggplot(dados_taxa_mortalidade,
            aes(x = datas_info, y = taxa_mortalidade, group = raca_cor)) + theme_minimal()

png('mortes.png', width = 1000, height = 1000)
g + geom_line(aes(color = raca_cor), size = 2) +
  scale_color_manual(name = "Pessoas", 
                     values = c("#00AFBB", "#E7B800")) +
  scale_x_date(date_labels = "%d-%m") + 
  theme(title = element_text(size = 40),
        axis.text = element_text(size = 30),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20), 
        
        ) + 
  labs(x = "", 
       y = "Letalidade em (%)", 
       title = "Taxa de letalidade da COVID-19 no ES",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")
dev.off()


## ------------------------------------------------------------------------

## Considerando 
min_data <- min(dados$datas_info)
max_data <- max(dados$datas_info)

dados_infectados <- dados %>%
  group_by(sexo, raca_cor, datas_info) %>%
  summarise(n_por_dia = n()) %>% 
  complete(nesting(raca_cor, sexo), datas_info = seq.Date(min_data, max_data, by = "day"), 
           fill = list(n_por_dia = 0)) %>%
  # filter(datas_info <= "2020-06-01") %>%
  mutate(total_acumulado = cumsum(n_por_dia))

dados_mortes <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(sexo, raca_cor, datas_info) %>%
  summarise(morte_por_dia = n()) %>% 
  complete(nesting(sexo, raca_cor), datas_info = seq.Date(min_data, max_data, by = "day"), 
           fill = list(morte_por_dia = 0)) %>%
  # filter(datas_info <= "2020-06-01") %>%
  mutate(mortes_acumuladas = cumsum(morte_por_dia))

dados_taxa_mortalidade <- inner_join(dados_infectados, dados_mortes) %>%
  mutate(taxa_mortalidade = ifelse(total_acumulado == 0, 0, 
                                   100 * mortes_acumuladas / total_acumulado)) %>%
  filter(taxa_mortalidade > 0) %>%
  filter(datas_info > "2020-04-15") %>%
  filter(datas_info < lubridate::today())

g <- ggplot(dados_taxa_mortalidade,
            aes(x = datas_info, y = taxa_mortalidade, group = raca_cor)) + theme_minimal() + 
  facet_wrap(~ sexo, ncol = 1, scales = 'free')

png('letalidade_sexo.png', width = 1000, height = 1000)
g + geom_line(aes(color = raca_cor)) +
  scale_color_manual(name = "Pessoas", 
                     values = c("#00AFBB", "#E7B800")) +
  scale_x_date(date_labels = "%d-%m") + 
  theme(title = element_text(size = 40),
        axis.text = element_text(size = 30),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20), 
  ) + 
  labs(x = "", 
       y = "Letalidade em (%)", 
       title = "Taxa de letalidade da COVID-19 no ES",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")
dev.off()


## ------------------------------------------------------------------------
g <- ggplot(filter(dados_taxa_mortalidade, datas_info > "2020-04-10"),
            aes(x = datas_info, y = taxa_mortalidade, group = raca_cor)) + theme_minimal() + 
  facet_wrap(~ sexo, ncol = 1) 

g + geom_line(aes(color = raca_cor)) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(name = "Raça/Cor") +
  labs(x = "", 
       y = "", 
       title = "Evolução da taxa de letalidade da COVID-19, por raça/cor e sexo",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")


## ------------------------------------------------------------------------
g <- ggplot(filter(dados_taxa_mortalidade, datas_info > "2020-04-10", raca_cor != "Indigena"),
            aes(x = datas_info, y = taxa_mortalidade, group = raca_cor)) + theme_minimal() + 
  facet_wrap(~ sexo, ncol = 1)

g + geom_line(aes(color = raca_cor)) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(name = "Raça/Cor") +
  labs(x = "", 
       y = "", 
       title = "Evolução da taxa de letalidade da COVID-19, por raça/cor e sexo",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")


## ------------------------------------------------------------------------
dados_infectados <- dados %>%
  group_by(grupo_etario, raca_cor, datas_info) %>%
  summarise(n_por_dia = n()) %>% 
  complete(nesting(grupo_etario, raca_cor), datas_info = seq.Date(min_data, max_data, by = "day"), 
           fill = list(n_por_dia = 0)) %>%
  # filter(datas_info <= "2020-06-01") %>%
  mutate(total_acumulado = cumsum(n_por_dia))

dados_mortes <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(grupo_etario, raca_cor, datas_info) %>%
  summarise(morte_por_dia = n()) %>% 
  complete(nesting(grupo_etario, raca_cor), datas_info = seq.Date(min_data, max_data, by = "day"), 
           fill = list(morte_por_dia = 0)) %>%
  # filter(datas_info <= "2020-06-01") %>%
  mutate(mortes_acumuladas = cumsum(morte_por_dia))

dados_taxa_mortalidade <- inner_join(dados_infectados, dados_mortes) %>%
  mutate(taxa_mortalidade = ifelse(total_acumulado == 0, 0, mortes_acumuladas / total_acumulado)) %>%
  filter(taxa_mortalidade > 0) %>%
  filter(datas_info > "2020-04-15") %>%
  filter(grupo_etario != "Jovens")

g <- ggplot(filter(dados_taxa_mortalidade, datas_info > "2020-04-10", raca_cor != "Indigena"),
            aes(x = datas_info, y = taxa_mortalidade, group = raca_cor)) + theme_minimal() + 
  facet_wrap(~ grupo_etario, ncol = 1, scales = 'free')

png('mortes_idade.png', width = 600, height = 400)
g + geom_line(aes(color = raca_cor)) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(name = "Raça/Cor") +
  labs(x = "", 
       y = "", 
       title = "Evolução da taxa de letalidade da COVID-19, por raça/cor e faixa etaria",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")
dev.off()


## ------------------------------------------------------------------------
dados_infectados <- dados %>%
  group_by(sexo, grupo_etario, raca_cor, datas_info) %>%
  summarise(n_por_dia = n()) %>% 
  complete(nesting(sexo, grupo_etario, raca_cor), datas_info = seq.Date(min_data, max_data, by = "day"), 
           fill = list(n_por_dia = 0)) %>%
  # filter(datas_info <= "2020-06-01") %>%
  mutate(total_acumulado = cumsum(n_por_dia))

dados_mortes <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(sexo, grupo_etario, raca_cor, datas_info) %>%
  summarise(morte_por_dia = n()) %>% 
  complete(nesting(sexo, grupo_etario, raca_cor), datas_info = seq.Date(min_data, max_data, by = "day"), 
           fill = list(morte_por_dia = 0)) %>%
  # filter(datas_info <= "2020-06-01") %>%
  mutate(mortes_acumuladas = cumsum(morte_por_dia))

dados_taxa_mortalidade <- inner_join(dados_infectados, dados_mortes) %>%
  mutate(taxa_mortalidade = ifelse(total_acumulado == 0, 0, mortes_acumuladas / total_acumulado)) %>%
  filter(taxa_mortalidade > 0) %>%
  filter(datas_info >= "2020-04-15")

g <- ggplot(filter(dados_taxa_mortalidade, datas_info > "2020-04-10", raca_cor != "Indigena", 
	           grupo_etario != "Jovens"),
            aes(x = datas_info, y = taxa_mortalidade, group = raca_cor)) + theme_minimal() + 
  facet_grid(grupo_etario ~ sexo, scales = "free_y")

png('mortes_grid.png', width = 1000, height = 1000)
g + geom_line(aes(color = raca_cor)) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(name = "Raça/Cor") +
  labs(x = "", 
       y = "", 
       title = "Evolução da taxa de letalidade, por raça/cor, sexo e faixa etaria",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 09/06/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")
dev.off()

