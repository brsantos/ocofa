## Analise completa


## Carregando libraries ##
library(dplyr) # manipulação dos dados
library(ggplot2) # gráficos


# Caminho do arquivo no painel
path_file <- "https://bi.static.es.gov.br/covid19/MICRODADOS.csv"

# Tentando encontrar o encoding do arquivo
readr::guess_encoding("https://bi.static.es.gov.br/covid19/MICRODADOS.csv")

# Lendo dados
dados <- read.csv(path_file, sep = ";", fileEncoding = "ISO-8859-1")

# Limpando os nomes das variaveis
dados <- janitor::clean_names(dados)

# Mudando nome da variável data, pois data também é uma função do R 
# e isso pode causar problemas
dados <- dados %>% 
  mutate(datas_info = as.Date(dados$data)) 


## checando informações para verificar se por exemplo existem casos do futuro
skimr::skim(dados)

# Se houver dados no futuro, rodar linha abaixo
# dados <- filter(dados, datas_info < lubridate::today())

## Criando categoria Cor/Raça negra
dados$raca_cor <- forcats::fct_recode(dados$raca_cor, 
                                      Brancas = "Branca",         
                                      Negras = "Parda",
                                      Negras = "Preta")

## Mudando categorização da variável sexo
dados$sexo <- forcats::fct_recode(dados$sexo,
                                   Masculino = "M",
                                   Feminino = "F",
                                   Ignorado = "I")

## Criando faixas etárias
dados$grupo_etario <- forcats::fct_recode(dados$faixa_etaria,
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


## Filtrando a base e deixando só com pessoas brancas e negras.
dados <- dados %>% 
  filter(raca_cor == "Negras" | raca_cor == "Brancas")


