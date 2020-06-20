Análise rápida
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.1     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ dplyr   1.0.0
    ## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ───────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
path_file <- "https://bi.static.es.gov.br/covid19/MICRODADOS.csv"

# Tentando encontrar o encoding do arquivo
readr::guess_encoding("https://bi.static.es.gov.br/covid19/MICRODADOS.csv")
```

    ## # A tibble: 1 x 2
    ##   encoding   confidence
    ##   <chr>           <dbl>
    ## 1 ISO-8859-1       0.62

``` r
pop_branca <- 1481678 
pop_negra <-  293334+1708796

dados_populacao <- data.frame(raca_cor = c('Brancas', 'Negras'), 
                              pop = c(pop_branca, pop_negra))

# Lendo dados
dados <- read.csv(path_file, sep = ";", fileEncoding = "ISO-8859-1")

# Limpando os nomes das variaveis
dados <- janitor::clean_names(dados)

# Mudando nome da variável data, pois data também é uma função do R 
# e isso pode causar problemas
dados <- dados %>% 
  mutate(datas_info = as.Date(dados$data)) 



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
```

``` r
casos <- dados %>%
  group_by(raca_cor) %>%
  filter(datas_info <= "2020-04-29") %>%
  summarise(n_casos = n()) 
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
mortes <- dados %>% 
  filter(datas_info <= "2020-04-29") %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(raca_cor) %>%
  summarise(n_mortes = n()) 
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
dados_populacao <- data.frame(raca_cor = c('Brancas', 'Negras'), 
                              pop = c(pop_branca, pop_negra))


letalidade <- inner_join(casos, mortes) %>%
  inner_join(dados_populacao) %>%
  mutate(letalidade =   ifelse(n_casos == 0, 0, 
                               round(100 * n_mortes / n_casos, 1)), 
         incidencia = round(100 * n_casos / pop, 4), 
         mortalidade = round(100 * n_mortes / pop, 4))
```

    ## Joining, by = "raca_cor"

    ## Joining, by = "raca_cor"

``` r
###--------------------------------------------
casos_t <- dados %>%
  group_by(raca_cor, datas_info) %>%
  filter(datas_info <= "2020-04-29") %>%
  summarise(n_casos = n()) %>%
  tidyr::complete(tidyr::nesting(raca_cor), 
                  datas_info = seq.Date(min(dados$datas_info),
                                        as.Date("2020-04-29"), 
                                        by = "day"), 
                  fill = list(n_casos = 0)) %>%
  mutate(casos_acumulados = cumsum(n_casos))
```

    ## `summarise()` regrouping output by 'raca_cor' (override with `.groups` argument)

``` r
mortes_t <- dados %>% 
  filter(datas_info <= "2020-04-29") %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(raca_cor, datas_info) %>%
  summarise(n_mortes = n()) %>%
  tidyr::complete(tidyr::nesting(raca_cor), 
                  datas_info = seq.Date(min(dados$datas_info),
                                        as.Date("2020-04-29"), 
                                        by = "day"), 
                  fill = list(n_mortes = 0)) %>%
  mutate(mortes_acumuladas = cumsum(n_mortes))
```

    ## `summarise()` regrouping output by 'raca_cor' (override with `.groups` argument)

``` r
letalidade_t <- inner_join(casos_t, mortes_t) %>%
  inner_join(dados_populacao) %>%
  mutate(letalidade = 
           ifelse(casos_acumulados == 0, 0, 
                  round(100 * mortes_acumuladas / casos_acumulados, 1)), 
         incidencia = round(100 * casos_acumulados / pop, 4), 
         mortalidade = round(100 * mortes_acumuladas / pop, 4))
```

    ## Joining, by = c("raca_cor", "datas_info")
    ## Joining, by = "raca_cor"

``` r
knitr::kable(letalidade)
```

| raca\_cor | n\_casos | n\_mortes |     pop | letalidade | incidencia | mortalidade |
| :-------- | -------: | --------: | ------: | ---------: | ---------: | ----------: |
| Brancas   |     1096 |        65 | 1481678 |        5.9 |     0.0740 |      0.0044 |
| Negras    |     1273 |        91 | 2002130 |        7.1 |     0.0636 |      0.0045 |

``` r
g <- ggplot(letalidade_t) + 
  aes(x = datas_info, group = raca_cor) + theme_minimal() 

# png('~/SpiderOak Hive/Pos Graduação/Seminários/16. UFES, Jun-20/figuras/mortes_grid.png', width = 600, height = 400)
g <- g +  geom_line(aes(color = raca_cor)) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(name = "Raça/Cor")
  

g + aes(y = letalidade) + 
  labs(x = "", 
       y = "", 
       title = "Evolução da taxa de letalidade, por raça/cor",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 29/04/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")
```

![](analise_rapida_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
g + aes(y = incidencia) + 
  labs(x = "", 
       y = "", 
       title = "Evolução da taxa de incidencia, por raça/cor",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 29/04/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")
```

![](analise_rapida_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
g + aes(y = mortalidade) + 
  labs(x = "", 
       y = "", 
       title = "Evolução da taxa de mortalidade, por raça/cor",
       subtitle = "Estado do Espírito Santo - Período de 29/2 a 29/04/2020",
       caption = "Fonte: Painel COVID-19 - Espírito Santo")
```

![](analise_rapida_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
# dev.off()
```
