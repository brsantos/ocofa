source('0_analise_dados.R')

## criando dados para fazer gráficos

#-------------------------------------------
casos <- dados %>%
  group_by(raca_cor) %>%
  summarise(n_casos = n())

mortes <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(raca_cor) %>%
  summarise(n_mortes = n())

letalidade <- inner_join(casos, mortes) %>%
  mutate(letalidade =   ifelse(n_casos == 0, 0, 
                                        round(100 * n_mortes / n_casos, 1)))


#--------------- Sexo -------------------------
# Checar se sexo ignorado está presente 
casos_sexo <- dados %>%
  group_by(raca_cor, sexo) %>%
  summarise(n_casos = n())

mortes_sexo <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(raca_cor, sexo) %>%
  summarise(n_mortes = n())

letalidade_sexo <- inner_join(casos_sexo, mortes_sexo) %>%
  mutate(letalidade =   ifelse(n_casos == 0, 0, 
                               round(100 * n_mortes / n_casos, 1)))


#--------------- Grupo etário -------------------------
# Checar se sexo ignorado está presente 
casos_idade <- dados %>%
  group_by(raca_cor, grupo_etario) %>%
  summarise(n_casos = n())

mortes_idade <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(raca_cor, grupo_etario) %>%
  summarise(n_mortes = n())

letalidade_idade <- inner_join(casos_idade, mortes_idade) %>%
  mutate(letalidade =   ifelse(n_casos == 0, 0, 
                               round(100 * n_mortes / n_casos, 1)))



#--------------- Sexo e Grupo etário -------------------------
# Checar se sexo ignorado está presente 
casos_sexo_idade <- dados %>%
  group_by(raca_cor, grupo_etario, sexo) %>%
  summarise(n_casos = n())

mortes_sexo_idade <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(raca_cor, grupo_etario, sexo) %>%
  summarise(n_mortes = n())

letalidade_sexo_idade <- inner_join(casos_sexo_idade, mortes_sexo_idade) %>%
  mutate(letalidade =   ifelse(n_casos == 0, 0, 
                               round(100 * n_mortes / n_casos, 1)))




###############################################
### Considerando valores em função do tempo ###
###############################################

#-------------------------------------------
casos_t <- dados %>%
  group_by(raca_cor, datas_info) %>%
  summarise(n_casos = n()) %>%
  tidyr::complete(tidyr::nesting(raca_cor), 
                  datas_info = seq.Date(min(dados$datas_info),
                                        max(dados$datas_info), 
                                        by = "day"), 
                  fill = list(n_casos = 0)) %>%
  mutate(casos_acumulados = cumsum(n_casos))


mortes_t <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(raca_cor, datas_info) %>%
  summarise(n_mortes = n()) %>%
  tidyr::complete(tidyr::nesting(raca_cor), 
                  datas_info = seq.Date(min(dados$datas_info),
                                        max(dados$datas_info), 
                                        by = "day"), 
                  fill = list(n_mortes = 0)) %>%
  mutate(mortes_acumulados = cumsum(n_mortes))


letalidade_t <- inner_join(casos_t, mortes_t) %>%
  mutate(letalidade =   ifelse(casos_acumulados == 0, 
                               0, 
                               round(100 * mortes_acumulados / casos_acumulados, 
                                     1)))


#---------------------- Sexo ----------------------------
#-------------------------------------------
casos_sexo_t <- dados %>%
  group_by(raca_cor, sexo, datas_info) %>%
  summarise(n_casos = n()) %>%
  tidyr::complete(tidyr::nesting(raca_cor, sexo), 
                  datas_info = seq.Date(min(dados$datas_info),
                                        max(dados$datas_info), 
                                        by = "day"), 
                  fill = list(n_casos = 0)) %>%
  mutate(casos_acumulados = cumsum(n_casos))


mortes_sexo_t <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(raca_cor, sexo, datas_info) %>%
  summarise(n_mortes = n()) %>%
  tidyr::complete(tidyr::nesting(raca_cor, sexo), 
                  datas_info = seq.Date(min(dados$datas_info),
                                        max(dados$datas_info), 
                                        by = "day"), 
                  fill = list(n_mortes = 0)) %>%
  mutate(mortes_acumulados = cumsum(n_mortes))


letalidade_sexo_t <- inner_join(casos_sexo_t, mortes_sexo_t) %>%
  mutate(letalidade =   ifelse(casos_acumulados == 0, 
                               0, 
                               round(100 * mortes_acumulados / casos_acumulados, 
                                     1)))


#---------------------- Idade  ----------------------------
#-------------------------------------------
casos_idade_t <- dados %>%
  group_by(raca_cor, grupo_etario, datas_info) %>%
  summarise(n_casos = n()) %>%
  tidyr::complete(tidyr::nesting(raca_cor, grupo_etario), 
                  datas_info = seq.Date(min(dados$datas_info),
                                        max(dados$datas_info), 
                                        by = "day"), 
                  fill = list(n_casos = 0)) %>%
  mutate(casos_acumulados = cumsum(n_casos))


mortes_idade_t <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(raca_cor, grupo_etario, datas_info) %>%
  summarise(n_mortes = n()) %>%
  tidyr::complete(tidyr::nesting(raca_cor, grupo_etario), 
                  datas_info = seq.Date(min(dados$datas_info),
                                        max(dados$datas_info), 
                                        by = "day"), 
                  fill = list(n_mortes = 0)) %>%
  mutate(mortes_acumulados = cumsum(n_mortes))


letalidade_idade_t <- inner_join(casos_idade_t, mortes_idade_t) %>%
  mutate(letalidade =   ifelse(casos_acumulados == 0, 
                               0, 
                               round(100 * mortes_acumulados / casos_acumulados, 
                                     1)))


#---------------------- Sexo e Idade  ------------------------
#-------------------------------------------
casos_sexo_idade_t <- dados %>%
  group_by(raca_cor, sexo, grupo_etario, datas_info) %>%
  summarise(n_casos = n()) %>%
  tidyr::complete(tidyr::nesting(raca_cor, sexo, grupo_etario), 
                  datas_info = seq.Date(min(dados$datas_info),
                                        max(dados$datas_info), 
                                        by = "day"), 
                  fill = list(n_casos = 0)) %>%
  mutate(casos_acumulados = cumsum(n_casos))


mortes_sexo_idade_t <- dados %>%
  filter(evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(raca_cor, sexo, grupo_etario, datas_info) %>%
  summarise(n_mortes = n()) %>%
  tidyr::complete(tidyr::nesting(raca_cor, sexo, grupo_etario), 
                  datas_info = seq.Date(min(dados$datas_info),
                                        max(dados$datas_info), 
                                        by = "day"), 
                  fill = list(n_mortes = 0)) %>%
  mutate(mortes_acumulados = cumsum(n_mortes))


letalidade_sexo_idade_t <- inner_join(casos_sexo_idade_t, mortes_sexo_idade_t) %>%
  mutate(letalidade =   ifelse(casos_acumulados == 0, 
                               0, 
                               round(100 * mortes_acumulados / casos_acumulados, 
                                     1)))


