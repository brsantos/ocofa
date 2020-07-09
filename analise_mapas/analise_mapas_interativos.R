library(dplyr)
library(ggplot2)
library(gganimate)
library(lubridate)
library(forcats)
library(leaflet)
library(rgdal)

## arrumar Joana D'Arc

shp_vitoria <- rgdal::readOGR(dsn = './bairros/', 
                              layer = 'bairros', 
                              verbose = FALSE)

plot(shp_vitoria@data)
class(shp_vitoria@data)
plot(shp_vitoria)


## Lendo dados de Vitória
path_file <- "https://bi.static.es.gov.br/covid19/MICRODADOS.csv"

# Tentando encontrar o encoding do arquivo
readr::guess_encoding("https://bi.static.es.gov.br/covid19/MICRODADOS.csv")

# Lendo dados
dados <- read.csv(path_file, sep = ";", fileEncoding = "ISO-8859-1")

dados_vitoria <- dplyr::filter(dados, Municipio == "VITORIA") %>%
  janitor::clean_names() %>%
  mutate(datas_info = as.Date(data_notificacao), 
         bairro = fct_recode(bairro,
                             "JOANA D'ARC" = "JOANA DARC"))   %>%
  filter(datas_info < today()) %>%
  mutate(mortes = ifelse(evolucao == "Óbito pelo COVID-19", 1, 0)) %>%
  group_by(bairro) %>%
  summarise(n_casos = n(), 
            n_mortes = sum(mortes)) %>%
  mutate(letalidade = round(100 * n_mortes/n_casos, 2))

shp_vitoria@data <- shp_vitoria@data %>%
  left_join(dados_vitoria, by = c('BAIRRO' = 'bairro')) 

# Transformando o formato de latitude e longitude
vitoria_ll <- spTransform(shp_vitoria, CRS("+init=epsg:4326"))


# Create a color palette for the map:
mypalette <- colorNumeric(palette = "viridis", 
                          domain = vitoria_ll@data$letalidade, 
                          na.color = "transparent")


# Prepare the text for tooltips:
mytext <- paste(
  "Bairro: ", vitoria_ll@data$BAIRRO,"<br/>", 
  "Número de casos: ", vitoria_ll@data$n_casos, "<br/>", 
  "Número de mortes: ", vitoria_ll@data$n_mortes, "<br/>",
  "Letalidade: ", vitoria_ll@data$letalidade, "<br/>",
  sep = "") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet() %>% 
  addTiles()  %>% 
  setView(lat=-20.3, lng=-40.3, zoom=12) %>%
  addPolygons(data = vitoria_ll, 
    fillColor = ~mypalette(letalidade), 
    stroke = TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight = 0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )) %>%
  addLegend(data = vitoria_ll, 
            pal = mypalette, 
            values = ~letalidade, 
            opacity = 0.9, 
            title = "Letalidade (%)", 
            position = "bottomleft")