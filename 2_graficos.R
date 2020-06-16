source('1_definicoes_graficos.R')

## Definições de cores e tamanhos de fontes


## Função generica para fazer gráficos

faz_grafico_letalidade <- function(sexo = FALSE, idade = FALSE, 
                                   tempo = FALSE, formato_svg = TRUE, 
                                   tabela = FALSE, salvar_tabela = FALSE, 
                                   titulo = NULL, data_inicio = NULL){
  
  nome_plot <- "letalidade"
  
  add_sexo <- ""
  add_idade <- ""
  add_tempo <- ""
  
  if (sexo) add_sexo <- "_sexo"
  if (idade) add_idade <- "_idade"
  if (tempo) add_tempo <- "_t"
  
  nome_plot <- paste0(nome_plot, add_sexo, add_idade, add_tempo)
   
  dados_plot <- get(nome_plot)
  if (!is.null(data_inicio)){
    dados_plot <- filter(dados_plot, datas_info > data_inicio)
  } 
  g <- ggplot(dados_plot) + theme_minimal() 
  
  adiciona_partes_grafico <- function(grafico, ...){
    output <- grafico
    if (sexo & idade) output <- output + facet_grid(grupo_etario ~ sexo, 
                                                    scales = "free", ...)
    else if (sexo) output <- output + facet_wrap(~ sexo, scales = "free", ...)
    else if (idade) output <- output + facet_wrap(~ grupo_etario, 
                                                  scales = "free", ...)
    output
  }
  
  adiciona_cores_fontes <- function(grafico, tempo, ...){
    output <- grafico +
      theme(title = element_text(size = 40),
            axis.text = element_text(size = 30, angle = 45),
            strip.text = element_text(size = 30),
            legend.title = element_text(size = 25),
            legend.text = element_text(size = 20))
    if (tempo){
      output <- output + scale_colour_manual(name = "Pessoas", 
                                             values = c("#00AFBB", "#E7B800"))  
    }
    else {
      output <- output + scale_fill_manual(name = "Pessoas", 
                                             values = c("#00AFBB", "#E7B800"))  
    }
  }
  
  g <- adiciona_partes_grafico(g) 
  
  if (salvar_tabela) write.csv(dados_plot, 
                               paste0(nome_plot, ".csv"), 
                               row.names = FALSE)
  
  if (tabela) print(dados_plot)
  else {
    if (!tempo){
      g <- g + aes(y = letalidade, x = raca_cor) + 
        geom_bar(aes(fill = raca_cor), 
                 stat = 'identity', width = 0.6) +
        geom_text(aes(label = letalidade), vjust = -0.5, size = 20) + 
        ylim(c(0, max(dados_plot$letalidade) * 1.15)) 
    }
    else {
      g <- g + aes(x = datas_info, y = letalidade) + 
        geom_line(aes(color = raca_cor), size = 2)      
    }
    g <- adiciona_cores_fontes(g, tempo)
    g <- g + 
      labs(x = "", 
           y = "", 
           title = titulo,
           caption = "Fonte: Painel COVID-19 - Espírito Santo")
    
    ggsave(paste0(nome_plot, ".svg"), g, width = 15, height = 15)
  }
}


