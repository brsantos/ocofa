source('2_graficos.R')

# Para fazer tabela é só colocar tabela = TRUE
faz_grafico_letalidade(tabela = TRUE)

# Para salvar gráfico, é só não escrever tabela = TRUE
faz_grafico_letalidade()

# Para colocar um título no gráfico
faz_grafico_letalidade(titulo = "Titulo do gráfico")

# Para salvar uma tabela com os valores e imprimir a tabela no console
faz_grafico_letalidade(salvar_tabela = TRUE, tabela = TRUE)

# Para salvar uma tabela com os valores e salvar o gráfico
faz_grafico_letalidade(salvar_tabela = TRUE)

# Para fazer gráfico acumulado no tempo é só fazer
faz_grafico_letalidade(tempo = TRUE, titulo = "Meu título")

# Para escolher data de inicio para melhorar a escala dos gráficos
faz_grafico_letalidade(tempo = TRUE, titulo = "Meu título", 
                       data_inicio = "2020-04-15")

# Para ir adicionando outras variáveis é só colocar 
faz_grafico_letalidade(sexo = TRUE, tempo = TRUE, titulo = "Meu título", 
                       data_inicio = "2020-04-15")

## Para fazer o gráfico em função do tempo com uma data de inicio 
## para as curvas
faz_grafico_letalidade(idade = TRUE, tempo = TRUE, titulo = "Meu título", 
                       data_inicio = "2020-04-15")

# Adicionando idade e sexo
faz_grafico_letalidade(idade = TRUE, sexo = TRUE, tempo = TRUE, 
                       titulo = "Meu título", data_inicio = "2020-04-15")
