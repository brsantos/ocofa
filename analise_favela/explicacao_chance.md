Como calcular a chance de morte pela Covid-19?
================
Bruno Santos

Considerando os dados disponibilizados pelo painel do Espírito Santo
[(https://coronavirus.es.gov.br/painel-covid-19-es)](https://coronavirus.es.gov.br/painel-covid-19-es)
e considerando uma listagem de bairros de Vitória classificados como
favela, segundo levantamento do OCOFA, podemos chegar nos seguintes
números

| favela | n\_casos | n\_mortes | letalidade |
| :----- | -------: | --------: | ---------: |
| Não    |     3290 |        94 |       2.86 |
| Sim    |     2911 |       132 |       4.53 |

Temos então que a letalidade da doença é igual a 4,53% em bairros
classificados como favela e 2,86% nos outros bairros. Dessa forma, para
uma pessoa infectada a chance dessa pessoa vir a falecer na favela é
igual

  
![\\mbox{Chance}\_F = \\frac{4,53\\%}{100,00\\% - 4,53\\%}
= 0,04745](https://latex.codecogs.com/png.latex?%5Cmbox%7BChance%7D_F%20%3D%20%5Cfrac%7B4%2C53%5C%25%7D%7B100%2C00%5C%25%20-%204%2C53%5C%25%7D%20%3D%200%2C04745
"\\mbox{Chance}_F = \\frac{4,53\\%}{100,00\\% - 4,53\\%} = 0,04745")  

Agora considerando uma pessoa infectada que não mora na favela, sua
chance de vir a falecer é igual a

  
![\\mbox{Chance}\_O = \\frac{2,86\\%}{100,00\\% - 2,86\\%}
= 0,02944](https://latex.codecogs.com/png.latex?%5Cmbox%7BChance%7D_O%20%3D%20%5Cfrac%7B2%2C86%5C%25%7D%7B100%2C00%5C%25%20-%202%2C86%5C%25%7D%20%3D%200%2C02944
"\\mbox{Chance}_O = \\frac{2,86\\%}{100,00\\% - 2,86\\%} = 0,02944")  
Logo, quando comparamos essas duas chances, obtemos

  
![\\frac{\\mbox{Chance}\_F}{\\mbox{Chance}\_O} = 1,6116 \\quad
\\Rightarrow \\quad \\mbox{Chance}\_F = 1,6116 \\times
\\mbox{Chance}\_O](https://latex.codecogs.com/png.latex?%5Cfrac%7B%5Cmbox%7BChance%7D_F%7D%7B%5Cmbox%7BChance%7D_O%7D%20%3D%201%2C6116%20%5Cquad%20%5CRightarrow%20%5Cquad%20%5Cmbox%7BChance%7D_F%20%3D%201%2C6116%20%5Ctimes%20%5Cmbox%7BChance%7D_O
"\\frac{\\mbox{Chance}_F}{\\mbox{Chance}_O} = 1,6116 \\quad \\Rightarrow \\quad \\mbox{Chance}_F = 1,6116 \\times \\mbox{Chance}_O")  
Logo, a chance de morrer das pessoas infectadas que moram na favela é
1,6 vezes a chance das pessoas que não moram em favelas em Vitória.
