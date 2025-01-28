# Dados COVID
library(readxl)

covid = read_excel('dados_isolXinc.xlsx')
unique(covid$city)

# tem 33 cidades