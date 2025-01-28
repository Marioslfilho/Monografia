library(dplyr)
library(readxl)
library(TMDb)
api = "1327383d17cbd8e07d9ee2a597bcb8aa"
v_03 = read.csv2('Mais Variavel/03_V.csv')
Oscar = read.csv2("C:/Users/T-Gamer/Desktop/Oscar.csv") %>% 
  select(id, Indicacao, Premiacao)

v_04 = v_03 %>% left_join(Oscar)
write.csv2(v_04, '04_V.csv', row.names = F)
