# Distribuidora e Produtora
library(dplyr)
library(readxl)

v_01 = read.csv2('01_V.csv')
teste = read_excel("C:/Users/T-Gamer/Desktop/TCC/Criando Base/input/todas_producoes2.xlsx",
                   skip = 1) %>% 
  mutate(Renda = as.numeric(`Renda (R$) no ano de exibição`))

distrib = teste %>% group_by(`Empresa distribuidora`) %>% summarise(n = n(),
                renda = sum(as.numeric(`Renda (R$) no ano de exibição`))/10**9) %>% 
  slice_max(n = 6, order_by = renda) %>% select(1) %>% mutate(Distribuidora = 1)

teste1 = teste %>% left_join(distrib) %>% 
  mutate(Distribuidora = ifelse(is.na(Distribuidora), 0, 1)) %>% 
  select(`CPB/ROE`, Distribuidora)

teste2 = inner_join(v_01, teste1, by = c('ROE' = 'CPB/ROE')) %>% 
  unique() %>% 
  group_by(ROE) %>% 
  mutate(Distribuidora = sum(Distribuidora)) %>% 
  unique()

write.csv2(teste2, '02_V.csv', row.names = F)
