# Criando a base da regressão
library(dplyr)
library(ggforce)
library(geobr)
library(tidygeocoder)
library(sf)
library(cepR)

devtools::install_github("rfsaldanha/cepR")

base_regressao = read.csv2('C:/Users/T-Gamer/Desktop/TCC/Criando Base/Base_Final.csv') %>% 
  mutate(Pandemia = ifelse(as.Date(DATA_EXIBICAO) >= as.Date('2020-04-01'), 1, 0))

base_regressao = base_regressao %>% 
  mutate(Indicado = ifelse(is.na(Indicacao), 0, 1),
         Premiado = ifelse(is.na(Premiacao), 0, 1),
         P_indicacao = ifelse(is.na(P_indicacao), 0, P_indicacao),
         P_premiacao = ifelse(is.na(P_premiacao), 0, P_premiacao))

# Adicionar competitividade correta

cepR::busca_multi(token = token)

cinemas = base_regressao %>% 
  select(REGISTRO_COMPLEXO, CEP_COMPLEXO) %>% 
  unique() %>%
  geocode(CEP_COMPLEXO, method = 'osm', lat = latitude, long = longitude)

teste = cinemas %>% na.omit() %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

a = data.frame(cep = '56903-525, Brasil') %>% 
  geocode(cep, method = 'osm', lat = latitude, long = longitude)

busca_multi(token = token)

write.csv2(base_regressao, 'base_regressao.csv', row.names = F)
