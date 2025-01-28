# Destrinchando as salas de cinema - Variável de Competição
library(dplyr)
library(tidygeocoder)
library(sf)
library(ggplot2)
library(rnaturalearth)
salas = read.csv2('salas.csv')

salas01 = salas %>% 
  select(REGISTRO_COMPLEXO, CEP_COMPLEXO) %>% 
  unique()

# Geocodificação 

resultados <- salas01 %>%
  mutate(CEP_COMPLEXO = paste0('Brasil, ', CEP_COMPLEXO)) %>% 
  geocode(address = CEP_COMPLEXO, method = 'osm') %>% 
  mutate(certo = ifelse(lat < 0 & long < 0, 1, 0))

resultados_SemNA = resultados %>% na.omit() %>% 
  filter(certo == 1)

df_sf <- st_as_sf(resultados_SemNA, coords = c("long", "lat"), crs = 4326)

# Abertura e Fechamento da Sala

salas_final = salas %>% 
  mutate_at(vars(contains('DATA')), ~ as.Date(as.character(.), format = "%d/%m/%Y")) %>% 
  mutate(DATA_ABERTURA = DATA_INICIO_FUNCIONAMENTO_SALA,
         DATA_FINAL = as.Date(ifelse(SITUACAO_SALA == 'EM FUNCIONAMENTO', 
                                     Sys.Date(), DATA_SITUACAO_SALA),
                              origin = "1970-01-01")) %>% 
  select(REGISTRO_COMPLEXO, DATA_ABERTURA, DATA_FINAL) %>% 
  group_by(REGISTRO_COMPLEXO) %>% 
  summarise(DATA_ABERTURA = min(DATA_ABERTURA), 
            DATA_FINAL = max(DATA_FINAL))

resultados_SemNA = resultados_SemNA %>% 
  inner_join(salas_final)

write.csv2(resultados_SemNA, 'resultado_liquido.csv', row.names = F)  
