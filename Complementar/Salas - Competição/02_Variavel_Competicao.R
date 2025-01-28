# Criando a variável competição

library(lubridate)
library(purrr)

base = read.csv2('resultado_liquido.csv') %>% 
  mutate_at(vars(contains('DATA')), as.Date) 

conta_conc_por_data = function(j, cinema){
  base_01 = base %>% 
    filter(floor_date(DATA_ABERTURA, 'month') <= j & ceiling_date(DATA_FINAL, 'month') >= j)
  
  latitude_cinema = base[base$REGISTRO_COMPLEXO == cinema,]$lat
  longitude_cinema = base[base$REGISTRO_COMPLEXO == cinema,]$lon
  
  distancia = sqrt((base_01$lat - latitude_cinema)**2 + (base_01$lon - longitude_cinema)**2)
  competitividade = sum(ifelse(distancia <= 0.009*3, 1, 0)) - 1
  return(competitividade)}

conta_conc_por_cinema = function(cinema){
  teste = base[base$REGISTRO_COMPLEXO == cinema,]
  vetor_existencia = seq.Date(from = floor_date(as.Date(teste$DATA_ABERTURA), 'month'),
                              to = ceiling_date(as.Date(teste$DATA_FINAL), 'month'),
                              by = 'month')
  concorrencias_e_datas = map(vetor_existencia, ~conta_conc_por_data(.x, cinema = cinema)) %>% 
    unlist()
  data = data.frame(REGISTRO_COMPLEXO = cinema, data = vetor_existencia, 
                    concorrencia = concorrencias_e_datas)
  data <- data[1:(nrow(data) - 1), ]
  return(data)
}

base_sem_na = base %>% na.omit()

cinemas = base_sem_na$REGISTRO_COMPLEXO
cinemas = cinemas[1011]

base_concorrencias = map_df(cinemas, conta_conc_por_cinema) 

write.csv2(base_concorrencias, 'base_concorrencia.csv', row.names = F)
