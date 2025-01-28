# Tendências

library(dplyr)
library(ggplot2)
library(stargazer)
library(ggthemes)

# Introdução

## Vou criar aqui algumas tendências que podem ser 
## interessantes e caracterizar alguma coisa que ta faltando. O documento vai ser dividido em:

## Introdução
## Tratamento da base
## 1) Salas de Cinema aberta por ano
## 2) Tamanho médio das salas de cinema abertas por ano
## 3) Soma da capacidade total das salas de cinema abertas
## 4) Complexos de cinema abertos por ano
## 5) Salas de cinema por complexo aberto por ano - Não fiz
## 6) Capacidade total de cinema aberto por complexo por ano - Não fiz
## Conclusão

# Tratamento da base

intervalo_mensal = seq.Date(from = as.Date('1970-01-01'), to = Sys.Date(), by = "month")

complexos = salas %>% 
  select(REGISTRO_SALA, NOME_SALA, SITUACAO_SALA,
         ASSENTOS_SALA, DATA_SITUACAO_SALA, REGISTRO_COMPLEXO, NOME_COMPLEXO, 
         DATA_INICIO_FUNCIONAMENTO_SALA, DATA_SITUACAO_COMPLEXO,
         SITUACAO_COMPLEXO) %>% 
  mutate_at(vars(contains('DATA')), ~ as.Date(as.character(.), format = "%d/%m/%Y"))

## Descritiva pro texto

reserva_cultural = salas %>% filter(REGISTRO_COMPLEXO == 6665) %>% 
  select(NOME_SALA, NOME_COMPLEXO, contains('DATA'))

stargazer(reserva_cultural, summary = F)

# 1) Salas de Cinema aberta por ano

salas_anual = complexos %>% 
  mutate(DATA_ABERTURA = DATA_INICIO_FUNCIONAMENTO_SALA,
         DATA_FINAL = as.Date(ifelse(SITUACAO_SALA == 'EM FUNCIONAMENTO', 
                             Sys.Date(), DATA_SITUACAO_SALA),
                             origin = "1970-01-01"), 
         ASSENTOS = ASSENTOS_SALA, .keep = 'none')

quantidade_salas = c()

calculo_salas = function(data){
  salas_calculo = salas_anual %>% 
    filter(DATA_ABERTURA <= data & data <= DATA_FINAL)
  retornado = dim(salas_calculo)[1]
}

for (j in intervalo_mensal) {
  quantidade_salas = append(quantidade_salas, calculo_salas(j))
}

data.frame(intervalo_mensal, quantidade_salas) %>% 
  ggplot(aes(x = intervalo_mensal, quantidade_salas)) + 
  geom_line() + 
  theme_bw()

# 2) Tamanho médio das salas de cinema abertas por ano

calculo_tamanho_medio = function(data){
  salas_calculo = salas_anual %>% 
    filter(DATA_ABERTURA <= data & data <= DATA_FINAL)
  media = mean(salas_calculo$ASSENTOS)
  return(media)
}

quantidade_assentos_sala = c()

for (j in intervalo_mensal) {
  quantidade_assentos_sala = append(quantidade_assentos_sala, calculo_tamanho_medio(j))
}

data.frame(intervalo_mensal, quantidade_assentos_sala) %>% 
  ggplot(aes(x = intervalo_mensal, quantidade_assentos_sala)) + 
  geom_line() + 
  theme_bw()

# 3) Soma da capacidade total das salas de cinema abertas

calculo_soma_assentos = function(data){
  salas_calculo = salas_anual %>% 
    filter(DATA_ABERTURA <= data & data <= DATA_FINAL)
  soma = sum(salas_calculo$ASSENTOS)
  return(soma)
}

soma_assentos_sala = c()

for (j in intervalo_mensal) {
  soma_assentos_sala = append(soma_assentos_sala, calculo_soma_assentos(j))
}

data.frame(intervalo_mensal, soma_assentos_sala) %>% 
  ggplot(aes(x = intervalo_mensal, soma_assentos_sala)) + 
  geom_line() + 
  theme_bw()

# 4) Complexos de cinema abertos por ano

complexos_anual = complexos %>% 
  mutate(DATA_ABERTURA = DATA_INICIO_FUNCIONAMENTO_SALA,
         DATA_FINAL = as.Date(ifelse(SITUACAO_SALA == 'EM FUNCIONAMENTO', 
                                     Sys.Date(), DATA_SITUACAO_SALA),
                              origin = "1970-01-01"), 
         ASSENTOS = ASSENTOS_SALA, REGISTRO_COMPLEXO = REGISTRO_COMPLEXO,
         .keep = 'none') %>% 
  group_by(REGISTRO_COMPLEXO) %>% 
  summarise(DATA_ABERTURA = min(DATA_ABERTURA), DATA_FINAL = max(DATA_FINAL))

calculo_complexos = function(data){
  complexo_calculo = complexos_anual %>% 
    filter(DATA_ABERTURA <= data & data <= DATA_FINAL)
  quantidade_complexos = dim(complexo_calculo)[1]
  return(quantidade_complexos)
}

soma_complexos = c()

for (j in intervalo_mensal) {
  soma_complexos = append(soma_complexos, calculo_complexos(j))
}

data.frame(intervalo_mensal, soma_complexos) %>% 
  ggplot(aes(x = intervalo_mensal, soma_complexos)) + 
  geom_line() + 
  theme_bw()
