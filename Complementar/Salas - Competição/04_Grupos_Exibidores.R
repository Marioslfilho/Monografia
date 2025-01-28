# Tendência das Exibidoras 

library(dplyr)
library(purrr)
library(ggrepel)
library(ggplot2)

exibidores = read.csv2('salas.csv') %>% 
  select(SITUACAO_SALA, NOME_GRUPO_EXIBIDOR, 
         ASSENTOS_SALA, DATA_SITUACAO_SALA, DATA_INICIO_FUNCIONAMENTO_SALA) %>% 
  mutate_at(vars(contains('DATA')), ~ as.Date(as.character(.), format = "%d/%m/%Y")) %>% 
  mutate(DATA_ABERTURA = DATA_INICIO_FUNCIONAMENTO_SALA,
         DATA_FINAL = as.Date(ifelse(SITUACAO_SALA == 'EM FUNCIONAMENTO', 
                                     Sys.Date(), DATA_SITUACAO_SALA),
                              origin = '1970-01-01'), 
         ASSENTOS = ASSENTOS_SALA, NOME_GRUPO_EXIBIDOR = NOME_GRUPO_EXIBIDOR, 
         .keep = 'none')

grupos_exibicao = unique(exibidores$NOME_GRUPO_EXIBIDOR)
intervalo_mensal = seq.Date(from = as.Date('1970-01-01'), to = Sys.Date(), by = "year")

# Introdução

## A ideia aqui é ver o 'market share' de grandes grupos exibidores, esse market share seria
## percentual de assentos do total ou até percentual de salas do total. Assim o documento seguiria:

## 1) Dados
## 2) Gráfico

# 1) Dados

exemplo = 'CINEMARK'
data = as.Date('2010-01-01')

assentos_grupo_exibidor = function(grupo_exibidor, data){
  df = exibidores %>% 
    filter(NOME_GRUPO_EXIBIDOR == grupo_exibidor) %>% 
    filter(DATA_ABERTURA <= data & data <= DATA_FINAL)
  n_assentos = sum(df$ASSENTOS)
  n_salas = dim(df)[1]
  base = data.frame(grupo_exibidor, n_assentos, n_salas)
  return(base)
}

por_data = function(datas){
  base = map_df(grupos_exibicao, ~assentos_grupo_exibidor(.x, data = datas))
  df = base %>% mutate(share_assentos = n_assentos*100/sum(n_assentos),
                       share_salas = n_salas*100/sum(n_salas), 
                       data = datas, .keep = 'unused')
  return(df)
}

base_final = map_df(intervalo_mensal, por_data)

meu_top5 = c('NÃO PERTENCE A NENHUM GRUPO EXIBIDOR', 'CINEMARK', 'UCI', 'KINOPLEX')

base_tratada = base_final %>% 
  mutate(grupo_exibidor= ifelse(grupo_exibidor %in% meu_top5, grupo_exibidor, 'Outros'),
         grupo_exibidor = ifelse(grupo_exibidor == meu_top5[1], 'Sem Grupo', grupo_exibidor)) %>% 
  group_by(grupo_exibidor, data) %>% 
  summarise(share_assentos = sum(share_assentos),
            share_salas = sum(share_salas))

ggplot(base_tratada, 
       aes(x = data, y = share_salas, group = grupo_exibidor, color = grupo_exibidor)) + 
  geom_line(linewidth = 1) + 
  coord_cartesian(xlim = c(min(base_tratada$data), max(base_tratada$data) + 3000)) +
  geom_text_repel(data = subset(base_tratada, data == max(data)), 
            aes(label = grupo_exibidor), hjust = -0.2) + 
  theme_bw() +
  theme(legend.position = 'none')

ggplot(base_tratada, 
       aes(x = data, y = share_assentos, group = grupo_exibidor, color = grupo_exibidor)) + 
  geom_line(linewidth = 1) + 
  coord_cartesian(xlim = c(min(base_tratada$data), max(base_tratada$data) + 3000)) +
  geom_text_repel(data = subset(base_tratada, data == max(data)), 
                  aes(label = grupo_exibidor), hjust = -0.2) + 
  theme_bw() +
  theme(legend.position = 'none')
  
