library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(plm)

base_regressao= read.csv2('base_regressao.csv')
corte_pandemico = "2023-01-01"

base_1 = base_regressao %>% 
    filter(DATA_EXIBICAO < as.Date(corte_pandemico))
base_2 = base_regressao %>% 
    filter(DATA_EXIBICAO >= as.Date(corte_pandemico) |
             DATA_EXIBICAO <= as.Date('2020-04-01')) 

formula = as.formula(PUBLICO ~ Pandemia + ASSENTOS_SALA + 
                       Dia_pos_lancamento + Brasileiro + Americano + duracao_filme + 
                       Distribuidora + pop_dir + pop_cast + Drama + Comedia + 
                       Familia + Animacao + P_indicacao + P_premiacao + compet +
                       delay_lancamentoBR + factor(month(as.Date(DATA_EXIBICAO))))

regressao_1 = lm(base_1, formula = formula)

regressao_2 = lm(base_2, formula = formula)

# Regressão em painel

modelo_painel <- plm(formula = formula, data = base_1, index = c('id', 'DATA_EXIBICAO'), 
                     model = "pooling")

summary(modelo_painel, vcov = vcovHC(modelo_painel, type = "HC0", 
                                     cluster = 'group', cluster.by = base_1$MUNICIPIO_SALA_COMPLEXO))


modelo_painel2 <- plm(formula = formula, data = base_2, index = c('id', 'DATA_EXIBICAO'), 
                     model = "pooling")
summary(modelo_painel2, vcov = vcovHC(modelo_painel2, type = "HC0", 
                                     cluster = 'group', cluster.by = base_2$MUNICIPIO_SALA_COMPLEXO))
