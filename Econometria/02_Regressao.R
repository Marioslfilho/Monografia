# Primeira regressão
library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)

base_regressao= read.csv2('base_regressao.csv')

beta = c()
beta_min = c()
beta_max = c()
data_pandemia = as.Date('2020-04-01')

datas_corte = seq.Date(as.Date('2020-12-31'), as.Date('2023-12-31'), by = 'quarter')

pegando_betas = function(i, data = base_regressao){
  base_usada = data
  j = datas_corte[i]
  range_datas = seq.Date(data_pandemia, j, by = 'day')
  
  base_usada1 = base_usada %>% filter(!as.Date(DATA_EXIBICAO) %in% range_datas) %>% 
    mutate(Pandemia = ifelse(as.Date(DATA_EXIBICAO) >= data_pandemia, 1, 0))
  
  regressao = plm(base_usada1, formula = formula, index = c('id', 'DATA_EXIBICAO'), 
                  model = "pooling") 
  
  beta = append(beta, regressao$coefficients[2])
  beta_min = append(beta_min, t(confint(regressao))[1, 2])
  beta_max = append(beta_max, t(confint(regressao))[2, 2])
  data_betas = data.frame(beta_min, beta, beta_max, j)
}

final = map_df(seq(1, length(datas_corte)), ~pegando_betas(., data = base_regressao))

grafico = ggplot(final, aes(x = j, y = beta, ymin = beta_min, ymax = beta_max)) + 
  geom_point() + 
  geom_smooth(se = F) +
  ylim(-20, 2) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red', linewidth = 1) +
  geom_errorbar(width = 2) + 
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid = element_line(color = 'lightgray'),
        axis.line = element_line(color = 'black')) + xlab('')
grafico
ggsave('Betas.png')


