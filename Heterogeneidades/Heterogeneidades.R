# Tem que agora capturar heterogeneidade e arrumar a regressão
## Heterogeneidades: Capital e não capital, grande distribuídora, grande exibidora,
## País que o filme foi, Tamanho da sala (dividir em quartil)

# Pais do filme

filme_br = map_df(seq(1, length(datas_corte)), ~pegando_betas(., data = base_teste %>% 
                                                                filter(Brasileiro == 1))) %>% 
  mutate(Categoria = 'Brasileiros')

filme_us = map_df(seq(1, length(datas_corte)), ~pegando_betas(., data = base_teste %>% 
                                                                filter(Americano == 1))) %>% 
  mutate(Categoria = 'Americanos')

filme_outros = map_df(seq(1, length(datas_corte)), ~pegando_betas(., data = base_teste %>% 
                                                                    filter(Brasileiro == 0 & Americano == 0))) %>% 
  mutate(Categoria = 'Outra Nacionalidade')

filme_nacionalidade = rbind(filme_br, filme_us, filme_outros)

grafico_nacionalidade = ggplot(filme_nacionalidade, aes(x = j, y = beta, ymin = beta_min, ymax = beta_max,
                                                        group = Categoria, color = Categoria)) + 
  geom_point(size = 2) + geom_line() + 
  ylim(-20, 2) + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid = element_line(color = 'lightgray'),
        axis.line = element_line(color = 'black'),
        legend.position = 'bottom') +
  geom_errorbar(width = 10) + xlab('')
grafico_nacionalidade

# Tamanho da sala

tamanho_sala = base_teste %>% 
  mutate(a1 = ifelse(Tamanho_sala >= quantile(base_teste$Tamanho_sala)[2], 1, 0),
         a2 = ifelse(Tamanho_sala >= quantile(base_teste$Tamanho_sala)[3], 1, 0),
         a3 = ifelse(Tamanho_sala >= quantile(base_teste$Tamanho_sala)[4], 1, 0),
         quartil_tamanho = 1 + a1 + a2 + a3) %>% select(-c(a1, a2, a3))

tamanho_1 = map_df(seq(1, length(datas_corte)), ~pegando_betas(., data = tamanho_sala %>% 
                                                                 filter(quartil_tamanho == 1))) %>% 
  mutate(Categoria = 1)

tamanho_2 = map_df(seq(1, length(datas_corte)), ~pegando_betas(., data = tamanho_sala %>% 
                                                                 filter(quartil_tamanho == 2))) %>% 
  mutate(Categoria = 2)

tamanho_3 = map_df(seq(1, length(datas_corte)), ~pegando_betas(., data = tamanho_sala %>% 
                                                                 filter(quartil_tamanho == 3))) %>% 
  mutate(Categoria = 3)

tamanho_4 = map_df(seq(1, length(datas_corte)), ~pegando_betas(., data = tamanho_sala %>% 
                                                                 filter(quartil_tamanho == 4))) %>% 
  mutate(Categoria = 4)

tamanho_salas = rbind(tamanho_1, tamanho_2, tamanho_3, tamanho_4)

grafico_tamanhos = ggplot(tamanho_salas, aes(x = j, y = beta, ymin = beta_min, ymax = beta_max,
                                             group = as.character(Categoria),
                                             color = as.character(Categoria))) + 
  geom_point(size = 2) + geom_line() +
  scale_color_discrete(name = 'Quartil') +
  ylim(-20, 2) +
  geom_errorbar(width = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid = element_line(color = 'lightgray'),
        axis.line = element_line(color = 'black'),
        legend.position = 'bottom') + xlab('')
grafico_tamanhos

# Por capitai

capitais= c('Rio Branco','Maceió','Macapá','Manaus','Salvador','Fortaleza','Vitória','Goiânia','São Luís','Cuiabá','Campo Grande','Belo Horizonte','Belém','João Pessoa','Curitiba','Recife','Teresina','Rio de Janeiro','Natal','Porto Alegre','Porto Velho','Boa Vista','Florianópolis','São Paulo','Aracaju','Palmas','Brasília')


base_teste1 = base_teste %>% 
  mutate(Capital = ifelse(MUNICIPIO_SALA_COMPLEXO %in% toupper(capitais), 1, 0))

capital = base_teste1 %>% filter(Capital == 1)
cidade = base_teste1 %>% filter(Capital == 0)

capital_betas = map_df(seq(1, length(datas_corte)), ~pegando_betas(., data = capital))
capital_betas = capital_betas %>% mutate(Categoria = 'Capital')

cidades_betas = map_df(seq(1, length(datas_corte)), ~pegando_betas(., data = cidade))
cidades_betas = cidades_betas %>% mutate(Categoria = 'Cidade')

por_capital = rbind(capital_betas, cidades_betas)

grafico_capital = ggplot(por_capital, aes(x = j, y = beta, ymin = beta_min, ymax = beta_max,
                                          group = as.character(Categoria),
                                          color = as.character(Categoria))) + 
  geom_point(size = 2) + geom_line() +
  scale_color_discrete(name = '') +
  ylim(-20, 2) +
  geom_errorbar(width = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid = element_line(color = 'lightgray'),
        axis.line = element_line(color = 'black'),
        legend.position = 'bottom') + xlab('')
grafico_capital

# Fazendo as duas regressões.

teste = head(base_teste, 100)

data_pandemia = '2020-04-01'

data_final= '2023-01-01'

base_pandemia = base_teste %>% filter(Data <= as.Date(data_final))

base_pos = base_teste %>% filter(!(as.Date(Data) %in% seq.Date(as.Date(data_pandemia), as.Date(data_final), by = 'day')))

base_pandemia = base_pandemia %>% 
  mutate(Pandemia = ifelse(as.Date(Data) >= as.Date(data_pandemia), 1, 0))

base_pos = base_pos %>% 
  mutate(Pandemia = ifelse(as.Date(Data) >= as.Date(data_final), 1, 0))

regressao1 = lm(base_pandemia, formula = Presenca_sala ~ Pandemia + Tamanho_sala + 
                  Dia_pos_lancamento + Brasileiro + Americano + duracao_filme + 
                  delay_lancamentoBR + factor(month(as.Date(Data))))

regressao2 = lm(base_pos, formula = Presenca_sala ~ Pandemia + Tamanho_sala + 
                  Dia_pos_lancamento + Brasileiro + Americano + duracao_filme + 
                  delay_lancamentoBR + factor(month(as.Date(Data))))
library(stargazer)
stargazer(regressao1, regressao2)
