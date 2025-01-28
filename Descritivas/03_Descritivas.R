# Oscar Premiações

setwd("C:/Users/T-Gamer/Desktop/TCC/")
library(dplyr)
library(stargazer)
library(tidyr)
library(ggplot2)
library(lubridate)
base = read.csv2('Criando Base/Base_Final.csv')
oscar_date = read.csv2('Complementar/oscar_dates.csv', sep = ',', encoding = 'latin1')

oscar_date = oscar_date %>% 
  mutate(Data_indicados = as.Date(B, format = "%d de %B de %Y"),
         Data_premiacao = as.Date(C, format = "%d de %B de %Y"),
         Ano = A, .keep = 'unused')



ano_indicacao = base %>% 
  mutate(Ano = year(releaseORIG) + 1) %>%
  inner_join(oscar_date)

teste = indicados %>% 
  filter(Indicado == 0)

indicados = ano_indicacao %>% 
  filter(Final_de_semana == 1) %>% 
  mutate(Distancia = (Indicacao %--% DATA_EXIBICAO/ddays(1)),
         Distancia = ifelse(is.na(Distancia),
                            (Data_indicados %--% DATA_EXIBICAO)/ddays(1), 
                            Distancia),
         Indicado = ifelse(is.na(P_indicacao), 0, 1)) %>% 
  group_by(Distancia, Indicado = as.character(Indicado)) %>% 
  summarise(publico = log(mean(PUBLICO, na.rm = T))) %>% 
  filter(Distancia <= 100, Distancia >= -100) %>% 
  mutate(Antes = ifelse(Distancia < 0, 1, 0))

premiados = base %>% 
  filter(!is.na(Premiacao), Final_de_semana == 1) %>% 
  mutate(Distancia = (Premiacao %--% DATA_EXIBICAO/ddays(1))) %>% 
  group_by(Distancia, P_premiacao = as.character(P_premiacao)) %>% 
  summarise(publico = log(mean(PUBLICO, na.rm = T))) %>% 
  filter(Distancia <= 30, Distancia >= -30)

ggplot(indicados, aes(x = Distancia, y = publico, color = Indicado, 
                      group = interaction(Indicado, Antes))) + 
  theme(panel.background = element_blank(),
        panel.grid = element_line(color = alpha('lightgray', 0.6)),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        axis.title = element_text(color = '#8F8F8F')) +
  labs(x = 'Days before nomination', y = 'Public') + 
  geom_line(alpha = 0.3, linewidth = 0.8) +
  geom_smooth(data = indicados %>% 
                filter(Distancia < -10 | Distancia > 0),se = F) +
  scale_color_manual(values = c('1' = '#F4A261', '0' = 'lightblue'),
                     labels = c('1' = 'Sim', '0' = 'Não')) + 
  geom_vline(xintercept = 0, color = "#8F8F8F") + 
  annotate(y = 5, x = 0, 'text', label = 'Pre Nomination', color = "#8F8F8F",
           hjust = 1.1) + 
  annotate(y = 5, x = 0, 'text', label = 'Post Nomination', color = "#8F8F8F",
           hjust = -0.1)

ggsave('Descritivas/Nomination.png')

ggplot(premiados, aes(x = Distancia, y = publico, color = P_premiacao)) + 
  theme(panel.background = element_blank(),
        panel.grid = element_line(color = alpha('lightgray', 0.6)),
        axis.ticks = element_blank(),
        legend.position = 'none') +
  labs(x = '', y = '') + 
  geom_line(alpha = 0.3, linewidth = 0.8) +
  geom_smooth(data = premiados %>% filter(Distancia < -10 & Distancia > 0),se = F) +
  geom_smooth(data = subset(premiados, P_premiacao == '0'), color = 'lightblue',
              se = F, fullrange = T, linetype = 'dashed') + 
  scale_color_manual(values = c('1' = '#F4A261', '0' = 'lightblue')) + 
  geom_vline(xintercept = 0, color = "#8F8F8F") + 
  annotate(y = max(premiados$publico), x = 0, 'text', label = 'Pre-Oscar Win', color = "#8F8F8F",
           hjust = 1.1) + 
  annotate(y = max(premiados$publico), x = 0, 'text', label = 'Post-Oscar Win', color = "#8F8F8F",
           hjust = -0.1)
ggsave('Descritivas/Oscar-Win.png')
