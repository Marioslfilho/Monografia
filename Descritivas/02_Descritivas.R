# Generos

setwd("C:/Users/T-Gamer/Desktop/TCC/")
library(dplyr)
library(stargazer)
library(ggplot2)
library(gghighlight)
library(tidyr)
base = read.csv2('Criando Base/Base_Final.csv')

genero_base = base %>% 
  select(genero, PUBLICO, DATA_EXIBICAO) %>% 
  separate_rows(genero, sep = ',') %>% 
  mutate(pandemia = ifelse(DATA_EXIBICAO > '2020-04-01', 1, 0)) %>% 
  group_by(genero, pandemia) %>% 
  summarise(n = n(), 
            media = mean(as.numeric(PUBLICO), na.rm = T)) %>% 
  group_by(pandemia) %>% 
  mutate(freq = n/sum(n),
         top4 = freq %in% head(sort(freq, decreasing = T), 4))

ggplot(genero_base
       ,aes(y = media, x = freq*100, color = pandemia)) + 
  geom_point(size = 4, aes(alpha = top4)) + 
  scale_alpha_manual(values = c(0.2, 1)) +
  theme(legend.position = 'none',
        panel.background = element_blank(),
        panel.grid = element_line(color = 'lightgray'),
        axis.ticks = element_blank()) +
  labs(x = '', y = '') +
  geom_text(aes(label = ifelse(top4, genero, '')), vjust = -1) +
  geom_smooth(aes(group = pandemia, color = pandemia), se = F)

ggsave('Generos.png')
