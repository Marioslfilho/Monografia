# Popularidade

setwd("C:/Users/T-Gamer/Desktop/TCC/")
library(dplyr)
library(stringr)
library(stargazer)
base = read.csv2('Criando Base/Base_Final.csv')

popularidade_diretor = base %>% 
  select(diretor, pop_dir) %>% 
  unique() %>% 
  arrange(-pop_dir) %>% 
  slice_head(n = 10) %>% 
  rename(Director = diretor, Popularity = pop_dir) %>% 
  stargazer(summary = F)

popularidade_cast = base %>% 
  select(nomeBRA, pop_cast) %>% 
  unique() %>% 
  arrange(-pop_cast) %>% 
  slice_head(n = 10) %>% 
  mutate(nomeBRA = str_to_title(nomeBRA)) %>% 
  rename(Movie = nomeBRA, Popularity = pop_cast) %>% 
  stargazer(summary = F)
