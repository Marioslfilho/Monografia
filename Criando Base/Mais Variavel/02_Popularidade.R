# Popularidade
library(dplyr)
library(readxl)
library(TMDb)
api = "1327383d17cbd8e07d9ee2a597bcb8aa"
v_02 = read.csv2('02_V.csv')

media_cast = function(id){
  a = mean(movie_credits(api_key = api, id = id)$cast$popularity[seq(1, 5)])
  return(a)
}

media_dir = function(id){
  teste = movie_credits(api_key = api, id = id)$crew %>% 
    filter(job == 'Director') %>% 
    select(popularity)
  teste = mean(teste$popularity)
  return(teste)
}

pop_dires = pop_cast = c()


for (i in seq(1787, 2962)){
  j = v_02$id[i]
  pop_dires = append(pop_dires, media_dir(j))
  pop_cast = append(pop_cast, media_cast(j))
}

add = data.frame(pop_dir = pop_dires, pop_cast = pop_cast, id = v_02$id)

v_03 = v_02 %>% inner_join(add)
write.csv2(v_03, '03_V.csv', row.names = F)
