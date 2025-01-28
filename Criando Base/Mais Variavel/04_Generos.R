library(dplyr)
library(readxl)
library(tidyr)
library(TMDb)
library(purrr)
api = "1327383d17cbd8e07d9ee2a597bcb8aa"

prot = v_04[sample(2962, 100),]
generos = c()

pegando_genero = function(i){
  genero = movie(api= api, id = i)$genres
  if (length(genero) == 0) {
    genero = NA
  }
  else{
    genero = genero[,'name'] %>% paste0(collapse = ',')
  }
  generos = data.frame(id = i, genero = genero)
  return(generos)
}

ids = v_04$id
generos_base = map_df(ids[c(seq(1, 1785), seq(1787, 2962))], pegando_genero)

generos_b = generos_base %>% 
  mutate(Drama = ifelse(grepl('Drama', genero), 1, 0),
         Comedia = ifelse(grepl('Comedy', genero), 1, 0),
         Familia = ifelse(grepl('Family', genero), 1, 0),
         Animacao = ifelse(grepl('Animation', genero), 1, 0))
# Drama, Comedia, Family, Animation
base_final_tmdb = v_04 %>% left_join(generos_b)

write.csv2(base_final_tmdb, 'base_final_tmdb.csv', row.names = F)
