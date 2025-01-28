# TMDB 
library(lubridate)
library(dplyr)
library(TMDb)
library(purrr)
api = "1327383d17cbd8e07d9ee2a597bcb8aa"

nome_filmes_TMDB_averiguado = read.csv2("output/filmes_TMDB_averiguado.csv") %>% 
  select(-X, -Director, -Gênero, -simil)

nome_filmes_TMDB_averiguado = nome_filmes_TMDB_averiguado[-c(2210, 1653),]

#tconst, releaseORIG, releaseBRA, genero, diretor, runtime
variaveis_TMDB = function(i){
  filme_procurado_id = nome_filmes_TMDB_averiguado$id[i]
  filme_procurado_releaseORIG = movie(api = api, id = filme_procurado_id)$release_date[1]
  filme_procurado_duracao = movie(api = api, id = filme_procurado_id)$runtime
  vetor_final = c(id = filme_procurado_id, 
                  releaseORIG = filme_procurado_releaseORIG, #Lançamento no primeiro país
                  duracao_filme = filme_procurado_duracao # Duração do filme em minutos
                  )
  return(vetor_final)
}

j_round1 = seq(1, 2964)
filmes_TMDB = map_df(j_round1, variaveis_TMDB)

filmes_TMDB1 = filmes_TMDB %>% mutate(id = as.numeric(id)) %>% 
  inner_join(nome_filmes_TMDB_averiguado, by = 'id') %>% 
  mutate(releaseORIG = as.Date(releaseORIG), Data = as.Date(Data),
         delay_lancamentoBR = (releaseORIG %--% Data)/ddays(1), .keep = 'unused') %>% 
  rename(releaseBRA = Data)

filmes_TMDB1 = filmes_TMDB1[-c(seq(1653, 1655), 1913, 2213, 2214),]

write.csv2(filmes_TMDB1, "Mais Variavel/01_V.csv", row.names = F)


