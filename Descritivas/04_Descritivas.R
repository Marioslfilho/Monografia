# GeoCoding
library(tidygeocoder)
library(sf)
library(dplyr)
library(geobr)
library(ggplot2)
library(ggforce)

setores_sp <- read_census_tract(code_tract = 3550308, year = 2010)
areas_ponderacao_sp <- read_weighting_area(code_weighting = 3550308, year = 2010)
salas = read.csv2('Criando Base/input/salas.csv')

salas = salas %>% select(contains('COMPLEXO')) %>% 
  filter(MUNICIPIO_COMPLEXO == 'SÃO PAULO') %>%
  select(REGISTRO_COMPLEXO, CEP_COMPLEXO) %>% unique()

# Use o serviço de geocoding OpenStreetMap
data_coords <- salas %>%
  geocode(CEP_COMPLEXO, method = 'osm', lat = latitude, long = longitude) %>% 
  na.omit()

# Passo 2: Converter o data frame de coordenadas para um objeto `sf`
data_coords_sf <- st_as_sf(data_coords, coords = c("longitude", "latitude"), crs = st_crs(setores_sp))

# Passo 3: Realizar a interseção espacial
# Isso irá adicionar os dados do setor censitário correspondente às suas coordenadas
cep_in_sector <- st_join(data_coords_sf, areas_ponderacao_sp, join = st_intersects) %>% 
  filter(!is.na(name_muni)) %>% 
  group_by(code_weighting) %>% 
  summarise(n = n()) %>% 
  select(-code_weighting) 

cep_in_sector2 <- st_join(data_coords_sf, setores_sp, join = st_intersects) %>% 
  filter(!is.na(name_muni)) %>% 
  group_by(code_tract) %>% 
  summarise(n = n()) %>% 
  select(-code_tract) 

# Base para gráficos

area = areas_ponderacao_sp %>% st_join(cep_in_sector) %>% 
  mutate(n = ifelse(is.na(n), 0, n))

area2 = setores_sp %>% st_join(cep_in_sector2) %>% 
  mutate(n = ifelse(is.na(n), 0, n))

# Gráficos

ggplot() +
  geom_sf(aes(fill = n), color = NA, data = area) +
  theme(panel.background = element_rect(fill = 'beige', color = 'black'),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()) +
  scale_fill_gradient2(low = "white", mid = 'lightblue',high = "darkred",
                       midpoint = 3, name = 'Quantity')
ggsave('Descritivas/mapa_sp.png')

ggplot() +
  geom_sf(data = area2, fill = NA, color = "grey") +  # Mapa de fundo com contornos
  geom_sf(data = st_centroid(area2 %>% 
                               filter(n > 0)), aes(size = n), color = "blue", alpha = 0.7) + # Adiciona pontos para distritos filtrados
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = NA, color = 'black'),
        panel.grid = element_blank()) +
  scale_size_continuous(name = "Cinema Count")
ggsave('sp_geral.png')

grafico = ggplot() +
  geom_sf(data = area2, fill = NA, color = "grey") +  # Mapa de fundo com contornos
  geom_sf(data = st_centroid(area2 %>% filter(n > 0)), 
          aes(size = n), color = "blue", alpha = 0.7) + # Adiciona pontos para distritos filtrados
  xlim(c(-46.7500, -46.6)) +
  ylim(c(-23.65, -23.5)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = NA, color = 'black'),
        panel.grid = element_blank()) +
  scale_size_continuous(name = "Cinema Count")
grafico
ggsave('sp_especifico.png')

for (i in c(1, 3, 5)) {
  grafico +
    geom_circle(aes(y0 = -23.56498, x0 = -46.65179, r = 0.009*i), linetype = 'dashed')
  ggsave(paste0('sp_circulo',i,'.png'))
}

cplxs = salas$REGISTRO_COMPLEXO

coords = data_coords %>% 
  filter(REGISTRO_COMPLEXO %in% cplxs)

i= 133

competicao = function(i, dist_max = 5){
  dist_coord = 0.009*dist_max
  
  complexo = coords[i,]
  lat = complexo$latitude
  long = complexo$longitude
  coords_1 = coords %>% 
    mutate(dist = sqrt((latitude-lat)**2 + (longitude-long)**2),
           km_5 = ifelse(dist < dist_coord, 1, 0))
  soma = sum(coords_1$km_5) - 1
  return(soma)
}