library(sf)
library(raster)
library(geobr)
library(tidyverse)
library(slopes)
library(tmap)


# importar os dados da rede viaria extraida do osm da regiao centro-oeste

co_osm <- st_read("C:/Users/Juciano Rodrigues/Documents/declive_roads/co_osm.shp")


# importar o arquivo raster da altitude de onde esta localizado goiania

dem_gyn <- raster("C:/Users/Juciano Rodrigues/Documents/declive_roads/dados_raster/16S495ZN.tif")

projection(dem_gyn) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" # atribui crs ao raster
crs(dem_gyn)

plot(dem_gyn)

# importa limites do municipio do rio de janeiro

goiania <- read_municipality(
  code_muni = 5208707,
  year = 2010)

goiania <- st_transform(goiania, crs = 4326) # atribui o mesmo crs do raster

plot(goiania$geom)


# recorta apenas a rede viaria de goiania
goiania_osm <- st_intersection(co_osm, goiania)

plot(altitude_gyn)

# visualiza para simples conferencia
raster::plot(dem_gyn)
plot(sf::st_geometry(goiania), add = TRUE)


# recorta a imagem raster nos limites de Goiania

altitude_gyn <- raster::crop(dem_gyn, goiania)

summary(values(altitude_gyn)) # gera estatisticas resumidas do raster

res(altitude_gyn) # verifica a resolucao dos pixel do raster

# visualiza para simples conferencia
raster::plot(altitude_gyn)
plot(sf::st_geometry(goiania), add = TRUE)


# filtra as vias de goiania
goiania_osm_filter <- goiania_osm %>% 
  dplyr::filter(highway %in% c('primary', "primary_link", 'secondary',"secondary_link", 'tertiary', "tertiary_link",
                               "trunk", "trunk_link", "residential", "cycleway", "living_street", "unclassified",
                               "motorway", "motorway_link", "pedestrian", "steps", "service", "track"))


# verifica as vias desconectadas e seleciona apenas 
goiania_osm_filter$group = rnet_group(goiania_osm_filter)
table(goiania_osm_filter$group)

# teste
t <- goiania_osm_filter %>% 
  mutate(group = as.factor(group))

plot(t["group"])

# filtra apenas a rede viaria principal
rede_viaria_gyn <- goiania_osm_filter %>% 
  filter(group == "1")

# visualiza para simples conferencia
raster::plot(altitude_gyn)
plot(sf::st_geometry(goiania), add = TRUE)
plot(sf::st_geometry(rede_viaria_gyn), add = TRUE)

# verifica compatilizacao dos crs
crs(altitude_gyn)
crs(rede_viaria_gyn)


# calcular a declividade

rede_viaria_gyn = rede_viaria_gyn %>% 
  st_cast("LINESTRING")

# quebra a rede em varios segmentos para aumentar a precisao do calculo
nrow(rede_viaria_gyn)
rede_viaria_gyn = stplanr::rnet_breakup_vertices(rede_viaria_gyn)
nrow(rede_viaria_gyn)

# calcula a declividade
rede_viaria_gyn$slope = slope_raster(rede_viaria_gyn, e = altitude_gyn) #28 segundos

rede_viaria_gyn$declive = rede_viaria_gyn$slope*100
summary(rede_viaria_gyn$declive)

rede_viaria_gyn$declive_class =  rede_viaria_gyn$declive %>%
  cut(
    breaks = c(0, 3, 5, 8, 10, 20, Inf),
    labels = c("0-3: plano", "3-5: leve","5-8: médio", "8-10: exigente", "10-20: terrível", ">20: impossível"),
    right = F
  )
round(prop.table(table(rede_viaria_gyn$declive_class))*100,1)



# Criar uma palete de cores, entre o verde escuro e o vermelho escuro

palredgreen = c("#267300", "#70A800", "#FFAA00", "#E60000", "#A80000", "#730000")

tmap_mode("view")
tm_shape(rede_viaria_gyn) +
  tm_lines(
    col = "declive_class",
    palette = palredgreen, #palete de cores
    lwd = 1, #espessura das linhas
    title.col = "Declive [%]") +
  tm_shape(goiania) +
  tm_borders()

table(rede_viaria_gyn$highway)

