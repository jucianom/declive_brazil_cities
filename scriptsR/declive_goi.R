library(sf)
library(raster)
library(geobr)
library(tidyverse)
library(slopes)
library(tmap)


# obtendo os dados das vias da cidade do Rio

# download dos dados das vias no open street maps - o conjunto de dados disponível é de toda Regiao Sudeste do Brasil

co_osm <- st_read("C:/Users/Juciano Rodrigues/Documents/declive_roads/co_osm.shp")


# salvando os dados do Sudeste no diretorio de trabalho

st_write(co_osm, "C:/Users/Juciano Rodrigues/Documents/declive_roads/co_osm.shp")



# importar os dados das vias da Regiao Sudeste

co_osm <- st_read("C:/Users/Juciano Rodrigues/Documents/declive_roads/co_osm.shp")

crs(co_osm) # verificar o crs


# importa limites do municipio de Goiania atraves do pacote geobr

goi_muni <- read_municipality(
  code_muni = 5208707,
  year = 2010)

goi_muni <-st_transform(goi_muni, crs = 4326) # transforma o crs


# recorta apenas os dados das vias do municipio de Goiania
goiania_osm <- st_intersection(co_osm, goi_muni)

plot(goiania_osm$geometry) # plotar para simples conferencia


# especiona a variavel highways
highways <- goiania_osm %>% 
  group_by(highway) %>% 
  count() %>% 
  st_drop_geometry()

# filtra apenas as vias de interesse
goiania_osm_filter <- goiania_osm %>% 
  dplyr::filter(highway %in% c('primary', "primary_link", 'secondary',"secondary_link", 'tertiary', "tertiary_link",
                               "trunk", "trunk_link", "residential", "cycleway", "living_street", "unclassified",
                               "motorway", "motorway_link", "pedestrian", "steps", "service", "track"))

plot(goiania_osm_filter$geometry) # plota o mapa para simples conferencia


# verifica as vias desconectadas e seleciona apenas 
goiania_osm_filter$group = rnet_group(goiania_osm_filter) # Obs.: abrir o arquivo rnet_group_functions.R
table(goiania_osm_filter$group)


# filtra apenas a rede viaria principal
rede_viaria_gyn <- goiania_osm_filter %>% 
  filter(group == "1")



# salvando os dados de Goinia no diretorio de trabalho

st_write(goiania_osm_filter, "C:/Users/Juciano Rodrigues/Documents/declive_roads/goiania_osm_filter.shp")



# # importa os dados raster

dem_gyn <- raster("C:/Users/Juciano Rodrigues/Documents/declive_roads/dados_raster/16S495ZN.tif")


# atribui sistema de referencia de coordenadas ao objeto raster

projection(dem_gyn) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" # atribui crs ao raster
crs(dem_gyn)

plot(dem_gyn)


# visualiza para simples conferencia
raster::plot(dem_gyn)
plot(sf::st_geometry(goi_muni), add = TRUE)

# recorta a parte da imagem que cobre apenas o municipio do Rio
altitude_gyn <- raster::crop(dem_gyn, goi_muni)

summary(values(altitude_gyn)) # gera estatisticas resumidas do raster

res(altitude_gyn) # verifica a resolucao dos pixel do raster

# visualiza para simples conferencia
raster::plot(altitude_gyn)
plot(sf::st_geometry(goi_muni), add = TRUE)




# verifica compatilizacao dos crs
crs(altitude_gyn)
crs(goiania_osm_filter)


# calcular a declividade

goiania_osm_filter = goiania_osm_filter %>% 
  st_cast("LINESTRING")

# quebra a rede em varios segmentos para aumentar a precisao do calculo
nrow(goiania_osm_filter)
goiania_osm_filter = stplanr::rnet_breakup_vertices(goiania_osm_filter)
nrow(goiania_osm_filter)

# calcula a declividade e cria outro objeto chamando rede_viaria_gyn
goiania_osm_filter$slope = slope_raster(goiania_osm_filter, e = altitude_gyn) #28 segundos

goiania_osm_filter$declive = goiania_osm_filter$slope*100
summary(goiania_osm_filter$declive)

goiania_osm_filter$declive_class =  goiania_osm_filter$declive %>%
  cut(
    breaks = c(0, 3, 5, 8, 10, 20, Inf),
    labels = c("0-3: plano", "3-5: leve","5-8: médio", "8-10: exigente", "10-20: terrível", ">20: impossível"),
    right = F
  )
round(prop.table(table(goiania_osm_filter$declive_class))*100,1)

rede_viaria_gyn <- goiania_osm_filter

st_write(rede_viaria_gyn, "C:/Users/Juciano Rodrigues/Documents/declive_roads/rede_viaria_gyn.shp")


# Criar uma palete de cores, entre o verde escuro e o vermelho escuro

palredgreen = c("#267300", "#70A800", "#FFAA00", "#E60000", "#A80000", "#730000")

rede_gyn <- rede_viaria_gyn %>% 
  dplyr::filter(highway %in% c('primary', "primary_link", 'secondary',"secondary_link", 'tertiary', "tertiary_link",
                               "residential", "cycleway"))

tmap_mode("view")
tmap_options(basemaps = leaflet::providers$CartoDB.Positron) #mapa base
mapadeclives_gyn =
  tm_shape(rede_gyn) +
  tm_lines(
    col = "declive_class",
    palette = palredgreen, #palete de cores
    lwd = 0.7, #espessura das linhas
    title.col = "Declive [%]",
    popup.vars = c("Tipo :" = "highway",
                   "Declive: " = "declive",
                   "Classe: " = "declive_class"),
    popup.format = list(digits = 1),
    # id = "declive"
    id = "name" #se o computaor não conseguir exportar por falta de memória, apagar esta linha.
  )
mapadeclives_gyn

tmap_save(mapadeclives_gyn, "C:/Users/Juciano Rodrigues/Documents/GitHub/declive_brazil_cities/DeclivesGoiania.html")

#