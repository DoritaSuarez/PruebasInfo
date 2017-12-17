library("plotly")
library("dplyr")
library("lattice")
library("sp")
library("rgdal")
library("rgeos")
library("maptools")
library("plyr")
library("gapminder")
library("scales")
library("reshape2")
library("tidyverse")
library("data.table")


# Lectura del shape de Colombia

shape <- readShapeSpatial("Mapas/COL_adm2.shp", repair = T)
shape1 <- readShapeSpatial("Mapas/Nuevos_mapas/depto.shp", repair = T)
mapa_ordenado <- fortify(shape1, region = "DPTO") %>%  data.table()
mapa_ordenado[, id := as.integer(id)]
mapa_anterior <- fortify(shape, region = "NAME_2") %>%  data.table()

# lectura de divipola

divipola <- read.delim("divipola.csv", header = T, sep = ",", encoding = "UTF-8") %>% data.table()
datos_simulados <- divipola[, .(conteo = length(MUNICIPIO)), by = c("DEPARTAMENTO", "CÓDIGO.DANE.DEL.DEPARTAMENTO")] %>% setnames(names(.), c("dpto", "cod_dpto", "conteo"))

# Creación del mapa

p <- ggplot() + geom_map(data = mapa_ordenado, aes(map_id = id), map = mapa_ordenado, color = "aliceblue", fill = "red")+
  geom_map(data = datos_simulados, aes(map_id = cod_dpto, fill = conteo), 
           map = mapa_ordenado) + expand_limits(x = mapa_ordenado$long, y = mapa_ordenado$lat)
p
