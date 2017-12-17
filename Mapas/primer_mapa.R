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

# Lectura del shape de Colombia por departamento

shape_dpto <- readShapeSpatial("Mapas/Nuevos_mapas/depto.shp", repair = T)
mapa_ordenado <- fortify(shape, region = "DPTO") %>%  data.table()
mapa_ordenado[, id := as.integer(id)]

# Lectura del shape de Colombia por municipio

shape_mpio <- readShapeSpatial("Mapas/Nuevos_mapas/mpio.shp", repair = T)
mapa_ordenado_mpio <- fortify(shape_mpio, region = "MPIO") %>%  data.table()
mapa_ordenado_mpio[, id := as.integer(id)]

# lectura de divipola

divipola <- read.delim("divipola.csv", header = T, sep = ",", encoding = "UTF-8") %>% data.table()
datos_simulados <- divipola[, .(conteo = length(MUNICIPIO)), by = c("DEPARTAMENTO", "C”DIGO.DANE.DEL.DEPARTAMENTO")] %>% setnames(names(.), c("dpto", "cod_dpto", "conteo"))


# Creaci√≥n del mapa

p <- ggplot() + geom_map(data = mapa_ordenado, aes(map_id = id), map = mapa_ordenado, color = "aliceblue", fill = "gray")+
  geom_map(data = datos_simulados, aes(map_id = cod_dpto, fill = conteo), 
           map = mapa_ordenado) + expand_limits(x = mapa_ordenado$long, y = mapa_ordenado$lat)
p


# lectura de divipola

divipola <- read.delim("divipola.csv", header = T, sep = ",", encoding = "UTF-8") %>% data.table() %>% setnames(names(.), c("region", "cod_dpto", "dpto", "cod_mpio", "mpio"))
divipola[, caracteres := nchar(cod_dpto)]
divipola[, cod_mpio1 := substr(cod_mpio, caracteres +1, nchar(cod_mpio))]
datos_simulados <- divipola[, .(conteo = length(mpio)), by = c("mpio", "cod_mpio1", "cod_dpto")]
datos_simulados[, conteo := as.integer(round(rnorm(nrow(datos_simulados), 100, 20)))]

p1 <- ggplot() + geom_map(data = mapa_ordenado_mpio, aes(map_id = id), map = mapa_ordenado_mpio, color = "aliceblue", fill = "gray")+
  geom_map(data = datos_simulados, aes(map_id = cod_mpio1, fill = conteo), 
           map = mapa_ordenado_mpio) + expand_limits(x = mapa_ordenado_mpio$long, y = mapa_ordenado_mpio$lat)
p1

# Tomando un subconjunto de los datos

datos_simulados_mpio <- datos_simulados[cod_dpto == 54]
mapa_ordenado_mpio1 <- mapa_ordenado_mpio[ id %in% datos_simulados_mpio$cod_mpio1]

mapa_ordenado_mpio1[, hole := "FALSE"]
mapa_ordenado_mpio1[, piece := 1]


p1 <- ggplot() + geom_map(data = mapa_ordenado_mpio1, aes(map_id = id), map = mapa_ordenado_mpio1, color = "aliceblue", fill = "gray")+
  geom_map(data = datos_simulados_mpio, aes(map_id = cod_mpio1, fill = conteo), 
           map = mapa_ordenado_mpio1) + expand_limits(x = mapa_ordenado_mpio1$long, y = mapa_ordenado_mpio1$lat)
p1
