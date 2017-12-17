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
mapa_ordenado <- fortify(shape_dpto, region = "DPTO") %>%  data.table()
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

datos_simulados_mpio <- datos_simulados[cod_dpto == 50]
mapa_ordenado_mpio1 <- mapa_ordenado_mpio[ id %in% datos_simulados_mpio$cod_mpio1]

datos_simulados2 <- mapa_ordenado_mpio_1[, .(conteo = length(lat)), by = id]


p1 <- ggplot() + geom_map(data = mapa_ordenado_mpio1, aes(map_id = id), map = mapa_ordenado_mpio1, color = "aliceblue", fill = "gray")+
  geom_map(data = datos_simulados2, aes(map_id = id, fill = conteo), 
           map = mapa_ordenado_mpio1) + expand_limits(x = mapa_ordenado_mpio1$long, y = mapa_ordenado_mpio1$lat)
p1


shape_dpto <- readShapeSpatial("Mapas/Nuevos_mapas/depto.shp", repair = T)

shape2 <- readShapeSpatial("Mapas/COL_adm2.shp", repair = T)
cruce <- data.table(shape2$ID_1, shape2$NAME_1, shape2$ID_2, shape2$NAME_2) %>%  setnames(names(.), c("cod_dpto", "dpto", "cod_mpio", "mpio"))
cruce[, mpio := iconv(mpio, from = "UTF-8", to = "latin1")]
cruce[, dpto := iconv(dpto, from = "UTF-8", to = "latin1")]

cruce[, cod_mpio := as.character(cod_mpio)]
cruce[, cod_dpto := as.character(cod_dpto)]

shape22 <- fortify(shape2, region = "ID_2") %>%  data.table() %>% left_join(., cruce, by = c("id" = "cod_mpio")) %>% data.table() %>% data.table()

shape12 <- shape22[cod_dpto == 1]
datos_simulados22 <- shape12[, .(conteo = length(order)), by = id]

p1 <- ggplot() + geom_map(data = shape12, aes(map_id = id), map = shape12, color = "aliceblue", fill = "gray")+
  geom_map(data = datos_simulados22, aes(map_id = id, fill = conteo), 
           map = shape12) + expand_limits(x = shape12$long, y = shape12$lat)
p1

# Corrigiendo departamentos

diferentes_divipola <- unique(divipola$dpto)[!(unique(divipola$dpto) %in% unique(shape22$dpto))] %>% .[2] %>% as.character()
diferentes_shape <- unique(shape22$dpto)[!(unique(shape22$dpto) %in% unique(divipola$dpto))]
shape22[, dpto := ifelse(dpto == "San AndrÈs y Providencia", "ArchipiÈlago de San AndrÈs, Providencia y Santa Catalina", dpto)]

# Corrigiendo municipios

diferentes_divipola <- unique(divipola$mpio)[!(unique(divipola$mpio) %in% unique(shape22$mpio))] %>% data.table() %>% setnames(names(.), "mpio")
diferentes_shape <- unique(shape22$mpio)[!(unique(shape22$mpio) %in% unique(divipola$mpio))] %>% data.table() %>% setnames(names(.), "mpio") %>%  .[order(.$mpio)] %>% data.table()

diferentes_divipola[, mpio := as.character(mpio)]
diferentes_divipola[, sin_a := iconv(mpio, from="UTF-8", to = "latin1")]
diferentes_divipola[, sin_a := tolower(iconv(sin_a, to="ASCII//TRANSLIT"))]

diferentes_shape[, sin_a := tolower(iconv(mpio, to="ASCII//TRANSLIT"))]

cruce1 <- left_join(diferentes_shape, diferentes_divipola, by = "sin_a") %>% na.omit()
shape221 <- left_join(shape22, cruce1, by = c("mpio" = "mpio.x")) %>% data.table()
shape221[, mpio := ifelse(is.na(mpio.y), mpio, mpio.y)]
shape221[, sin_a := NULL]
shape221[, mpio.y := NULL]

shape22 <- shape221

mpios_cambio <- read.delim("shape_divipola_nombres", header = T) %>% data.table()

cambios <- data.table(diferentes_divipola, diferentes_shape)
shape221 <- left_join(shape22, mpios_cambio, by = c("mpio" = "mpios_shape")) %>% data.table()
shape221[, mpios_divipola := as.character(mpios_divipola)]
shape221[, mpio := ifelse(is.na(mpios_divipola), mpio, mpios_divipola)]
shape221[, mpios_divipola := NULL]

shape22 <- shape221

# Asignando los cÛdigos divipola

divipola_codigos <- divipola[, c("mpio", "cod_mpio1", "dpto", "cod_dpto")] %>% setnames(names(.), c("mpio", "cod_mpio_dp", "dpto", "cod_dpto_dp"))
shape221 <- left_join(shape22, divipola_codigos, by = c("mpio", "dpto")) %>% data.table()
shape221 <- shape221[, c("long", "lat", "order", "hole", "piece", "cod_mpio_dp", "group", "cod_dpto_dp", "mpio", "dpto")] %>%
  setnames(c("cod_mpio_dp", "cod_dpto_dp"), c("id", "cod_dpto")) 
  
shape22 <- shape221

shape_bogota <- shape22[mpio  == "Bogot· D.C."]
shape_sin_bogota <- shape22[mpio  != "Bogot· D.C."]




# Shapes finales ----------------------------------------------------------

shape_departamental <- mapa_ordenado
shape_municipios <- shape_sin_bogota
shape_bogota <- shape_bogota

write.table(shape_departamental, "shape_departamentos.txt", sep = "\t")
write.table(shape_municipios, "shape_municipios.txt", sep = "\t")
write.table(shape_bogota, "shape_bogota.txt", sep = "\t")



# Uso 

shape_predeterminado <- shape_municipios[cod_dpto == "73"]
datos_simulados22 <- shape_predeterminado[, .(conteo = length(order)), by = id]

p1 <- ggplot() + geom_map(data = shape_predeterminado, aes(map_id = id), map = shape_predeterminado, color = "aliceblue", fill = "gray")+
  geom_map(data = datos_simulados22, aes(map_id = id, fill = conteo), 
           map = shape_predeterminado) + expand_limits(x = shape_predeterminado$long, y = shape_predeterminado$lat)
p1
