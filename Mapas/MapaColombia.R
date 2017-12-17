# Librerias

library("plotly")
library("dplyr")
library("lattice")
library("sp")
library("rgdal")
library("rgeos")
library("maptools")
library("plyr")
library("scales")
library("reshape2")
# Lectura y manipulación previa de los datos y shape ----------------------
# El archivo de shape tiene 3 niveles. Nivel 0: Fronteras, Nivel 1: Departamento, Nivel 2: Municipios

datos <- read.delim("0-Dorita/Trabajo/Universidades/Universidad del Rosario/Proyecto CCE - CINTEL/Aplicacion/Intento 1/src/Secop2limpioAA.txt")
shape <- readShapeSpatial("0-Dorita/Trabajo/Universidades/Universidad del Rosario/Proyecto CCE - CINTEL/Aplicacion/Intento 1/src/Mapas/COL_adm2.shp", repair = T)

## Conversiones necesarias sobre base secop

deptos <- table(datos$Departamento) %>% data.frame() %>% .[-1,]
names(deptos) <- c("Departamento", "Contratos")
deptos$Departamento <- toupper(deptos$Departamento) %>% iconv(., to="ASCII//TRANSLIT")

## Conversiones necesarias sobre shape

np_dist <- fortify(shape, region = "NAME_2")
np_dist$id <- iconv(np_dist$id, "UTF-8", "latin1") %>% toupper(.) %>% iconv(., to="ASCII//TRANSLIT")
k <- which(np_dist$id== "SANTAFE DE BOGOTA") # Caso aislado - Hablar con CCE
np_dist$id[k] <- "BOGOTA"


# Generación de mapas -----------------------------------------------------

# Mapa básico

p <- ggplot() +   geom_map(data = np_dist, aes(map_id = id), map = np_dist, color = "aliceblue", fill = "gray")+
  geom_map(data = deptos, aes(map_id = Departamento, fill = Contratos), 
           map = np_dist) + expand_limits(x = np_dist$long, y = np_dist$lat)+ylim(c(-5,13)) + xlim(c(-85,-60))

p

ggplotly(p)

valorContratos <- ddply(datos, .(Departamento), summarize, Medias = mean(as.numeric(as.character(Valor.Contrato)), na.rm = T)) %>% .[-1,]
valorContratos$Departamento <- toupper(valorContratos$Departamento) %>% iconv(., to="ASCII//TRANSLIT")

library(gapminder)

p <- ggplot(gapminder, aes(x = year, y = lifeExp, text = paste("country:",country))) + 
  geom_point() +
  facet_wrap(~ continent)

ggplot() + geom_map(data = np_dist, aes(map_id = id), map = np_dist, color = "aliceblue", fill = "gray")+
  geom_map(data = valorContratos, aes(map_id = Departamento, fill = Medias), 
                    map = np_dist) + expand_limits(x = np_dist$long, y = np_dist$lat)


ggplot() + geom_map(data = np_dist, aes(map_id = id), map = np_dist, color = "aliceblue", fill = "gray")+
  geom_map(data = valorContratos, aes(map_id = Departamento, fill = Medias), 
                    map = np_dist) + expand_limits(x = np_dist$long, y = np_dist$lat) +
                    scale_fill_gradient2(low = muted("red"), mid = "deepskyblue", midpoint = mean(valorContratos$Medias), high = muted("darkblue"), limits = c(0, max(valorContratos$Medias)))


namesInData <- levels(factor(deptos$Departamento))
namesInMap <- levels(factor(np_dist$id))

# Verificando en donde no coinciden los dos mapas

namesInData[which(!namesInData %in% namesInMap)]
idmap <- substr(namesInMap, 1, 5)
idbas <- substr(namesInData, 1, 5)

namesInMap[which(!namesInMap %in% namesInData)]
namesInMap[which(!idbas %in% idmap)]


### Graficos, los que siguen 

df <- table( datos$Modalidad.Contratacion, datos$Segmento) %>% data.frame()
names(df) <- c("Tipo_Contrato", "Segmento", "Contratos")

p <- plot_ly(data = df, x =~ Segmento, y=~Contratos, color= ~Tipo_Contrato, text = ~paste(Tipo_Contrato, Contratos, 'Contratos'))

p
