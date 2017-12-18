library(dplyr)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(ggthemes)
setwd("~/tic-tank/Dorita/CCE - CINTEL/entregable_4_infografias/Prueba_inicial/PruebasInfo/Infografias")

shape_municipios <- read.delim("../Mapas/shape_municipios.txt") %>% data.table()
# Uso 

shape_predeterminado <- shape_municipios[cod_dpto == "91"]
# shape_predeterminado_fondo <- shape_departamental[id == "73"]
datos_simulados22 <- shape_predeterminado[, .(conteo = length(order)), by = id]
datos_simulados22[, clasificacion := "Nivel 1"]
datos_simulados22[, clasificacion := ifelse(conteo < quantile(conteo, 2/3), "Nivel 2", clasificacion)]
datos_simulados22[, clasificacion := ifelse(conteo < quantile(conteo, 1/3), "Nivel 3", clasificacion)]
datos_simulados22[, clasificacion := factor(clasificacion)]

datos_simulados22[, clasificacion5 := "Nivel 1"]
datos_simulados22[, clasificacion5 := ifelse(conteo < quantile(conteo, 4/5), "Nivel 2", clasificacion5)]
datos_simulados22[, clasificacion5 := ifelse(conteo < quantile(conteo, 3/5), "Nivel 3", clasificacion5)]
datos_simulados22[, clasificacion5 := ifelse(conteo < quantile(conteo, 2/5), "Nivel 4", clasificacion5)]
datos_simulados22[, clasificacion5 := ifelse(conteo < quantile(conteo, 1/5), "Nivel 5", clasificacion5)]
datos_simulados22[, clasificacion5 := factor(clasificacion5)]


# map <- ggplot() +
#   geom_map(data = shape_predeterminado, aes( map_id = id), map = shape_predeterminado, color = "aliceblue", fill = "gray") +
#   geom_map(data = datos_simulados22, aes(map_id = id, fill = clasificacion), map = shape_predeterminado) +
#   geom_map(data = shape_predeterminado, aes( map_id = id), map = shape_predeterminado, color = "white", fill = NA) +
#   expand_limits(x = shape_predeterminado$long, y = shape_predeterminado$lat) +
#   scale_fill_manual(values = c("#36E00B", "#F1E818", "#F10034"),
#                     labels = c("Bueno", "Regular", "Malo"),
#                     name = "Clasificación")
# map
# 
# map <- ggplot() +
#   geom_map(data = shape_predeterminado, aes( map_id = id), map = shape_predeterminado, color = "aliceblue", fill = "gray") +
#   geom_map(data = datos_simulados22, aes(map_id = id, fill = clasificacion5), map = shape_predeterminado) +
#   geom_map(data = shape_predeterminado, aes( map_id = id), map = shape_predeterminado, color = "white", fill = NA) +
#   expand_limits(x = shape_predeterminado$long, y = shape_predeterminado$lat)+
#   scale_fill_manual(values = colorRampPalette(brewer.pal(5,"Greens"))(5),
#                     name = "Clasificación")
# 
# map
# 
# map <- ggplot() +
#   geom_map(data = shape_predeterminado, aes( map_id = id), map = shape_predeterminado, color = "aliceblue", fill = "gray") +
#   geom_map(data = datos_simulados22, aes(map_id = id, fill = clasificacion5), map = shape_predeterminado) +
#   geom_map(data = shape_predeterminado, aes( map_id = id), map = shape_predeterminado, color = "white", fill = NA) +
#   expand_limits(x = shape_predeterminado$long, y = shape_predeterminado$lat)+
#   scale_fill_manual(values = colorRampPalette(brewer.pal(5,"Blues"))(5),
#                     name = "Clasificación")
# 
# map
# 
# 
# map <- ggplot() +
#   geom_map(data = shape_predeterminado, aes( map_id = id), map = shape_predeterminado, color = "aliceblue", fill = "gray") +
#   geom_map(data = datos_simulados22, aes(map_id = id, fill = conteo), map = shape_predeterminado) +
#   geom_map(data = shape_predeterminado, aes( map_id = id), map = shape_predeterminado, color = "white", fill = NA) +
#   expand_limits(x = shape_predeterminado$long, y = shape_predeterminado$lat) +
#   scale_fill_gradient(low = "#FF7400",
#                       high ="#36E00B",
#                       name = "Clasificación")
# map

map <- ggplot() +
  geom_map(data = shape_predeterminado, aes( map_id = id), map = shape_predeterminado, color = "aliceblue", fill = "gray") +
  geom_map(data = datos_simulados22, aes(map_id = id, fill = conteo), map = shape_predeterminado) +
  geom_map(data = shape_predeterminado, aes( map_id = id), map = shape_predeterminado, color = "white", fill = NA) +
  expand_limits(x = shape_predeterminado$long, y = shape_predeterminado$lat) +
  scale_fill_gradient(name = "Clasificación")+
  xlab("")+
  ylab("")
map +theme_wsj()


map + theme(
  plot.background = element_rect(fill = "gray"), 
  panel.background = element_rect(fill = NA , colour= NA),
  strip.background = element_rect(fill = "grey95", colour = "white"),
  axis.line = element_line(colour = NA),
  axis.text = element_text(colour = NA),
  axis.ticks = element_line(colour = NA),
  panel.grid.major = element_line(colour = NA),
  panel.grid.minor = element_line(colour = NA),
  # legend.position = "bottom",
  legend.background = element_rect(colour = NA, fill = NA)
)


