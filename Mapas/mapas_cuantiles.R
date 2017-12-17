library(dplyr)
library(ggplot2)
library(data.table)

shape_municipios <- read.delim("shape_municipios.txt") %>% data.table()
# Uso 

shape_predeterminado <- shape_municipios[cod_dpto == "91"]
# shape_predeterminado_fondo <- shape_departamental[id == "73"]
datos_simulados22 <- shape_predeterminado[, .(conteo = length(order)), by = id]
datos_simulados22[, clasificacion := "Nivel 1"]
datos_simulados22[, clasificacion := ifelse(conteo < quantile(conteo, 2/3), "Nivel 2", clasificacion)]
datos_simulados22[, clasificacion := ifelse(conteo < quantile(conteo, 1/3), "Nivel 3", clasificacion)]
datos_simulados22[, clasificacion := factor(clasificacion)]



ggplot() +
  geom_map(data = shape_predeterminado, aes( map_id = id), map = shape_predeterminado, color = "aliceblue", fill = "gray") +
  geom_map(data = datos_simulados22, aes(map_id = id, fill = clasificacion), map = shape_predeterminado) +
  geom_map(data = shape_predeterminado, aes( map_id = id), map = shape_predeterminado, color = "white", fill = NA) +
  expand_limits(x = shape_predeterminado$long, y = shape_predeterminado$lat) +
  scale_fill_manual(values = c("#00C1FF", "#D0E100", "#FF2E00"),
                    labels = c("Bueno", "Regular", "Malo"),
                    name = "Clasificación")



