# install.packages("plotly")
library("plotly")
library("dplyr")
packageVersion('plotly')
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))
l <- list(color = toRGB("white"), width = 2)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_geo(df, locationmode = 'USA-states') %>%
  add_trace(
    z = ~total.exports, text = ~hover, locations = ~code,
    color = ~total.exports, colors = 'Purples'
  ) %>%
  colorbar(title = "Millions USD") %>%
  layout(
    title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
    geo = g
  )

chart_link = plotly_POST(p, sharing = "public")
chart_link


p <- plot_ly(iris, x = ~Sepal.Length, color = ~Species, type = "box")
p

Sys.setenv("plotly_username"="DoritaSuarez")
Sys.setenv("plotly_api_key"="dianthe77")

p <- ggplot2::diamonds %>% count(cut, clarity) %>%
  plot_ly(x = ~cut, y = ~n, color = ~clarity)



api_create(p, filename = "r-docs/midwest-boxplots")



#Bubble chart

data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")

colors <- c('rgba(204,204,204,1)', 'rgba(222,45,38,0.8)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
            'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
            'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
            'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
            'rgba(204,204,204,1)')


data$State <- as.factor(c('Massachusetts', 'California', 'Massachusetts', 'Pennsylvania', 'New Jersey', 'Illinois', 'Washington DC',
                          'Massachusetts', 'Connecticut', 'New York', 'North Carolina', 'New Hampshire', 'New York', 'Indiana',
                          'New York', 'Michigan', 'Rhode Island', 'California', 'Georgia', 'California', 'California'))

p <- plot_ly(data, x = ~Women, y = ~Men, text = ~School, type = 'scatter', mode = 'markers',
             size =~gap , color = ~State, colors = "Paired", sizes = c(10,50),
             marker = list(opacity = 0.5, sizemode = "diameter"),
             hoverinfo = "text",
             text = ~paste('School:', School, '<br>Gender gap:', gap)) %>%
  layout(title = 'Gender Gap in Earnings per University',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend=F)
p


## Inteto mapas


library(maptools)
library(ggplot2)
library(lattice)
library(sp)

xx <- readShapePoly("0-Dorita/Trabajo/Universidades/Universidad del Rosario/Proyecto CCE - CINTEL/Aplicacion/Intento 1/src/Mapas/COL_adm2.shp",IDvar="NAME_1", proj4string=CRS("+proj=longlat +ellps=clrk66"))

datos <- read.delim("0-Dorita/Trabajo/Universidades/Universidad del Rosario/Proyecto CCE - CINTEL/Aplicacion/Intento 1/src/Secop2limpioAA.txt")
shape <- readShapeSpatial("0-Dorita/Trabajo/Universidades/Universidad del Rosario/Proyecto CCE - CINTEL/Aplicacion/Intento 1/src/Mapas/COL_adm2.shp", repair = T)

deptos <- data.frame(table(datos$Departamento))
deptos <- deptos[-1, ]
names(deptos) <- c("Departamento", "Contratos")
plot(shape, border ="gray")
deptos$Departamento <- toupper(deptos$Departamento)
deptos$Departamento <- iconv(deptos$Departamento, to="ASCII//TRANSLIT")

library(ggplot2)
library(rgdal)
library(rgeos)
library(maptools)
np_dist <- fortify(shape, region = "NAME_2")
np_dist$id <- iconv(np_dist$id, "UTF-8", "latin1")
np_dist$id <- toupper(np_dist$id)  #change ids to uppercase
np_dist$id <- iconv(np_dist$id, to="ASCII//TRANSLIT")
k <- which(np_dist$id== "SANTAFE DE BOGOTA")
np_dist$id[k] <- "BOGOTA"

ggplot() +   geom_map(data = np_dist, aes(map_id = id), map = np_dist, color = "aliceblue", fill = "gray")+
  geom_map(data = deptos, aes(map_id = Departamento, fill = Contratos), 
                    map = np_dist) + expand_limits(x = np_dist$long, y = np_dist$lat)



np_dist$id

deptos <- deptos[-14,]







mapa<-function(xx,depto,sati)
{
  col<-matrix(NA,ncol=2,nrow=length(depto))
  color<-numeric(length(depto))
  for(i in 1:length(depto)){
    ifelse(sati[i]<73.3,color[i]<-"red", ifelse(sati[i]<85.5, color[i]<-"yellow", color[i]<-"darkgreen"))}
  col[,1]<-depto;col[,2]<-color
  col<-col[order(col[,1]),]
  posi<-numeric(length(depto))
  xxx <-xx[xx$NAME_1 %in%depto,]
  plot(xx,border="gray",col="white",lwd=0.9,asp=0,axes =F, las = 1)
  plot(xxx, border="gray", col=col[,2],lwd=1,asp=0,add=T)
  text(coordinates(xxx), labels=sapply(slot(xxx, "polygons"),function(i) slot(i, "ID")), cex=0.65,font=2, pos=1, col="black")
}
mapa(shape,c("Norte de Santander","Quindío","Bolívar","Santander","Atlántico","Valle del Cauca","Cundinamarca","Antioquia"),c(83,78,76.9,75.6,74.3,73.8,70.1,68.6))


library(data.table) # read data with fread()
library(dplyr)  # data manipulation 
library(stringr) # easy string manipulation
library(DT)
# == Shapefile == #
library(sp) # work with Spatial Polygons
library(rgdal) # read the shapefile with readOGR()


library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(ggmap) # maps with ggplot
library(ggiraph) # ggplot interactive <3
library(gganimate) # animate ggplot
library(plotly) # interactive plot

library(leaflet) # interactive maps




  

# Mas graficos ------------------------------------------------------------


segmento <- table(datos$Segmento) %>% data.frame() 
names(segmento) <- c("Segmento", "Cantidad")

p <- plot_ly(segmento, labels = ~Segmento, values = ~Cantidad, type = 'pie') %>%
  layout(title = 'United States Personal Expenditures by Categories in 1960',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


p



colors <- rainbow(n = 5, start = 0, end = 1, alpha = 0.8)
colors <- topo.colors(5)
colors <- c("#1aa3a3", "#b2e5e5","#0b5e56",  "#86becb" )

p <- plot_ly(segmento, labels = ~Segmento, values = ~Cantidad, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste(Cantidad, 'Contratos'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
  layout(title = 'División de los contratos por Segmento',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



p


bp<- ggplot(segmento, aes(x="", y=Cantidad, fill=Segmento))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)

bp

ggplotly(bp)



fton <- function(x){as.numeric(as.character(x))}
ffec <- function(x){as.Date(paste(as.character(x), "0:00:00"), "%d/%m/%Y %H:%M:%S")}



# Construcción de indicadores ---------------------------------------------

datos[, "indicador1"] <- (fton(datos[, "Valor.Contrato"]) - fton(datos[, "Precio.Base.Propuesta"]))/fton(datos[, "Precio.Base.Propuesta"])

datos[, "indicador2"] <- as.numeric(ffec(datos[, "Fecha.Inicio.Ejecucion"]) - ffec(datos[, "Fecha.Inicio.Contrato"]))

datos[, "indicador3"] <- ffec(datos[, "Fecha.Inicio.Ejecucion"]) - ffec(datos[, "Fecha.Creacion.Contrato"])

k <- which(datos$indicador2 == 0)

datos1 <- datos[k, ]

p <- plot_ly(datos1, x = ~indicador1, y = ~indicador2, text = ~Estado.Contrato, type = 'scatter', mode = 'markers', size = ~indicador2, color = ~Segmento, colors = 'Paired',
             sizes = c(10, 40), marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = '',
         xaxis = list(showgrid = FALSE, title = "Diferencia contrato"),
         yaxis = list(showgrid = FALSE, title = "Diferencia dias"),
         showlegend = FALSE)
p
