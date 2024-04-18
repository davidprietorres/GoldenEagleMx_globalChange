library(ecospat)    # comparacion de nichos en espacio ambiental sensu Broenimann et al. (2012)
library(raster)     # manipular rasters
library(dismo)      # dismo (distribution modeling)
library(phyloclim)
library(TeachingDemos)
library(dismo)
library(biomod2)
library(sp)
library(rgeos)
library(maptools)
library(rgdal)
library(usdm)
library(spocc)
library(sf)
library(dplyr)
library(foreign)
library(ggplot2)
library(grid)
library(gtable)
library(rgdal)
library(dplyr)
library(ade4)
library(adehabitatHR)


rm(list=ls()) #elimina TODOS los objetos del ambiente de trabajo
#############################################################################################
#1.Leer el archivo shp de los puntos de anidacion de la especie.
setwd("F:/proyectos_unpublished/aquila_gcc_nesting/shapefiles/")###directorio donde est?n todos los archivos.

###Crear el archivo con datos de nido en el presente
data1 <- readOGR("anidacion_sites2.shp")#presencias de la especie 1
presente <- data.frame(data1)
presente$POINT_Y <- as.numeric(presente$POINT_Y)##convertir la columna de latitud en valores n?mericos
presente$POINT_X <- as.numeric(presente$POINT_X)##convertir la columna de longitud en valores n?mericos

present_fin <- data.frame(presente$POINTID,presente$POINT_X,presente$POINT_Y)##crea el archivo con solo tres columnas
names(present_fin) = c("data", "long", "lat")##renombra a las columnas


###Crear el archivo con datos de nido en el 2040's
data2 <- readOGR("anidacion_sites2.shp")#presencias de la especie 1
nidos_2040 <- data.frame(data2)
nidos_2040$POINT_Y <- as.numeric(nidos_2040$POINT_Y)##convertir la columna de latitud en valores n?mericos
nidos_2040$POINT_X <- as.numeric(nidos_2040$POINT_X)##convertir la columna de longitud en valores n?mericos

final_2040 <- data.frame(nidos_2040$POINTID,nidos_2040$POINT_X,nidos_2040$POINT_Y)
names(final_2040) = c("data", "long", "lat")##renombra a las columnas

###Crear el archivo con datos de nido en el 2060's
data3 <- readOGR("anidacion_sites2.shp")#presencias de la especie 1
nidos_2060 <- data.frame(data3)
nidos_2060$POINT_Y <- as.numeric(nidos_2060$POINT_Y)##convertir la columna de latitud en valores n?mericos
nidos_2060$POINT_X <- as.numeric(nidos_2060$POINT_X)##convertir la columna de longitud en valores n?mericos

final_2060 <- data.frame(nidos_2060$POINTID,nidos_2060$POINT_X,nidos_2060$POINT_Y)
names(final_2060) = c("data", "long", "lat")##renombra a las columnas

###Crear el archivo con datos de nido en el 2080's
data4 <- readOGR("anidacion_sites2.shp")#presencias de la especie 1
nidos_2080 <- data.frame(data4)
nidos_2080$POINT_Y <- as.numeric(nidos_2080$POINT_Y)##convertir la columna de latitud en valores n?mericos
nidos_2080$POINT_X <- as.numeric(nidos_2080$POINT_X)##convertir la columna de longitud en valores n?mericos

final_2080 <- data.frame(nidos_2080$POINTID,nidos_2080$POINT_X,nidos_2080$POINT_Y)
names(final_2080) = c("data", "long", "lat")##renombra a las columnas


##2. Leer los archivos de las condiciones clim?ticas de la M de cada especie.
##2000-Presente
setwd("F:/proyectos_unpublished/aquila_gcc_nesting/capas_climas/present_mx/") 
variables_presente <- list.files(".",pattern = "*.asc$",full.names = T)###crea el stack 
varclim_2000 <- stack(variables_presente)

##2040
setwd("F:/proyectos_unpublished/aquila_gcc_nesting/capas_climas/futuro2040mx/") 
variables_2040 <- list.files(".",pattern = "*.asc$",full.names = T)###crea el stack 
varclim_2040 <- stack(variables_2040)

##2060
setwd("F:/proyectos_unpublished/aquila_gcc_nesting/capas_climas/futuro2060mx/") 
variables_2060 <- list.files(".",pattern = "*.asc$",full.names = T)###crea el stack 
varclim_2060 <- stack(variables_2060)

##2080
setwd("F:/proyectos_unpublished/aquila_gcc_nesting/capas_climas/futuro2080mx/") 
variables_2080 <- list.files(".",pattern = "*.asc$",full.names = T)###crea el stack 
varclim_2080 <- stack(variables_2080)


##3. Crear un data frame de las condiciones clim?ticas del ?rea M de cada especie.
clim_punto_1 <- rasterToPoints(varclim_2000[[1]], fun=NULL, spatial=TRUE)##puntos background del tiempo 1
clim_punto_2 <- rasterToPoints(varclim_2040[[1]], fun=NULL, spatial=TRUE)##puntos background del tiempo 2
clim_punto_3 <- rasterToPoints(varclim_2060[[1]], fun=NULL, spatial=TRUE)##puntos background del tiempo 3
clim_punto_4 <- rasterToPoints(varclim_2080[[1]], fun=NULL, spatial=TRUE)##puntos background del tiempo 4


##4. Extraer los valores de todas las variables ambientales para cada punto de la M
##Tiempo 1
clima_tiempo_1 <- raster::extract(varclim_2000, clim_punto_1)###me da los valores ambientales para las variables de la M sp1.
clima_tiempo_1_1 <- data.frame(coordinates(clim_punto_1),clima_tiempo_1)##convierte el archivo en un dataframe
clima_tiempo_1 <- subset(clima_tiempo_1_1, !is.na(bio_02) & !is.na(bio_07) & !is.na(bio_10) & !is.na(bio_13) & !is.na(bio_14) & !is.na(bio_15))##elimina los datos dentro de la M que no tienen informaci?n completa para las variables (es decir los NA).

names(clima_tiempo_1)[1] = "long" ##cambia el nombre de la columna longitud para que coincida con los otros archivos
names(clima_tiempo_1)[2] = "lat" ##cambia el nombre de la columna latitud para que coincida con los otros archivos


##Tiempo 2
clima_tiempo_2 <- raster::extract(varclim_2040, clim_punto_2)###me da los valores ambientales para las variables de la M sp1.
clima_tiempo_2_1 <- data.frame(coordinates(clim_punto_2),clima_tiempo_2)##convierte el archivo en un dataframe
clima_tiempo_2 <- subset(clima_tiempo_2_1, !is.na(bio_02) & !is.na(bio_07) & !is.na(bio_10) & !is.na(bio_13) & !is.na(bio_14) & !is.na(bio_15))##elimina los datos dentro de la M que no tienen informaci?n completa para las variables (es decir los NA). 

names(clima_tiempo_2)[1] = "long" ##cambia el nombre de la columna longitud para que coincida con los otros archivos
names(clima_tiempo_2)[2] = "lat" ##cambia el nombre de la columna latitud para que coincida con los otros archivos

##Tiempo 3
clima_tiempo_3 <- raster::extract(varclim_2060, clim_punto_3)###me da los valores ambientales para las variables de la M sp1.
clima_tiempo_3_1 <- data.frame(coordinates(clim_punto_3), clima_tiempo_3)##convierte el archivo en un dataframe
clima_tiempo_3 <- subset(clima_tiempo_3_1, !is.na(bio_02) & !is.na(bio_07) & !is.na(bio_10) & !is.na(bio_13) & !is.na(bio_14) & !is.na(bio_15))##elimina los datos dentro de la M que no tienen informaci?n completa para las variables (es decir los NA).

names(clima_tiempo_3)[1] = "long" ##cambia el nombre de la columna longitud para que coincida con los otros archivos
names(clima_tiempo_3)[2] = "lat" ##cambia el nombre de la columna latitud para que coincida con los otros archivos


##Tiempo 4
clima_tiempo_4 <- raster::extract(varclim_2080, clim_punto_4)###me da los valores ambientales para las variables de la M sp1.
clima_tiempo_4_1 <- data.frame(coordinates(clim_punto_4), clima_tiempo_4)##convierte el archivo en un dataframe
clima_tiempo_4 <- subset(clima_tiempo_4_1, !is.na(bio_02) & !is.na(bio_07) & !is.na(bio_10) & !is.na(bio_13) & !is.na(bio_14) & !is.na(bio_15))##elimina los datos dentro de la M que no tienen informaci?n completa para las variables (es decir los NA). 

names(clima_tiempo_4)[1] = "long" ##cambia el nombre de la columna longitud para que coincida con los otros archivos
names(clima_tiempo_4)[2] = "lat" ##cambia el nombre de la columna latitud para que coincida con los otros archivos


##UNIR TODOS LOS DATOS DE CLIMA EN LA MISMA BASE DE DATOS
muestreo1 <- sample_n(clima_tiempo_1, size = 10000)
muestreo2 <- sample_n(clima_tiempo_2, size = 10000)
muestreo3 <- sample_n(clima_tiempo_3, size = 10000)
muestreo4 <- sample_n(clima_tiempo_4, size = 10000)

clim <- rbind(muestreo1, muestreo2, muestreo3, muestreo4)

#5. Seleccionar los datos de presencia de las especies pero considerando solo las coordenadas geogr?ficas.
occ.sp1 <- present_fin[2:3]
occ.sp2 <- final_2040[2:3]
occ.sp3 <- final_2060[2:3]
occ.sp4 <- final_2080[2:3]

#6.Integrar la informaci?n de datos de ocurrencia y los datos del background para cada especie. Adem?s limpiar los datos a la resoluci?n espacial deseada (en este caso 1km)
occ_sp1 <- na.exclude(ecospat.sample.envar (dfsp = occ.sp1,colspxy = 1:2, 
                                           colspkept = 1:2, dfvar = clima_tiempo_1,
                                           colvarxy= 1:2, colvar="all", resolution= 0.04166667))

occ_sp2 <- na.exclude(ecospat.sample.envar (dfsp = occ.sp2,colspxy = 1:2, 
                                            colspkept = 1:2, dfvar = clima_tiempo_2,
                                            colvarxy= 1:2, colvar="all", resolution= 0.04166667))

occ_sp3 <- na.exclude(ecospat.sample.envar (dfsp = occ.sp3,colspxy = 1:2, 
                                            colspkept = 1:2, dfvar = clima_tiempo_3,
                                            colvarxy= 1:2, colvar="all", resolution= 0.04166667))

occ_sp4 <- na.exclude(ecospat.sample.envar (dfsp = occ.sp4,colspxy = 1:2, 
                                            colspkept = 1:2, dfvar = clima_tiempo_4,
                                            colvarxy= 1:2, colvar="all", resolution= 0.04166667))

###UNIR TODOS LOS DATOS DE ANIDACI?N CON SUS CLIMAS EN CADA ESCENARIO CLIMATICO EN UN SOLO ARCHIVO DE DATOS
data <- rbind(clim[,3:8], occ_sp1[,3:8], occ_sp2[,3:8], occ_sp3[,3:8], occ_sp4[,3:8])##SELECCIONA SOLO los datos de clima de cada sitio de anidaci?n

###CONCATENAR EN UNA SOLA BASE DE DATOS LA INFORMACI?N DE CLIMAS EN M?XICO PARA TODOS LOS ESCENARIOS CLIMATICOS Y LOS CLIMAS DE LAS LOCALIDADES DE ANIDACION EN CADA TIEMPO ANALIZADO
w <- c(rep(1,nrow(clim)), rep(0,nrow(occ_sp1)), rep(0,nrow(occ_sp2)), rep(0,nrow(occ_sp3)), rep(0,nrow(occ_sp4)))#ESTO LE PONE UN UNO "1" A LOS DATOS DE CLIMAS EN "M" Y CERO "0" A LOS DATOS DE SITIOS DE ANIDACION


### PCA is done with all data from the study area
pca.cal <- dudi.pca(data, row.w = w, center = T, scale = T, scannf = F, nf = 2)

row.clim <- 1:nrow(clim)
row.sp1 <- (1+ nrow(clim)):(nrow(clim) + nrow(occ_sp1))
row.sp2 <- (1 + nrow(clim) + nrow(occ_sp1)) : (nrow(clim) + nrow(occ_sp1) + nrow(occ_sp2))
row.sp3 <- (1 + nrow(clim) + nrow(occ_sp1) + nrow(occ_sp2)) : (nrow(clim) + nrow(occ_sp1) + nrow(occ_sp2) + nrow(occ_sp3))
row.sp4 <- (1 + nrow(clim) + nrow(occ_sp1) + nrow(occ_sp2) + nrow(occ_sp3)) : (nrow(clim) + nrow(occ_sp1) + nrow(occ_sp2) + nrow(occ_sp3) + nrow(occ_sp4))


### Having the PCA results, we need the 1er and 2do eigenvector values for the background and the occurrence records per year
### Coordinates in each axis of PCA of all the study area and each breeding-site/year
scores.clim <- pca.cal$li[row.clim, ]
scores.sp1 <- pca.cal$li[row.sp1, ]
scores.sp2 <- pca.cal$li[row.sp2, ]
scores.sp3 <- pca.cal$li[row.sp3, ]
scores.sp4 <- pca.cal$li[row.sp4, ]

### Contribution of each variable to each PCA component
windows()
ecospat.plot.contrib(contrib = pca.cal$co, eigen = pca.cal$eig)


### Resolution of the environmental space based on the PCA values calculated for the background and occurrence records
R <- 100 ### This is the same as using R=100 in the lines below

### Create density surfaces of occurrence in the environmental space (two axis), considering the observed occurrence density and availability of condition in the background
z0 <- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.clim, R=100) 
z1 <- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.sp1, R=100) 
z2 <- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.sp2, R=100) 
z3 <- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.sp3, R=100) 
z4 <- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.sp4, R=100)



#Metricos de sobrelape de nicho observado (D - el metrico de Schoener e I - el metrico de Warren)
ecospat.niche.overlap (z1=z1, z2=z2, cor=TRUE)
ecospat.niche.overlap (z1=z1, z2=z3, cor=TRUE)
ecospat.niche.overlap (z1=z1, z2=z4, cor=TRUE)


a.dyn1<-ecospat.niche.equivalency.test(z1=z1 , z2=z2, rep=1000)
a.dyn2<-ecospat.niche.equivalency.test(z1=z1 , z2=z3, rep=1000)
a.dyn3<-ecospat.niche.equivalency.test(z1=z1 , z2=z4, rep=1000)

#GRAFICO PARA EL TEST DE EQUIVALENCIA DE NICHO
windows() # en mac o windows() en PC
par(mfrow=c(1,3))
ecospat.plot.overlap.test(a.dyn1,"D","Equivalencia")
ecospat.plot.overlap.test(a.dyn2,"D","Equivalencia")
ecospat.plot.overlap.test(a.dyn3,"D","Equivalencia")



b.dyn_2040_1 <-ecospat.niche.similarity.test(z1=z1 , z2=z2, rep=1000, alternative = "greater", rand.type=2)
b.dyn_2040_2 <-ecospat.niche.similarity.test(z1=z2 , z2=z1, rep=1000, alternative = "greater", rand.type=2)

b.dyn_2060_1 <-ecospat.niche.similarity.test(z1=z1 , z2=z3, rep=1000, alternative = "greater", rand.type=2)
b.dyn_2060_2 <-ecospat.niche.similarity.test(z1=z3 , z2=z1, rep=1000, alternative = "greater", rand.type=2)

b.dyn_2080_1 <-ecospat.niche.similarity.test(z1=z1 , z2=z4, rep=1000, alternative = "greater", rand.type=2)
b.dyn_2080_2 <-ecospat.niche.similarity.test(z1=z4 , z2=z1, rep=1000, alternative = "greater", rand.type=2)



### Individual niche plots. Modifications of plot.niche functions from Broenniman et al. 2012 and Silva et al. 2014.
n.groups <- 4
g.names <- c("Current", "2040's", "2060's", "2080's")
g.codenames <- c("Current", "2040's", "2060's", "2080's")

g.colors <- c("green", "yellow", "orange", "red")
z <- c(list(z1, z2, z3, z4))


plot.niche.all(z, n.groups, g.names,
               contornar = TRUE, 
               densidade = TRUE,
               quantis = 6,
               back = FALSE, title = "",
               g.colors, n = 3,
               cor1)







