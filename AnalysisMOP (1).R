library(fields)
library(parallel)
library(raster)
library(grDevices)
library(maptools)
library(TeachingDemos)
library(dismo)
library(biomod2)
library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(usdm)
library(ENMeval)
library(foreign)
library(spocc)


#########################################################################
#############Correr el analisis para cada escenario######################
#########################################################################
MOP_FOLDER <- "D:/proyectos_sigs/aquila_gcc_nesting/MOP_analyses/"####ruta de la carpeta donde estan todos los archivos
setwd(dir=MOP_FOLDER)
# Load functions
source("mop_v2.R")###llama a la funci?n MOP


###MOP Calibracion
#Indicar donde est?n las capas de la M (presente).
m_layers_path <- list.files(path = "D:/proyectos_sigs/aquila_gcc_nesting/MOP_analyses/capas_climaticas/presente/",
                            pattern = "*.asc$",
                            full.names = T)
#Convertir las capas de la M en un stack
mstack <- stack(m_layers_path)

###########
####FY 1###
###########
#Indicar donde est?n las capas de la projecci?n 1.
g_layers_path1 <- list.files(path = "D:/proyectos_sigs/aquila_gcc_nesting/MOP_analyses/capas_climaticas/2080miroc_spp370/",
                            pattern = "*.asc$",full.names = T)

#Convertir las capas de la projecci?n 1 en un stack
gstack1 <- stack(g_layers_path1)


#Correr el MOP 
mop_normalized1 <- mop(mstack,
                       gstack1,
                       percentil_prop = 0.3,
                       normalized = FALSE)  

#transformar el MOP a una capa normalizada
valor1 <- data.frame(summary(mop_normalized1))
map_fy1_mop1 <- 1-(mop_normalized1/(max(valor1[5,])))

plot(map_fy1_mop1)

##guardar el archivo
setwd("D:/proyectos_sigs/aquila_gcc_nesting/MOP_analyses/2.mop_maps_final/")#DONDE Guardar la capa
writeRaster(map_fy1_mop1, filename="Aquila_chrysaetos_MOP_80_5.asc", overwrite=T, suffix='names')

#1. Transformar las capas del MOP de cada a?o en mapas binarios (>0.1 NO es incertidumbre, <0.1 son ?reas de incertidumbre)
map_fy1_mopbin <- map_fy1_mop1 <= 0.1 ##selecciona como "?reas de extrapolation" aquellos sitios con valor menor a 0.1
setwd("D:/proyectos_sigs/aquila_gcc_nesting/MOP_analyses/3.mop_maps_binarios/")#DONDE Guardar la capa
writeRaster(map_fy1_mopbin, filename="Aquila_chrysaetos_MOPbin_80_5.asc", overwrite=T, suffix='names')
