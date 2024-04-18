install.packages ("tidyverse")
install.packages("tibble")
install.packages("dismo")
install.packages("rJava")

install.packages(c("rgbif","TeachingDemos","biomod2", "sp", "raster", "rgeos", "maptools", "rgdal", "usdm", "ENMeval", "foreign", "spocc", "corrplot", "parallel", ""))


# Installing and loading packages
if(!require(devtools)){
  install.packages("devtools")
}

if(!require(kuenm)){
  devtools::install_github("marlonecobos/kuenm")
}

library(kuenm)


# Without vignette
remotes::install_github("Model-R/modleR", build = TRUE, force = TRUE)
remotes::install_github("mrmaxent/maxnet", force = TRUE)

library(tidyverse)
library(tibble)
library(dismo)
library(rgbif)
library(TeachingDemos)
library(biomod2)
library(sp)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(usdm)
library(ENMeval)
library(foreign)
library(spocc)
library(corrplot)
library(parallel)
library(maxnet)
library(modleR)
library(rJava)

rm(list=ls()) #elimina TODOS los objetos del ambiente de trabajo

################DATOS DE PRESENCIA DE LA ESPECIE##########################
occs2 <- read.csv2("F:/models3/Aquila_chrysaetos_model.csv", sep =",", header=TRUE)
names(occs2)
##########Crear archivo CSV para los puntos a usar en los modelos####
occs2$lat <- as.numeric(as.character(occs2$lat))
occs2$lon <- as.numeric(as.character(occs2$lon))

sp<- as.character(occs2$species) ## seleccionar la columna del nombre de la especie
lat<- as.numeric(occs2$lat)## seleccionar la columna del nombre de la latitud
lon<- as.numeric(occs2$lon)## seleccionar la columna del nombre de la longitud
occs3<-data.frame(sp,lon,lat) ## crear el archivo con solo esas tres variables!

occs= occs3

################DATOS AMBIENTALES EN EL AREA M ##########################
setwd("F:/models3/capas_climas/present_pca/")
pca_path <- list.files(".",pattern = "*.asc$",full.names = T)###crea el stack de las 19 variables climaticas del presente
capas_presente<- stack(pca_path)
example_vars= capas_presente


#####CORRER LOS MODELOS Y PARAMETROS##########
setwd("F:/models3/modleR_test_pca/")
test_folder <- "F:/models3/modleR_test_pca/"

###configurar los datos para repartir los datos (replicas) y quitar duplicados, NA y etc.
sdmdata_1sp <- setup_sdmdata(species_name = "Aquila_chrysaetos",
                             occurrences = occs,
                             predictors = example_vars,
                             models_dir = test_folder,
                             partition_type = "crossvalidation",
                             cv_partitions = 10,
                             cv_n = 1,
                             seed = 633,
                             buffer_type = NULL,
                             dist_buf= 10,
                             png_sdmdata = TRUE,
                             n_back = 10000,
                             clean_dupl = TRUE,
                             clean_uni = TRUE,
                             clean_nas = TRUE,
                             geo_filt = FALSE,
                             geo_filt_dist = FALSE,
                             select_variables = FALSE,
                             sample_proportion = 0.5,
                             cutoff = 0.8)

###correr los modelos
species_name = "Aquila_chrysaetos"
predictors = example_vars
models_dir = test_folder
project_model = TRUE
write_bin_cut = TRUE
dismo_threshold = "spec_sens"
proc_threshold = 5
proj_data_folder = file.path("F:/models3/capas_climas/futuro_pca/")###directorio donde están las capas del futuro


#####################################################
######## CORRER LOS MODELOS #########################
many <- do_many(species_name = species_name,
                predictors = predictors,
                models_dir = models_dir,
                png_partitions = FALSE,
                write_rda = FALSE,
                bioclim = FALSE,
                domain = FALSE,
                mahal = FALSE,
                maxent = TRUE,
                maxnet = FALSE,
                glm = FALSE,
                svmk = FALSE,
                svme = FALSE,
                rf = FALSE,
                brt = FALSE,
                equalize = TRUE,
                write_bin_cut = write_bin_cut,
                dismo_threshold = dismo_threshold,
                proc_threshold = proc_threshold,
                project_model = project_model, 
                proj_data_folder = proj_data_folder)


 ###############################################################
##### CORRER LOS MODELOS EN EL AREA M #########################
final_dir = "final_models"
mean_th_par = "spec_sens"
algorithms = NULL
species_area_M <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "present",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              mean_th_par = mean_th_par,
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens1 <- ensemble_model(species_name = species_name,
                       occurrences = occs,
                       performance_metric = "pROC",
                       which_ensemble = c("pca",
                                          "consensus"),
                       consensus_level = 0.5,
                       which_final = c("bin_mean",
                                       "bin_consensus"),
                       models_dir = test_folder,
                       png_final = FALSE,
                       overwrite = TRUE) #argument from writeRaster



###################################################################
#####  CORRER LOS MODELOS EN EL FUTURO ############################
######################
#######2040###########
###2040: bcc_csm2_spp370  
species_2040_1 <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "proj2",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              mean_th_par = mean_th_par,
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens_species_2040_1 <- ensemble_model(species_name = species_name,
                       occurrences = occs,
                       proj_dir = "proj2",
                       performance_metric = "pROC",
                       which_ensemble = c("pca",
                                          "consensus"),
                       consensus_level = 0.5,
                       which_final = c("bin_mean",
                                       "bin_consensus"),
                       models_dir = test_folder,
                       png_final = FALSE,
                       overwrite = TRUE) #argument from writeRaster

###2040: canesm_spp370
species_2040_2 <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "proj3",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              mean_th_par = mean_th_par,
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens_species_2040_2 <- ensemble_model(species_name = species_name,
                                     occurrences = occs,
                                     proj_dir = "proj3",
                                     performance_metric = "pROC",
                                     which_ensemble = c("pca",
                                                        "consensus"),
                                     consensus_level = 0.5,
                                     which_final = c("bin_mean",
                                                     "bin_consensus"),
                                     models_dir = test_folder,
                                     png_final = FALSE,
                                     overwrite = TRUE) #argument from writeRaster

###2040: cnrm_cm6_spp370
species_2040_3 <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "proj4",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              mean_th_par = mean_th_par,
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens_species_2040_3 <- ensemble_model(species_name = species_name,
                                     occurrences = occs,
                                     proj_dir = "proj4",
                                     performance_metric = "pROC",
                                     which_ensemble = c("pca",
                                                        "consensus"),
                                     consensus_level = 0.5,
                                     which_final = c("bin_mean",
                                                     "bin_consensus"),
                                     models_dir = test_folder,
                                     png_final = FALSE,
                                     overwrite = TRUE) #argument from writeRaster

###2040: ipslcm6_spp370
species_2040_4 <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "proj5",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              mean_th_par = mean_th_par,
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens_species_2040_4 <- ensemble_model(species_name = species_name,
                                     occurrences = occs,
                                     proj_dir = "proj5",
                                     performance_metric = "pROC",
                                     which_ensemble = c("pca",
                                                        "consensus"),
                                     consensus_level = 0.5,
                                     which_final = c("bin_mean",
                                                     "bin_consensus"),
                                     models_dir = test_folder,
                                     png_final = FALSE,
                                     overwrite = TRUE) #argument from writeRaster

###2040: miroc_spp370
species_2040_5 <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "proj6",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              mean_th_par = mean_th_par,
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens_species_2040_5 <- ensemble_model(species_name = species_name,
                                     occurrences = occs,
                                     proj_dir = "proj6",
                                     performance_metric = "pROC",
                                     which_ensemble = c("pca",
                                                        "consensus"),
                                     consensus_level = 0.5,
                                     which_final = c("bin_mean",
                                                     "bin_consensus"),
                                     models_dir = test_folder,
                                     png_final = FALSE,
                                     overwrite = TRUE) #argument from writeRaster

######################
#######2060###########
###2060: bcc_csm2_spp370  
species_2060_1 <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "proj7",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              mean_th_par = mean_th_par,
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens_species_2060_1 <- ensemble_model(species_name = species_name,
                                     occurrences = occs,
                                     proj_dir = "proj7",
                                     performance_metric = "pROC",
                                     which_ensemble = c("pca",
                                                        "consensus"),
                                     consensus_level = 0.5,
                                     which_final = c("bin_mean",
                                                     "bin_consensus"),
                                     models_dir = test_folder,
                                     png_final = FALSE,
                                     overwrite = TRUE) #argument from writeRaster

###2060: canesm_spp370
species_2060_2 <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "proj8",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              mean_th_par = mean_th_par,
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens_species_2060_2 <- ensemble_model(species_name = species_name,
                                     occurrences = occs,
                                     proj_dir = "proj8",
                                     performance_metric = "pROC",
                                     which_ensemble = c("pca",
                                                        "consensus"),
                                     consensus_level = 0.5,
                                     which_final = c("bin_mean",
                                                     "bin_consensus"),
                                     models_dir = test_folder,
                                     png_final = FALSE,
                                     overwrite = TRUE) #argument from writeRaster

###2060: cnrm_cm6_spp370
species_2060_3 <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "proj9",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              mean_th_par = mean_th_par,
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens_species_2060_3 <- ensemble_model(species_name = species_name,
                                     occurrences = occs,
                                     proj_dir = "proj9",
                                     performance_metric = "pROC",
                                     which_ensemble = c("pca",
                                                        "consensus"),
                                     consensus_level = 0.5,
                                     which_final = c("bin_mean",
                                                     "bin_consensus"),
                                     models_dir = test_folder,
                                     png_final = FALSE,
                                     overwrite = TRUE) #argument from writeRaster

###2060: ipslcm6_spp370
species_2060_4 <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "proj10",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              mean_th_par = mean_th_par,
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens_species_2060_4 <- ensemble_model(species_name = species_name,
                                     occurrences = occs,
                                     proj_dir = "proj10",
                                     performance_metric = "pROC",
                                     which_ensemble = c("pca",
                                                        "consensus"),
                                     consensus_level = 0.5,
                                     which_final = c("bin_mean",
                                                     "bin_consensus"),
                                     models_dir = test_folder,
                                     png_final = FALSE,
                                     overwrite = TRUE) #argument from writeRaster


###2060: miroc_spp370
species_2060_5 <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "proj11",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              mean_th_par = mean_th_par,
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens_species_2060_5 <- ensemble_model(species_name = species_name,
                                     occurrences = occs,
                                     proj_dir = "proj11",
                                     performance_metric = "pROC",
                                     which_ensemble = c("pca",
                                                        "consensus"),
                                     consensus_level = 0.5,
                                     which_final = c("bin_mean",
                                                     "bin_consensus"),
                                     models_dir = test_folder,
                                     png_final = FALSE,
                                     overwrite = TRUE) #argument from writeRaster

######################
#######2080###########
###2080: bcc_csm2_spp370  
species_2080_1 <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "proj12",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              mean_th_par = mean_th_par,
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens_species_2080_1 <- ensemble_model(species_name = species_name,
                                     occurrences = occs,
                                     proj_dir = "proj12",
                                     performance_metric = "pROC",
                                     which_ensemble = c("pca",
                                                        "consensus"),
                                     consensus_level = 0.5,
                                     which_final = c("bin_mean",
                                                     "bin_consensus"),
                                     models_dir = test_folder,
                                     png_final = FALSE,
                                     overwrite = TRUE) #argument from writeRaster

###2080: canesm_spp370
species_2080_2 <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "proj13",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              mean_th_par = mean_th_par,
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens_species_2080_2 <- ensemble_model(species_name = species_name,
                                     occurrences = occs,
                                     proj_dir = "proj13",
                                     performance_metric = "pROC",
                                     which_ensemble = c("pca",
                                                        "consensus"),
                                     consensus_level = 0.5,
                                     which_final = c("bin_mean",
                                                     "bin_consensus"),
                                     models_dir = test_folder,
                                     png_final = FALSE,
                                     overwrite = TRUE) #argument from writeRaster

###2080: cnrm_cm6_spp370
species_2080_3 <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "proj14",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              mean_th_par = mean_th_par,
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens_species_2080_3 <- ensemble_model(species_name = species_name,
                                     occurrences = occs,
                                     proj_dir = "proj14",
                                     performance_metric = "pROC",
                                     which_ensemble = c("pca",
                                                        "consensus"),
                                     consensus_level = 0.5,
                                     which_final = c("bin_mean",
                                                     "bin_consensus"),
                                     models_dir = test_folder,
                                     png_final = FALSE,
                                     overwrite = TRUE) #argument from writeRaster

###2080: ipslcm6_spp370
species_2080_4 <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "proj15",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              mean_th_par = mean_th_par,
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens_species_2080_4 <- ensemble_model(species_name = species_name,
                                     occurrences = occs,
                                     proj_dir = "proj15",
                                     performance_metric = "pROC",
                                     which_ensemble = c("pca",
                                                        "consensus"),
                                     consensus_level = 0.5,
                                     which_final = c("bin_mean",
                                                     "bin_consensus"),
                                     models_dir = test_folder,
                                     png_final = FALSE,
                                     overwrite = TRUE) #argument from writeRaster


###2080: miroc_spp370
species_2080_5 <- final_model(species_name = species_name,
                              algorithms = algorithms,
                              models_dir = models_dir,
                              final_dir = final_dir,
                              png_final = TRUE,
                              proj_dir = "proj16",
                              which_models = c("bin_mean",
                                               "bin_consensus"),
                              consensus_level = 0.5,
                              uncertainty = TRUE,
                              overwrite = TRUE)

ens_species_2080_5 <- ensemble_model(species_name = species_name,
                                     occurrences = occs,
                                     proj_dir = "proj16",
                                     performance_metric = "pROC",
                                     which_ensemble = c("pca",
                                                        "consensus"),
                                     consensus_level = 0.5,
                                     which_final = c("bin_mean",
                                                     "bin_consensus"),
                                     models_dir = test_folder,
                                     png_final = FALSE,
                                     overwrite = TRUE) #argument from writeRaster

###FIN