#Loading packages
pkgs <- c("tidyverse","missForest","parallel","here")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#Load all files from data folder
load(file=file.path(data_dir,"dummy_dataset.Rdata"))
load(file=file.path(data_dir,"dummy_dataset_C.Rdata"))
load(file=file.path(data_dir,"dummy_dataset_NC.Rdata"))
load(file=file.path(data_dir,"dummy_dataset_NA.Rdata"))
load(file=file.path(data_dir,"dummy_dataset_noNA.Rdata"))
load(file=file.path(data_dir,"dummy_dataset_predict.Rdata"))
load(file=file.path(data_dir,"dummy_dataset_species.Rdata"))

#Loading all functions
source(here::here("R","data_prep.R"))
source(here::here("R","IUCN_test.R"))
source(here::here("R","IUCN_predict.R"))
source(here::here("R","missForest.R"))

l

#Running code

#Trying out missforest
test_missForest = missForest_test(dummy_dataset_noNA)

#Applying missforest
run_missForest = missForest_applied(dummy_dataset_NA,0.9,test_missForest)

#Splitting data with NA filled out by missForest
split = data_prep(dummy_dataset_NC,dummy_dataset_C)

#Trying out IUCN predictions
test_IUCN = IUCN_test(split)

#Running IUCN predictions
run_IUCN = IUCN_predict(split,dummy_dataset_predict,dummy_dataset_species)

#IUCN consensus (0.5 for this dummy dataset)
IUCN_final = IUCN_consensus(run_IUCN,length(split),50)
