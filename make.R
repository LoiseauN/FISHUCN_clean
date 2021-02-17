#Loading packages
pkgs <- c("tidyverse","missForest","parallel","here","tidymodels","ranger","caret")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))
  
#Loading all data
path = (here::here("data"))
setwd(path)
files <- list.files(here::here("data"))
lapply(files, load, envir=.GlobalEnv)

#Loading all functions
path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)

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
