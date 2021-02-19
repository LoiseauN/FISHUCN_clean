#' Run the Entire Project
#'
#' This script reproduces all analyses and figures of the ___________ article.
#'
#' @author Raphaël SEGUIN, \email{raphael.seguin46@@gmail.com},
#'         Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com},
#'         Valentine FLEURE, \email{valentine.fleure@@gmail.com},
#'
#' @date 2021/02/17
#' 

#-----------------Loading packages-------------------

pkgs <- c("tidyverse","missForest","parallel","here","tidymodels","ranger","caret")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))


#-----------------Loading all data---------------------

path = (here::here("data"))
setwd(path)
files <- list.files(here::here("data"))
lapply(files, load, envir=.GlobalEnv)

#-----------------Loading all functions---------------------

path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)

path = (here::here("analysis"))
setwd(path)
files.source = list.files(here::here("analysis"))
sapply(files.source, source)

FB_vars= FB_vars %>%
  select(-Env_1)

#------------------Running code------------------------

#IF YOUR DATA HAS NA IN IT, RUN MISSFOREST 
#Trying out missforest
test_missForest = missForest_test(FB_vars)

#Applying missforest
run_missForest = missForest_applied(FB_vars,0.6,test_missForest)

#ELSE GO DIRECTLY TO THIS STEP
#Splitting data with NA filled out by missForest or with original data with no NA
split = data_prep(run_missForest)

#Trying out IUCN predictions
test_IUCN = IUCN_test(split)

#Running IUCN predictions
run_IUCN = IUCN_predict(split,run_missForest)

#IUCN consensus (0.5 for this dummy dataset)
IUCN_final = IUCN_consensus(run_IUCN,length(split),80)

#------------------Loading outputs----------------------

path = (here::here("outputs"))
setwd(path)
files <- list.files(here::here("outputs"))
lapply(files, load, envir=.GlobalEnv)


#-----------------Creating Figures----------------------

#Figure of variable importance
#Saved in Figures folder
figure1 = var_imp(rel_inf)


