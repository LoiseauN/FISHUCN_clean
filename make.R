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

pkgs <- c("tidyverse","missForest","parallel","here","tidymodels","ranger","caret","tuneRanger")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#-----------------Loading all data---------------------

path = (here::here("data"))
setwd(path)
files <- list.files(here::here("data"),pattern = ".rds")
FB_vars = lapply(files, readRDS) %>% bind_rows()

#-----------------Loading all functions---------------------

path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)

path = (here::here("analysis"))
setwd(path)
files.source = list.files(here::here("analysis"))
sapply(files.source, source)

#-----------------Reproductibility---------------------
set.seed(42)

#------------------Running code------------------------

#PLEASE READ THIS : To run this, your data needs to be formatted as follows : 
#Species as rownames
#All traits as columns
#A IUCN column as column with IUCN status (CR, EN, VU, LC, NT) and NA for species with no IUCN information

#Convert IUCN data to T and NT 
FB_IUCN = IUCN_split(FB_vars)

#IF YOUR DATA HAS NA IN IT, RUN MISSFOREST, ELSE GO DIRECTLY TO DATA_PREP FUNCTION

#Trying out missforest
test_missForest = missForest_test(FB_IUCN)

#Applying missforest
#Virer les NA 
run_missForest = missForest_applied(FB_IUCN,0.6,test_missForest)

save(run_missForest, file = "/home/vfleure/Documents/FISHUCN_clean/Python/data.RData")
#HERE WE HAVE TO CALL PYTHON SCRIPT TO EXPORT FOR PYTHON

#Splitting data with NA filled out by missForest or with original data with no NA
split = data_prep(run_missForest)

#Trying out IUCN predictions
test_IUCN = IUCN_test(split,10)

#Running IUCN predictions
run_IUCN = IUCN_predict(split,run_missForest,10)

#IUCN consensus (0.5 for this dummy dataset)
IUCN_final = IUCN_consensus(run_IUCN,length(split),80)

#THEN CALL PYTHON SCRIPT TO GET CONSENSUS OF DEEP LEARNING

#THEN FINAL FUNCTION THAT MAKES CONSENSUS OF BOTH METHODS

#------------------Loading outputs----------------------

path = (here::here("outputs"))
setwd(path)
files <- list.files(here::here("outputs"))
lapply(files, load, envir=.GlobalEnv)


#-----------------Creating Figures----------------------

#Figure of variable importance
#Saved in Figures folder
figure1 = var_imp(rel_inf)


