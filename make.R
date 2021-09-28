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

pkgs <- c("tidyverse","missForest","parallel","here","tidymodels",
          "ranger","caret","tuneRanger","smoof","caper","RCurl","XML","tidyverse",
          "pbmcapply","doParallel","rfishbase","beepr","arm")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))


#-----------------Loading all data---------------------

path = (here::here("data"))
setwd(path)
files <- list.files(here::here("data"),pattern = ".rds")
FB_vars = lapply(files, readRDS) %>% bind_rows()

path = (here::here("data"))
setwd(path)
files <- list.files(here::here("data"),pattern = ".RData")
data_list = lapply(files, load, .GlobalEnv)

path = (here::here("outputs"))
setwd(path)
files <- list.files(here::here("outputs"),pattern = ".RData")
data_list = lapply(files, load, .GlobalEnv)

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

setwd(here::here())

#Scrap Data from Fishbase
species_traits = FB_scrap()

species_traits = species_traits %>% dplyr::select(-Trophic_Level)

#ADD SAVE HERE
save(species_traits,file = "outputs/species_traits.RData")

#Selecting variables of interest
FB_scrapped = prep_data(FishDistribArea_all,species_traits,FamilyElasmo)

#PLEASE READ THIS : To run this, your data needs to be formatted as follows : 
#Species as rownames
#All traits as columns

FB_vars = FB_scrapped %>% dplyr::select(-c(Importance,CommonLength)) #LongevityWild

#Get IUCN status
#IUCN_status <- get_iucn_status(FB_vars)
#save(IUCN_status,file = "outputs/IUCN_status.RData")


load("outputs/IUCN_status.Rdata")
IUCN_status$species <- gsub("-","_",IUCN_status$species)

#Get IUCN status
FB_final <- FB_vars %>% left_join(IUCN_status,by='species') %>% dplyr::rename(IUCN = "IUCN_status") %>% column_to_rownames("species")

#SOME TRANSFORMATION TO INCLUDE IN ONE OF THE FUNCTIONS
FB_final$Common_length = as.numeric(FB_final$Common_length)
FB_final$IUCN = as.factor(FB_final$IUCN)

#Convert IUCN data to T and NT 
FB_IUCN = IUCN_split(FB_final)
#FB_IUCN <- FB_IUCN[,-22]
#FB_vars <- FB_vars[,-22]



#IF YOUR DATA HAS NA IN IT, RUN MISSFOREST, ELSE GO DIRECTLY TO DATA_PREP FUNCTION
#Trying out missforest
#HERE ADD OPTION THAT DELETES TEMPORARILY THE VARIABLES IF THEY HAVE TOO MANY NAs
test_missForest = missForest_test(FB_IUCN,FB_final)

#Applying missforest
run_missForest = missForest_applied(FB_IUCN,0.6,test_missForest)

save(run_missForest, file = "outputs/data_noNA.Rdata")

# #HERE ADD FUNCTION TO SELECT VARIABLES
# data_model = run_missForest %>%
#   dplyr::select()

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


