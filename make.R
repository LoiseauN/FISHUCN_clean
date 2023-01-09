#' Run the Entire Project
#'
#' This script reproduces all analyses and figures of the ___________ article.
#'
#' @author  Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com},
#'          Raphaël SEGUIN, \email{raphael.seguin46@@gmail.com},
#'          Valentine FLEURE, \email{valentine.fleure@@gmail.com},
#'
#' @date 2021/02/17
#' 



## Parameters ----
rm(list=ls())
set.seed(42)



## Installing/Loading packages ----

pkgs <- c("arm", "beepr", "caper", "caret", "cluster", "doParallel", "dplyr", 
           "ggalluvial", "ggplot2", "ggstatsplot", "grid", "gridExtra", "here", 
           "hrbrthemes", "missForest", "palmerpenguins", "parallel", 
           "pbmcapply", "plyr", "ranger", "raster", "RCurl", "rfishbase", 
           "rphylopic", "rredlist", "scales", "stringr", "taxize", "tidymodels",
           "tidyverse", "viridis", "XML", "circlize","edarf","rgdal",
          "sp","rgeos","sf")

nip <- pkgs[!(pkgs %in% utils::installed.packages())]
nip <- lapply(nip, utils::install.packages, dependencies = TRUE)
ip  <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

devtools::install_github("zmjones/edarf", subdir = "pkg")
## Loading all functions ----

files.source <- list.files(here::here("R"), pattern = "\\.R$", 
                           full.names = TRUE)
invisible(sapply(files.source[-14], source)) ##### pcoaFig.R is not a function

## Loading all data ----

files   <- list.files(here::here("data"), pattern = "\\.rds$", 
                      full.names = TRUE)
FB_vars <- lapply(files, readRDS) %>% 
  dplyr::bind_rows()


files     <- list.files(here::here("data"), pattern = "\\.RData$", 
                        full.names = TRUE)
data_list <- lapply(files, load, .GlobalEnv)


files     <- list.files(here::here("outputs"), pattern = "\\.RData$|\\.Rdata$",
                        full.names = TRUE)
data_list <- lapply(files, load, .GlobalEnv)


#TO CHECK HERE .Rdata for IUCN_status


#------------------Running code------------------------
#Scrap Data from Fishbase
species_traits = FB_scrap()

species_traits = species_traits %>% dplyr::select(-Trophic_Level)

#remove(freshwaterfish)
species_traits <- species_traits[species_traits$Env_1 %in% c("Marine","Marine_brackish"),]

#Save species_traits
save(species_traits,file = here::here("outputs/species_traits.RData"))


## Removing freshwater ----
FishDistribArea_all <-  FishDistribArea_all[FishDistribArea_all$species %in% 
                                              str_replace(rownames(species_traits), "-", "_"),]

#Selecting variables of interest
FB_scrapped = prep_data(FishDistribArea_all,species_traits,FamilyElasmo)
save(FB_scrapped,file = here::here("outputs/FB_scrapped.RData"))


#PLEASE READ THIS : To run this, your data needs to be formatted as follows : 
#Species as rownames
#All traits as columns

FB_vars = FB_scrapped %>%   
  left_join(DepthRange,by="species") %>% 
  mutate(Depth_min = log10(Depth_min+1),
         Depth_max = log10(Depth_max+1))

#Get IUCN status
IUCN_status <- get_iucn_status(IUCN_status)
save(IUCN_status,file = "outputs/IUCN_status.RData")

IUCN_status$species <- gsub("-","_",IUCN_status$species)

#Get IUCN status
FB_final <- FB_vars %>% left_join(IUCN_status,by='species') %>% 
  dplyr::rename(IUCN = "IUCN_status")%>%
 column_to_rownames("species")



#SOME TRANSFORMATION TO INCLUDE IN ONE OF THE FUNCTIONS
#FB_final$Common_length = as.numeric(FB_final$Common_length)
FB_final$IUCN = as.factor(FB_final$IUCN)

#Sorting out some problems with data 

FB_final[FB_final==""]<-NA
FB_final <- FB_final[rowSums(is.na(FB_final)) != 23, ]

save(FB_final,file = "outputs/FB_final.Rdata")

###########
#TEST
#FB_final <- FB_final[,c("Max_length","Env_2","Troph","Genus",
#                        "Family","BodyShapeI","Aquarium","K","Depth_min","Depth_max","IUCN")]
###########
#Convert IUCN data to T and NT 
FB_IUCN = IUCN_split(FB_final)
#FB_IUCN <- FB_IUCN[,-22]
#FB_vars <- FB_vars[,-22]
dim(FB_final)



#IF YOUR DATA HAS NA IN IT, RUN MISSFOREST, ELSE GO DIRECTLY TO DATA_PREP FUNCTION
#Trying out missforest
#HERE ADD OPTION THAT DELETES TEMPORARILY THE VARIABLES IF THEY HAVE TOO MANY NAs
test_missForest = missForest_test(FB_IUCN,FB_final)

#These variables we can't fill out : (Depth_max and)R2 was not good enough 
FB_IUCN_more = FB_IUCN %>% dplyr::select(-c(Depth_min,Depth_max,Troph))
FB_IUCN_more = FB_IUCN_more[!rowSums(is.na(FB_IUCN_more)) == ncol(FB_IUCN_more), ] 

#BIT OF CODE TO FILL OUT TAXONOMIC NA 

#Filling out genus and family where there is NA
FB_IUCN_taxo_na = FB_IUCN_more %>% filter(is.na(Genus)) 

#Add Genus and calculate the rest with taxize
FB_IUCN_taxo_na = FB_IUCN_taxo_na %>% mutate(Genus = sub("\\_.*", "", rownames(FB_IUCN_taxo_na))) %>% dplyr::select(-Family)


taxo =  classification(FB_IUCN_taxo_na$Genus, db = "ncbi") %>% 
  rbind() %>% 
  filter(rank == "family")%>% 
  dplyr::select(name, query) %>% 
  dplyr::rename(Family = "name",
                Genus = "query")

taxo <- unique(taxo)
FB_IUCN_taxo_nona <- FB_IUCN_taxo_na %>% left_join(taxo,by="Genus")
rownames(FB_IUCN_taxo_nona) <- rownames(FB_IUCN_taxo_na)

FB_IUCN_taxo_nona = FB_IUCN_taxo_nona[!duplicated(FB_IUCN_taxo_nona), ]

#Manually filling out the rest
FB_IUCN_taxo_nona["Bembrops_greyae",13] = "Percophidae"
FB_IUCN_taxo_nona["Verilus_anomalus",13] = "Acropomatidae"
FB_IUCN_taxo_nona["Lissonanchus_lusherae",13] = "Gobiesocidae"
FB_IUCN_taxo_nona["Morwong_fuscus",13] = "Cheilodactylidae"
FB_IUCN_taxo_nona["Pseudogoniistius_nigripes",13] = "Cheilodactylidae"
FB_IUCN_taxo_nona["Eques_lanceolatus",13] = "Sciaenidae"
FB_IUCN_taxo_nona["Pteropelor_noronhai",13] = "Scorpaenidae"



#And now adding in to the final dataframe
FB_IUCN_temp = FB_IUCN_more %>% filter(!is.na(Genus))

FB_IUCN_final = rbind(FB_IUCN_temp,FB_IUCN_taxo_nona)


#Applying missforest
data_noNA = missForest_applied(FB_IUCN_final,0.55,test_missForest)
save(data_noNA, file = here::here("outputs/data_noNA.Rdata"))

###Checking species that are not in data_noNA

dim(FB_final) - dim(data_noNA)
FB_nonselec <-FB_final[!rownames(FB_final) %in% rownames(data_noNA),]
FB_nonselec <-FB_nonselec[is.na(FB_nonselec$IUCN),]



#Splitting data with NA filled out by missForest or with original data with no NA
split = data_prep(data_noNA)

#Trying out IUCN predictions
test_IUCN = IUCN_test(split,10)

#Running IUCN predictions
run_IUCN = IUCN_predict(split,data_noNA,10)
save(run_IUCN,file = "outputs/run_IUCN.Rdata")

#IUCN consensus (0.5 for this dummy dataset)
IUCN_preds_machine_final = IUCN_machine(run_IUCN,length(split),75)

# Running IUCN predictions using deep learning
IUCN_deep_predict()

#THEN CALL PYTHON SCRIPT TO GET CONSENSUS OF DEEP LEARNING
IUCN_preds_deep_final = IUCN_deep(IUCN_preds_deep,75)
IUCN_preds_deep_final[IUCN_preds_deep_final=="NaN"] <- NA
#THEN FINAL FUNCTION THAT MAKES COMPLEMENTARITY OF BOTH METHODS
all_predict <- IUCN_complementarity(IUCN_preds_machine_final,IUCN_preds_deep_final)



save(all_predict,file = "outputs/all_predict.Rdata")



#THEN  FUNCTION THAT MAKES CONSENSUS OF BOTH METHODS FOR SUPPLEMENTARY




