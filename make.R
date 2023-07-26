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
set.seed(666)

## Installing/Loading packages ----

pkgs <- c("arm", "beepr", "caper", "caret", "cluster", "doParallel", "dplyr", 
           "ggalluvial", "ggplot2", "ggstatsplot", "grid", "gridExtra", "here", 
           "hrbrthemes", "missForest", "palmerpenguins", "parallel", 
           "pbmcapply", "plyr", "ranger", "raster", "RCurl", "rfishbase", 
           "rphylopic", "rredlist", "scales", "stringr", "taxize", "tidymodels",
           "tidyverse", "viridis", "XML", "circlize","edarf","rgdal",
          "sp","rgeos","sf","wesanderson","ggpubr", "harrypotter",
          "randomForest","caret","ROCR","RColorBrewer")

nip <- pkgs[!(pkgs %in% utils::installed.packages())]
nip <- lapply(nip, utils::install.packages, dependencies = TRUE)
ip  <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#devtools::install_github("zmjones/edarf", subdir = "pkg")
## Loading all functions ----

files.source <- list.files(here::here("R"), pattern = "\\.R$", 
                           full.names = TRUE)
sapply(files.source,source,.GlobalEnv)

## Loading all data ----

files   <- list.files(here::here("data"), pattern = "\\.rds$", 
                      full.names = TRUE)

files     <- list.files(here::here("data"), pattern = "\\.RData$", 
                        full.names = TRUE)
data_list <- lapply(files, load, .GlobalEnv)


#files     <- list.files(here::here("outputs"), pattern = "\\.RData$|\\.Rdata$",
#                        full.names = TRUE)
#data_list <- lapply(files, load, .GlobalEnv)


#TO CHECK HERE .Rdata for IUCN_status


#------------------Running code------------------------
#Get IUCN status
IUCN_status <- get_iucn_status(FishDistribArea_all) # remove 2 species that are extinct
save(IUCN_status,file = "outputs/IUCN_status.RData")

#Scrap Data from Fishbase
species_traits = FB_scrap()

#Save species_traits
save(species_traits,file = here::here("outputs/species_traits.RData"))

#Remove trophic_level from the webscrapping because only NA. Fill after in dataPrep
species_traits = species_traits %>% dplyr::select(-Trophic_Level)

#remove(freshwaterfish)
species_traits <- species_traits[!species_traits$Env_1 %in% c("Freshwater_brackish","Freshwater"),]

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

IUCN_status$species <- gsub("-","_",IUCN_status$species)

#Get IUCN status
FB_final <- FB_vars %>% left_join(IUCN_status,by='species') %>% 
  dplyr::rename(IUCN = "IUCN_status")

  rownames(FB_final) <- FB_final[,1]
  FB_final <- FB_final[,-1]
  
  
#SOME TRANSFORMATION TO INCLUDE IN ONE OF THE FUNCTIONS
#FB_final$Common_length = as.numeric(FB_final$Common_length)
FB_final$IUCN = as.factor(FB_final$IUCN)

#Sorting out some species with only missing data
FB_final[FB_final==""] <-NA
FB_final <- FB_final[rowSums(is.na(FB_final)) != ncol(FB_final), ]

save(FB_final,file = "outputs/FB_final.Rdata")
dim(FB_final)


FB_IUCN_more = FB_final
#last check
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
save(taxo,file = here::here("outputs", "taxo.RData"))

taxo <- unique(taxo)
FB_IUCN_taxo_nona <- FB_IUCN_taxo_na %>% left_join(taxo,by="Genus")
rownames(FB_IUCN_taxo_nona) <- rownames(FB_IUCN_taxo_na)

FB_IUCN_taxo_nona = FB_IUCN_taxo_nona[!duplicated(FB_IUCN_taxo_nona), ]

#Manually filling out the rest
FB_IUCN_taxo_nona["Bembrops_greyae","Family"] = "Percophidae"
FB_IUCN_taxo_nona["Verilus_anomalus","Family"] = "Acropomatidae"
FB_IUCN_taxo_nona["Lissonanchus_lusherae","Family"] = "Gobiesocidae"
FB_IUCN_taxo_nona["Morwong_fuscus","Family"] = "Cheilodactylidae"
FB_IUCN_taxo_nona["Pseudogoniistius_nigripes","Family"] = "Cheilodactylidae"
FB_IUCN_taxo_nona["Eques_lanceolatus","Family"] = "Sciaenidae"
FB_IUCN_taxo_nona["Pteropelor_noronhai","Family"] = "Scorpaenidae"

#And now adding in to the final dataframe
FB_IUCN_temp = FB_IUCN_more %>% filter(!is.na(Genus))
FB_IUCN_final = rbind(FB_IUCN_temp,FB_IUCN_taxo_nona)

#still freshwater fish need to be clean
marine_families <- marine_families[marine_families$Marin_fresh == "M",]
FB_IUCN_final = FB_IUCN_final[FB_IUCN_final$Family %in% marine_families$Family,]

sapply(FB_IUCN_final, function(y) sum(length(which(is.na(y)))))

#Prepare for fill missforest
FB_IUCN = IUCN_split(FB_IUCN_final)


#IF YOUR DATA HAS NA IN IT, RUN MISSFOREST, ELSE GO DIRECTLY TO DATA_PREP FUNCTION
#Trying out missforest
test_missForest = missForest_test(FB_IUCN,FB_IUCN_final)

#These variables we can't fill out : (Depth_min)R2 was not good enough 
#Depth max and trophic were not very good but not a great number of missing value
#FB_IUCN_more = FB_IUCN %>% dplyr::select(-c(Depth_min))

#Applying missforest
data_noNA = missForest_applied(FB_IUCN_final,0.3,test_missForest)
save(data_noNA, file = here::here("outputs/data_noNA.Rdata"))

###Checking species that are not in data_noNA
dim(FB_IUCN_final) - dim(data_noNA)
FB_nonselec <-FB_IUCN_final[!rownames(FB_IUCN_final) %in% rownames(data_noNA),]
FB_nonselec <-FB_nonselec[is.na(FB_nonselec$IUCN),]

#Splitting data with NA filled out by missForest or with original data with no NA
split = data_prep(data_noNA)

#Clean name of the variables 



#Trying out IUCN predictions
test_IUCN = IUCN_test(split,10)
#Give the accuracy ! 

# Plot importance plot
importance_plot = var_imp(test_IUCN[[1]])  

#Running IUCN predictions
run_IUCN = IUCN_predict(split,data_noNA,10)
save(run_IUCN,file = "outputs/run_IUCN.Rdata")

#Call outputs and keep prediction with 80% of model agree
IUCN_preds_machine_final = IUCN_machine(run_IUCN,length(split),80)

# Running IUCN predictions using deep learning
IUCN_deep_predict()

#Call outputs and keep prediction with 80% of model agree
load(file = here::here("outputs/IUCN_preds_deep.RData"))
IUCN_preds_deep_final = IUCN_deep(IUCN_preds_deep,80)
IUCN_preds_deep_final[IUCN_preds_deep_final=="NaN"] <- NA

#THEN FINAL FUNCTION THAT MAKES COMPLEMENTARITY OF BOTH METHODS
all_predict <- IUCN_complementarity(IUCN_preds_machine_final,IUCN_preds_deep_final)
save(all_predict,file = "outputs/all_predict.Rdata")


#TO CORRECT THEN  FUNCTION THAT MAKES CONSENSUS OF BOTH METHODS FOR SUPPLEMENTARY
all_predict_sup <- IUCN_consensus(IUCN_preds_machine_final,IUCN_preds_deep_final)
save(all_predict_sup,file = "outputs/all_predict_sup.Rdata")

create_data_zonation(data = FB_final, data_predict = all_predict)

  

  #------------------Figure------------------------
#Figure 2 
chid_chord(data_zonation, sup = FALSE)
#For Supp
chid_chord(data_zonation, sup = TRUE)

#to test because take time data <- all_geo_res[sample(c(1:nrow(all_geo_res)), 100000, replace = TRUE),]
figRank(data = all_geo_res, sup = FALSE)
#For Supp
figRank(data = all_geo_res, sup = TRUE)
