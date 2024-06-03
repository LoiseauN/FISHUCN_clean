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
          "randomForest","caret","ROCR","RColorBrewer","tidyr",
          "RColorBrewer","rnaturalearth","rnaturalearthdata","torch","cito","segregation")

nip <- pkgs[!(pkgs %in% utils::installed.packages())]
nip <- lapply(nip, utils::install.packages, dependencies = TRUE)
ip  <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#devtools::install_github("zmjones/edarf", subdir = "pkg")
#devtools::install_github("iholzleitner/facefuns")
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


#------------------Running code------------------------
#Get IUCN status
IUCN_status <- get_iucn_status(FishDistribArea_all) # remove 2 species that are extinct
save(IUCN_status,file = "outputs/IUCN_status.RData")

IUCN_status_detailled <- get_iucn_status_detailled(FishDistribArea_all)
save(IUCN_status_detailled,file = "outputs/IUCN_status_detailled.RData")

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

species_traits  <-  species_traits[str_replace(rownames(species_traits), "-", "_")  %in% 
                                   FishDistribArea_all$species,]

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
  dplyr::rename(IUCN = "IUCN_status") %>% 
  mutate(IUCN = as.factor(IUCN))

  rownames(FB_final) <- FB_final[,1]
  FB_final <- FB_final[,-1]
  
#Sorting out some rows with only missing data
FB_final[FB_final==""] <-NA
FB_final <- FB_final[rowSums(is.na(FB_final)) != ncol(FB_final), ]

save(FB_final,file = "outputs/FB_final.Rdata")


FB_IUCN_more = FB_final
#Filling out genus and family where there is NA
FB_IUCN_taxo_nona <- fill_taxo(data = FB_IUCN_more)

#And now adding in to the final dataframe
FB_IUCN_temp = FB_IUCN_more %>% filter(!is.na(Genus))
FB_IUCN_final = rbind(FB_IUCN_temp,FB_IUCN_taxo_nona)

#still freshwater fish need to be clean
marine_families <- marine_families[marine_families$Marin_fresh == "M",]
FB_IUCN_final = FB_IUCN_final[FB_IUCN_final$Family %in% marine_families$Family,]
FB_IUCN_all_marine <- FB_IUCN_final

#Remove 3 trait of repro not informative enougt

FB_IUCN_final <- FB_IUCN_final[,!colnames(FB_IUCN_final) %in% c("ReproMode","Fertilization","RepGuild1")]

#Remove species with too much NA  (more than 40%)
FB_IUCN_final<- FB_IUCN_final[rowSums(is.na(FB_IUCN_final[,-ncol(FB_IUCN_final)])) < 0.6*(ncol(FB_IUCN_final)-1), ]
dim(FB_IUCN_final)

###Checking species that are not in FB_IUCN_final
dim(FB_IUCN_final)- dim(FB_IUCN_all_marine)
FB_nonselec <- FB_IUCN_all_marine[!rownames(FB_IUCN_all_marine) %in% rownames(FB_IUCN_final),]
save(FB_nonselec, file = here::here("outputs/FB_nonselec.Rdata"))

#FB_IUCN_final[!rownames(FB_IUCN_final) %in% rownames(FB_IUCN_all_marine),]
FB_nonselec_NS <- FB_nonselec[is.na(FB_nonselec$IUCN),]
FB_IUCN_final[rownames(FB_IUCN_final) %in% rownames(FB_nonselec_NS),]

#Prepare for fill missforest
FB_IUCN = IUCN_split(FB_IUCN_final)

#These variables we can't fill out : (Depth_min)R2 was not good enough 
#Depth max and trophic were not very good but not a great number of missing value
FB_IUCN_final = FB_IUCN_final %>% dplyr::select(-c(Depth_min))

#IF YOUR DATA HAS NA IN IT, RUN MISSFOREST, ELSE GO DIRECTLY TO DATA_PREP FUNCTION
#Trying out missforest
test_missForest = missForest_test(FB_IUCN,FB_IUCN_final)

#Applying missforest
data_noNA = missForest_applied(FB_IUCN_final,0.2,test_missForest) #(0.2 to keep trophic with only 25 value to predict,
# other are higher thant 0.6)
save(data_noNA, file = here::here("outputs/data_noNA.Rdata"))

###Checking species that are not in data_noNA
dim(FB_IUCN_all_marine) - dim(data_noNA)

#Splitting data with NA filled out by missForest or with original data with no NA
split = data_prep(data_noNA)


#TEST FOR IUCN PREDICTION AND CROSS VAL FOR DEEP AND RF
#Trying out IUCN predictions
test_IUCN = IUCN_test(data_splited_deep_RF,10)
#OLD test_IUCN = IUCN_test(split,10)
save(test_IUCN,file = here::here("outputs","test_IUCN.Rdata"))


#Give the accuracy ! 
performance_RF <- IUCN_performance_RF(test_IUCN,10)
plot_performance_RF(performance_RF)

metric_performance <- IUCN_metric_performance_RF(test_IUCN,10)
plot_metric_RF(metric_performance)

#Partial and importance variables
output_importance_pd <- IUCN_importance_pd(data_splited_deep_RF,data_noNA,10)
save(output_importance_pd,file = here::here("outputs","output_importance_pd.Rdata"))

#Running IUCN predictions RF
run_IUCN = IUCN_predict(data_splited_deep_RF,data_noNA,10)


#OLD run_IUCN = IUCN_predict(split,data_noNA,10)
save(run_IUCN,file = here::here("outputs/run_IUCN.Rdata"))

#Call outputs and keep prediction with 80% of model agree
#OLD IUCN_preds_machine_final = IUCN_machine(run_IUCN,length(split),80)
IUCN_preds_machine_final = IUCN_machine(run_IUCN,length(data_splited_deep_RF),80)

# Running IUCN predictions using deep learning

opt <- config_optimizer(type = "adagrad",
                        lr_decay = 1e-04,
                        weight_decay = 0.1,
                        verbose = TRUE)
#
scheduler <- config_lr_scheduler(type = "step",
                                 step_size = 30,
                                 gamma = 0.15,
                                 verbose = TRUE)


#Deep test
pred_deep_cito_test <- IUCN_deep_cito(data_splited_deep_RF,loop = 10)
save(pred_deep_cito_test,file = here::here("outputs/pred_deep_cito_test.RData"))

#Deep prediction
IUCN_preds_deep <- IUCN_predict_deep(data_splited_deep_RF,data_noNA,10)
save(IUCN_preds_deep,file = here::here("outputs/IUCN_preds_deep.RData"))

#Call outputs and keep prediction with 80% of model agree
load(file = here::here("outputs/IUCN_preds_deep.RData"))
IUCN_preds_deep_final <- IUCN_deep(data_predicted = IUCN_preds_deep,
                                   splits = length(data_splited_deep_RF),baseline = 80)

#THEN FINAL FUNCTION THAT MAKES COMPLEMENTARITY or CONSENSUS OF BOTH METHODS
all_predict <- IUCN_complementarity(IUCN_preds_machine_final,IUCN_preds_deep_final)
save(all_predict,file = "outputs/all_predict.Rdata")


#------------------Prepare outputs for the figure and zoantion------------------------

#Data for zonation
create_data_zonation(data = FB_IUCN_all_marine, data_predict = all_predict)

#Data for phylo and chord
dat_network <- create_dat_network()
dat_network[dat_network$species %in% rownames(FB_nonselec_NS),]
save(dat_network,file = "outputs/dat_network.Rdata")

#Protection and conservation
MPA_Protect <- protect_target(data = dat_network,
               mpa = PctMPAI_IV, 
               distrib = FishDistribArea_all) 
save(MPA_Protect,file = "outputs/MPA_Protect.Rdata")

#have some number 
number <- have_number(data = dat_network,  prediction = all_predict)

IUCN_status_detailled  <- IUCN_status_detailled[IUCN_status_detailled$species %in% dat_network$species,]
table(IUCN_status_detailled$IUCN_status)

#process output of zonation
Zrank_main <- process_out_zonation(nb_scenario = 2)
head(Zrank_main)

#merge all res
all_geo_res <- preparallRes(Zrank_main)
save(all_geo_res,file=here::here("outputs","all_geo_res.RData"))

#------------------Figure------------------------
#Figure 1 : barplot distribution IUCN cadtegories for different taxa
load(file =  here::here("outputs/FB_IUCN_all_marine.Rdata"))
figure1(data = FB_IUCN_all_marine)

#Figure 2 : Workflow

#Figure 3 : gain in categories
chid_chord(sup = FALSE)

#For Supp
chid_chord(sup = TRUE)

#Figure 4 
figure4(data =  output_importance_pd)
  
#Figure 5

#Figure 6
figure6(data = MPA_Protect)

#to test because take time data <- all_geo_res[sample(c(1:nrow(all_geo_res)), 100000, replace = TRUE),]
figRank(data = all_geo_res, sup = FALSE)
#For Supp
figRank(data = all_geo_res, sup = TRUE)
