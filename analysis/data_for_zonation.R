#' Prepare data for zonation
#'
#' Choice of the thresold and merging output from deep and Machine
#' 
#' 
#' @return A list with each downsampled dataframe from your data
#'
#' @export
#' 
#' 
#' 


#------------------Loading outputs----------------------
path = (here::here("outputs"))
setwd(path)
files <- list.files(here::here("outputs"))
lapply(files, load, envir=.GlobalEnv)

#Data from Deep
#pred_deep <- read.csv2("res_inference_deep.csv",sep=",",header=T)
#pred_deep <- subset(pred_deep,pred_deep$proba>=80)

#Data from machine
#load("res_inference_machine.RData")
#pred_mach <- subset(res_inference_machine,res_inference_machine$percentage>=80)


#Merge Data
FISHUCN <- merge(pred_deep,pred_mach,by="species",all=T)
colnames(FISHUCN) <- c("species", "IUCN_deep","proba", "IUCN_machine","n","percentage")

#Highlight consensus or not between deeplearing and machine learning
FISHUCN$agree <- NA
for (i in 1:nrow(FISHUCN)){
  print(i)
  if(is.na(FISHUCN$IUCN_deep[i]) & !is.na(FISHUCN$IUCN_machine[i])) {FISHUCN$agree[i] <- "ONLY_MACHINE"}
  
  else if(!is.na(FISHUCN$IUCN_deep[i]) & is.na(FISHUCN$IUCN_machine[i])){FISHUCN$agree[i] <- "ONLY_DEEP"}
  
  else if(FISHUCN$IUCN_deep[i]==FISHUCN$IUCN_machine[i]){FISHUCN$agree[i] <- "AGREE"}
  
  else{FISHUCN$agree[i] <- "NOT AGREE"}
  
}


#Predict with the complementary approach
FISHUCN$predict <- NA
for (i in 1:nrow(FISHUCN)){
  print(i)
  
  if(FISHUCN$agree[i] == "ONLY_MACHINE") {FISHUCN$predict[i] <- as.character(FISHUCN$IUCN_machine[i])}
  
  if(FISHUCN$agree[i] == "ONLY_DEEP"){FISHUCN$predict[i] <- as.character(FISHUCN$IUCN_deep[i])}
  
  if(FISHUCN$agree[i] == "AGREE"){FISHUCN$predict[i] <- as.character(FISHUCN$IUCN_machine[i])}
  
  if(FISHUCN$agree[i] == "NOT AGREE"){FISHUCN$predict[i] <- NA}
  
}


#Predict with the consensus
FISHUCN$predict_consensus <- NA
for (i in 1:nrow(FISHUCN)){
  print(i)
  
  if(FISHUCN$agree[i] == "ONLY_MACHINE") {FISHUCN$predict_consensus[i] <- NA}
  
  if(FISHUCN$agree[i] == "ONLY_DEEP"){FISHUCN$predict_consensus[i] <- NA}
  
  if(FISHUCN$agree[i] == "AGREE"){FISHUCN$predict_consensus[i] <- as.character(FISHUCN$IUCN_machine[i])}
  
  if(FISHUCN$agree[i] == "NOT AGREE"){FISHUCN$predict_consensus[i] <- NA}
  
}

# Take the 
FISHUCN$proba_select <- NA 

for (i in 1:nrow(FISHUCN)){
  print(i)
  
  if(FISHUCN$agree[i] == "ONLY_MACHINE") {FISHUCN$proba_select[i] <- FISHUCN$percentage[i]}
  
  if(FISHUCN$agree[i] == "ONLY_DEEP"){FISHUCN$proba_select[i] <- FISHUCN$proba[i]}
  
  #Feel after
  if(FISHUCN$agree[i] == "AGREE"){FISHUCN$proba_select[i] <- NA}
  
  if(FISHUCN$agree[i] == "NOT AGREE"){FISHUCN$proba_select[i] <- NA}
  
}



save(FISHUCN,file="FISHUCN.RData")



#Last file to send in zonation

data_zonation <- data.frame(species = rownames(FB_final),
                            IUCN_cat = FB_final$IUCN) 

#data_zonation$IUCN_alone <- NA

# ----------  Change 
#for(i in 1:nrow(data_zonation)){ 
 
#   print(i)
  
#if(is.na(data_zonation$IUCN_cat[i])) data_zonation$IUCN_alone[i] <- NA  
#else if(data_zonation$IUCN_cat[i] =="LC") data_zonation$IUCN_alone[i] <- "NThr"
#else if(data_zonation$IUCN_cat[i] =="NT") data_zonation$IUCN_alone[i] <- "NThr"
#else if(data_zonation$IUCN_cat[i] =="nt") data_zonation$IUCN_alone[i] <- "NThr"
#else if(data_zonation$IUCN_cat[i] =="VU") data_zonation$IUCN_alone[i] <- "Thr"
#else if(data_zonation$IUCN_cat[i] =="EN") data_zonation$IUCN_alone[i] <- "Thr"
#else if(data_zonation$IUCN_cat[i] =="CR") data_zonation$IUCN_alone[i] <- "Thr"

#}

# ----------  Change 
data_zonation<- data_zonation %>%
  left_join(all_predict,by="species")
data_zonation$proba_select <- as.numeric(data_zonation$proba_select)/100





rescale_threat <-  subset(data_zonation, data_zonation$predict_complementary =="Thr" & is.na(data_zonation$IUCN_cat))
rescale_non_threat <-  subset(data_zonation, data_zonation$predict_complementary =="NThr" & is.na(data_zonation$IUCN_cat))
rescale_threat$proba_rescale <-  rescalex(a=2,b=5,data=rescale_threat$proba_select)
rescale_non_threat$proba_rescale <-  rescalex(a=1,b=2,data=1-rescale_non_threat$proba_select)
  
  rescale_data <- data.frame(species = c(rescale_threat$species ,
                                         rescale_non_threat$species),
                            proba_rescale = c(rescale_threat$proba_rescale ,
                                              rescale_non_threat$proba_rescale))
  
  data_zonation<- data_zonation %>%
    left_join(rescale_data,by="species")


  
#--- Prepare for scenario  
  
data_zonation$selected_species_IUCNonly <- NA
data_zonation$selected_species_complementary_W_IUCN <- NA
data_zonation$selected_species_consensus_W_IUCN <- NA

for(i in 1:nrow(data_zonation)){ 
  
 print(i)
  
if(!is.na(data_zonation$IUCN_cat[i]) ) data_zonation$selected_species_IUCNonly[i] <- 1

  if(!is.na(data_zonation$IUCN_cat[i]) | !is.na(data_zonation$predict_complementary[i])) 
  data_zonation$selected_species_complementary_W_IUCN[i] <- 1
 

  if(!is.na(data_zonation$IUCN_cat[i]) | !is.na(data_zonation$predict_consensus[i])) 
    data_zonation$selected_species_consensus_W_IUCN[i] <- 1
  }
  



data_zonation$weight_zonation_IUCN_and_Predict <- NA
data_zonation$weight_zonation_IUCN_alone <- NA
data_zonation$scenario1_NoWeight <- 1



# SCENARIO 1: 
# 1 for everyone


# SCENARIO 2  :  Weighted in function of the machine learning probability with negative influence for non threaten
# NA  by IUCN and predit = 2 (for SM 2 and 1)
# NonThreatened by IUCN  = 1 
# NonThreatened predit = 2-probability
# Threatened by IUCN = 6
# Threatened predit = 2 + probability (range between 2 and 5)


#data_final_zonation <- lapply(1: length(data_zonation_list), function(x){
  
  for(i in 1:nrow(data_zonation)){
   
    print(i)
    
    if(data_zonation$IUCN_cat[i] %in% "Thr"){
      data_zonation$weight_zonation_IUCN_alone[i] <- 6
      data_zonation$weight_zonation_IUCN_and_Predict[i] <-  6  
      }

     else if (data_zonation$IUCN_cat[i] %in% "NThr"){
       data_zonation$weight_zonation_IUCN_alone[i] <- 1
       data_zonation$weight_zonation_IUCN_and_Predict[i] <- 1
      }

    else{ 
    #Non information at all
    if(is.na(data_zonation$predict_complementary[i])){
      
      data_zonation$weight_zonation_IUCN_alone[i] <- 2
      data_zonation$weight_zonation_IUCN_and_Predict[i] <- 2 # ou 1.5 
      
    }
    
      else if(data_zonation$predict_complementary[i] %in% "NThr"){
      
      #Non Threatened predict par model
        data_zonation$weight_zonation_IUCN_alone[i] <- 1
        data_zonation$weight_zonation_IUCN_and_Predict[i] <- data_zonation[data_zonation$species %in% data_zonation$species[i],]$proba_rescale
    }
    
      else if(data_zonation$predict_complementary[i] %in% "Thr"){
      
        data_zonation$weight_zonation_IUCN_alone[i] <- 1
        data_zonation$weight_zonation_IUCN_and_Predict[i] <- data_zonation[data_zonation$species %in% data_zonation$species[i],]$proba_rescale

         }
    
      }
    
    }  
  
    
 boxplot(data_zonation$scenario1_NoWeight,
        data_zonation$weight_zonation_IUCN_alone,
        data_zonation$weight_zonation_IUCN_and_Predict)


 test <- subset(data_zonation,data_zonation$selected_species_IUCNonly==1)
 boxplot(test$scenario1_NoWeight,
         test$weight_zonation_IUCN_alone,
         test$weight_zonation_IUCN_and_Predict)
 
 test <- subset(data_zonation,data_zonation$selected_species_complementary_W_IUCN==1)
 boxplot(test$scenario1_NoWeight,
         test$weight_zonation_IUCN_alone,
         test$weight_zonation_IUCN_and_Predict)
 
  

save(data_zonation,file= here::here("outputs","data_zonation.RData"))




