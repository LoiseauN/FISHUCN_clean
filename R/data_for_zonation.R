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
#------------------Rescale Function----------------------

#Function to rescale proba
rescalex <- function(a,b,data){
  step1 <- data-min(data,na.rm=T)
  step2 <- b-a
  step3 <- max(data,na.rm=T)-min(data,na.rm=T)
  step4 <- (step1*step2)/step3
  res <- step4 + a
  return(res)}

#------------------Loading outputs----------------------
path = (here::here("outputs"))
setwd(path)
files <- list.files(here::here("outputs"))
lapply(files, load, envir=.GlobalEnv)

#Data from Deep
pred_deep <- read.csv2("res_inference_deep.csv",sep=",",header=T)
pred_deep <- subset(pred_deep,pred_deep$proba>=75)

#Data from machine
load("res_inference_machine.RData")
pred_mach <- subset(res_inference_machine,res_inference_machine$percentage>=80)

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
load("FB_vars.RData")

data_zonation <- data.frame(species = rownames(FB_vars),
                            IUCN_cat = FB_vars$IUCN) 

data_zonation$IUCN_alone <- NA

# ----------  Change 
for(i in 1:nrow(data_zonation)){ 
 
   print(i)
  
if(is.na(data_zonation$IUCN_cat[i])) data_zonation$IUCN_alone[i] <- NA  
else if(data_zonation$IUCN_cat[i] =="LC") data_zonation$IUCN_alone[i] <- "NThr"
else if(data_zonation$IUCN_cat[i] =="NT") data_zonation$IUCN_alone[i] <- "NThr"
else if(data_zonation$IUCN_cat[i] =="nt") data_zonation$IUCN_alone[i] <- "NThr"
else if(data_zonation$IUCN_cat[i] =="VU") data_zonation$IUCN_alone[i] <- "Thr"
else if(data_zonation$IUCN_cat[i] =="EN") data_zonation$IUCN_alone[i] <- "Thr"
else if(data_zonation$IUCN_cat[i] =="CR") data_zonation$IUCN_alone[i] <- "Thr"

}

# ----------  Change 
data_zonation <- merge(data_zonation,FISHUCN,  by = "species", all.x = T)
data_zonation$proba_select <- as.numeric(data_zonation$proba_select)/100

# Remove three line problem
data_zonation <- data_zonation[!data_zonation$species %in% c("1102","152","194"),]



  rescale_threat <-  subset(data_zonation, data_zonation$predict =="Thr" & is.na(data_zonation$IUCN_alone))
  rescale_non_threat <-  subset(data_zonation, data_zonation$predict =="NThr" & is.na(data_zonation$IUCN_alone))
  rescale_threat$proba_rescale <-  rescalex(a=2,b=5,data=rescale_threat$proba_select)
  rescale_non_threat$proba_rescale <-  rescalex(a=1,b=2,data=1-rescale_non_threat$proba_select)
  
  rescale_data <- data.frame(species = c(rescale_threat$species ,
                                         rescale_non_threat$species),
                            proba_rescale = c(rescale_threat$proba_rescale ,
                                              rescale_non_threat$proba_rescale))
  

  data_zonation <- merge(data_zonation,rescale_data, by = "species", all.x = T)

  
#--- Prepare for scenario  
  
  
  # SCENARIO 1: 
  # 1 for everyone
  data_zonation$scenario1_NoWeight <- 1
  data_zonation$scenario2_IUCNalone <- NA
  data_zonation$scenario3_IUCN_and_Predict <- NA
  
data_zonation$selected_species <- NA
data_zonation$selected_species_consensus <- NA

for(i in 1:nrow(data_zonation)){ 
  
  print(i)
  
  if(!is.na(data_zonation$IUCN_alone[i]) | !is.na(data_zonation$predict[i])) 
  data_zonation$selected_species[i] <- 1

  if(!is.na(data_zonation$IUCN_alone[i]) | !is.na(data_zonation$predict_consensus[i])) 
    data_zonation$selected_species_consensus[i] <- 1
  
}
  


data_zonation_list <- list()
#keep only predicted and species already in IUCN 
  data_zonation_list[[1]] <- subset(data_zonation,data_zonation$selected_species == 1)[,-c(11,17:18)]
  #keep only predicted and species already in IUCN with consensus
  data_zonation_list[[2]] <- subset(data_zonation,data_zonation$selected_species_consensus == 1)[,-c(10,17:18)]
  colnames(data_zonation_list[[2]])[10] <- "predict"
   #all species
  data_zonation_list[[3]] <- data_zonation[,-c(11,17:18)]
  
names(data_zonation_list) <- c("main", "sensibility1","sensibility2")
  


# SCENARIO 3  :  Weighted in function of the machine learning probability with negative influence for non threaten
# NA  by IUCN and predit = 2 (for SM 2 and 1)
# NonThreatened by IUCN = 1 or if both model AGREE
# NonThreatened predit = 2-probability
# Threatened by IUCN = 6
# Threatened predit = 2 + probability (range between 2 and 5)
# or 5 if both model AGREE


data_final_zonation <- lapply(1: length(data_zonation_list), function(x){
  
  data <- data_zonation_list[[x]]
  
  for(i in 1:nrow(data)){
   
    print(i)
    
    if(data$IUCN_alone[i] %in% "Thr"){
      data$scenario2_IUCNalone[i] <- 6
      data$scenario3_IUCN_and_Predict[i] <-  6 }
    
     else if (data$IUCN_alone[i] %in% "NThr"){
      data$scenario2_IUCNalone[i] <- 1
      data$scenario3_IUCN_and_Predict[i] <- 1
    }

    else{ 
    #Non information at all
    if(is.na(data$predict[i])){
      
      data$scenario2_IUCNalone[i] <- 2
      data$scenario3_IUCN_and_Predict[i] <- 2 # ou 1.5 
    
    }
    
      else if(data$predict[i] %in% "NThr"){
      
      #Non Threatened predict par model
      if (!data$agree[i]  %in% "AGREE"){ 
        data$scenario2_IUCNalone[i] <- 1
        data$scenario3_IUCN_and_Predict[i] <- data[data$species %in% data$species[i],]$proba_rescale
        
      }
      else { 
          
          data$scenario2_IUCNalone[i] <- 1
          data$scenario3_IUCN_and_Predict[i] <- 1
       }
    }
    
      else if(data$predict[i] %in% "Thr"){
      
      if (!data$agree[i]  %in% "AGREE"){ 
      
        data$scenario2_IUCNalone[i] <- 1
        data$scenario3_IUCN_and_Predict[i] <- data[data$species %in% data$species[i],]$proba_rescale
      
        } 
      
      else { 
          
          data$scenario2_IUCNalone[i] <- 1
          data$scenario3_IUCN_and_Predict[i] <- 5
          
        }
    }
    
  }
  }  
  return(data)
})
  
names(data_final_zonation) <- c("main", "sensibility1","sensibility2")
  boxplot(data_final_zonation[[1]]$scenario1_NoWeight,
          data_final_zonation[[1]]$scenario2_IUCNalone,
          data_final_zonation[[1]]$scenario3_IUCN_and_Predict)

  

save(data_final_zonation,file= here::here("outputs","data_final_zonation.RData"))




