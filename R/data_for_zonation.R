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

#Data from Deep
pred_deep <- read.csv2("res_inference_deep.csv",sep=",",header=T)
pred_deep <- subset(pred_deep,pred_deep$proba>=75)

#Data from machine
load("res_inference_machine.RData")
pred_mach <- subset(res_inference_machine,res_inference_machine$percentage>=75)

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
  
  if(FISHUCN$agree[i] == "NOT AGREE"){FISHUCN$predict[i] <- "NA"}
  
}


#Predict with the consensus
FISHUCN$predict_consensus <- NA
for (i in 1:nrow(FISHUCN)){
  print(i)
  
  if(FISHUCN$agree[i] == "ONLY_MACHINE") {FISHUCN$predict_consensus[i] <- "NA"}
  
  if(FISHUCN$agree[i] == "ONLY_DEEP"){FISHUCN$predict_consensus[i] <- "NA"}
  
  if(FISHUCN$agree[i] == "AGREE"){FISHUCN$predict_consensus[i] <- as.character(FISHUCN$IUCN_machine[i])}
  
  if(FISHUCN$agree[i] == "NOT AGREE"){FISHUCN$predict_consensus[i] <- "NA"}
  
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

data_zonation <- data.frame(row.names = rownames(FB_vars),
                            IUCN = FB_vars$IUCN) 

data_zonation$IUCN_alone <- NA

# ----------  Change 
for(i in 1:nrow(data_zonation)){ 
 
   print(i)
  
if(is.na(data_zonation$IUCN[i])) data_zonation$IUCN_alone[i] <- NA  
else if(data_zonation$IUCN[i] =="LC") data_zonation$IUCN_alone[i] <- "NThr"
else if(data_zonation$IUCN[i] =="NT") data_zonation$IUCN_alone[i] <- "NThr"
else if(data_zonation$IUCN[i] =="nt") data_zonation$IUCN_alone[i] <- "NThr"
else if(data_zonation$IUCN[i] =="VU") data_zonation$IUCN_alone[i] <- "Thr"
else if(data_zonation$IUCN[i] =="EN") data_zonation$IUCN_alone[i] <- "Thr"
else if(data_zonation$IUCN[i] =="CR") data_zonation$IUCN_alone[i] <- "Thr"

}




#Function to rescale proba
rescalex <- function(a,b,data){
  step1 <- data-min(data,na.rm=T)
  step2 <- b-a
  step3 <- max(data,na.rm=T)-min(data,na.rm=T)
  step4 <- (step1*step2)/step3
  res <- step4 + a
  return(res)}


#Rescale prob for threatened predict 
rescale_threat <-  subset(data_zonation, data_zonation$IUCN_last_pred =="Threatened"& is.na(data_zonation$IUCN_alone))
rescale_non_threat <-  subset(data_zonation, data_zonation$IUCN_last_pred =="NonThreatened"& is.na(data_zonation$IUCN_alone))

rescale_threat$C <-  rescalex(a=2,b=5,data=rescale_threat$C)
rescale_non_threat$NC <-  rescalex(a=1,b=2,data=1-rescale_non_threat$NC)
# SCENARIO 1: 
# 1 for everyone

# SCENARIO 3  :  Weighted in function of the machine learning probability with negative influence for non threaten
# NA  by IUCN and predit = 1.5 (for SM 2 and 1)
# NonThreatened by IUCN = 1
# NonThreatened predit = 2-probability
# Threatened by IUCN = 5
# Threatened predit = 2 + probability (range between 2 and 5)
data_zonation$scenario1_NoWeight <- 1
data_zonation$scenario2_IUCNalone <- NA
data_zonation$scenario3_IUCN_and_Predic <- NA

for(i in 1:nrow(data_zonation)){
  print(i)
  
  #Non information at all
  if(is.na(data_zonation$IUCN_last_pred[i])){
    
    data_zonation$scenario2_IUCNalone[i] <- 1
    data_zonation$scenario3_IUCN_and_Predic[i] <- 1 # ou 1.5 
  }
  
  if(data_zonation$IUCN_last_pred[i] %in% "NonThreatened"){
    
    #Non Threatened predict par model
    if(is.na(data_zonation$IUCN_alone[i])){ 
      
      data_zonation$scenario2_IUCNalone[i] <- 1
      data_zonation$scenario3_IUCN_and_Predic[i] <- rescale_non_threat[rescale_non_threat$species %in% data_zonation$species[i],]$NC
      
    }else{
      data_zonation$scenario2_IUCNalone[i] <- 1
      data_zonation$scenario3_IUCN_and_Predic[i] <- 1
      
    }
  }
  
  if(data_zonation$IUCN_last_pred[i] %in% "Threatened"){
    
    if(is.na(data_zonation$IUCN_alone[i])){ 
      
      data_zonation$scenario2_IUCNalone[i] <- 1
      data_zonation$scenario3_IUCN_and_Predic[i] <- rescale_threat[rescale_threat$species %in% data_zonation$species[i],]$C
      
      
    }else{
      
      data_zonation$scenario2_IUCNalone[i] <- 5
      data_zonation$scenario3_IUCN_and_Predic[i] <-  5
      
    }
  }
  
}


data_zonation_reduced <- data_zonation[,c("species",
                                          "scenario1_NoWeight",
                                          "scenario2_IUCNalone",
                                          "scenario3_IUCN_and_Predic")]


save(data_zonation_reduced,file= "data_zonation_reduced.RData")
#Reduce for Laure 
load("data_zonation.RData")



