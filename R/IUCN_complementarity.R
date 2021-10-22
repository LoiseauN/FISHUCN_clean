#' Compleme,tar of IUCN status based on the 200 models
#'
#' This functions calculates the consensus of the 200 models based on a specified baseline
#'
#' @param data_predicted The output from the IUCN_predict function
#' @param splits Length of the data_prep list
#' @param baseline A percentage, between 50 and 100%, to keep IUCN prediction
#' 
#' @return A data frame with its IUCN classifications, the number of models that classified it there and the percentage
#'
#' @export

IUCN_complementarity = function(data_machine,data_deep){
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

