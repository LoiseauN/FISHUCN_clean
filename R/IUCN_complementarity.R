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
  
  #data_machine <- IUCN_preds_machine_final
  #data_deep <- IUCN_preds_deep_final
  
  #Merge Data
  all_predict <- merge(data_deep,data_machine,by="species",all=T)
  colnames(all_predict) <- c("species", "IUCN_deep","proba", "IUCN_machine","n","percentage")
  
  #Highlight consensus or not between deeplearing and machine learning
  all_predict$agree <- NA
  for (i in 1:nrow(all_predict)){
    
    if(is.na(all_predict$IUCN_deep[i]) & !is.na(all_predict$IUCN_machine[i])) {all_predict$agree[i] <- "ONLY_MACHINE"}
    
    else if(!is.na(all_predict$IUCN_deep[i]) & is.na(all_predict$IUCN_machine[i])){all_predict$agree[i] <- "ONLY_DEEP"}
    
    else if(all_predict$IUCN_deep[i]==all_predict$IUCN_machine[i]){all_predict$agree[i] <- "AGREE"}
    
    else{all_predict$agree[i] <- "NOT AGREE"}
    
  }
  
  
  #Predict with the complementary approach
  all_predict$predict_complementary <- NA
  for (i in 1:nrow(all_predict)){
    
    if(all_predict$agree[i] == "ONLY_MACHINE") {all_predict$predict_complementary[i] <- as.character(all_predict$IUCN_machine[i])}
    
    if(all_predict$agree[i] == "ONLY_DEEP"){all_predict$predict_complementary[i] <- as.character(all_predict$IUCN_deep[i])}
    
    if(all_predict$agree[i] == "AGREE"){all_predict$predict_complementary[i] <- as.character(all_predict$IUCN_machine[i])}
    
    if(all_predict$agree[i] == "NOT AGREE"){all_predict$predict_complementary[i] <- NA}
    
  }
  
  #Predict with the consensus
  all_predict$predict_consensus <- NA
  for (i in 1:nrow(all_predict)){
    
    if(all_predict$agree[i] == "ONLY_MACHINE") {all_predict$predict_consensus[i] <- NA}
    
    if(all_predict$agree[i] == "ONLY_DEEP"){all_predict$predict_consensus[i] <- NA}
    
    if(all_predict$agree[i] == "AGREE"){all_predict$predict_consensus[i] <- as.character(all_predict$IUCN_machine[i])}
    
    if(all_predict$agree[i] == "NOT AGREE"){all_predict$predict_consensus[i] <- NA}
    
  }
  
  # Take the 
  all_predict$proba_select <- NA 
  
  for (i in 1:nrow(all_predict)){
    
    if(all_predict$agree[i] == "ONLY_MACHINE") {all_predict$proba_select[i] <- all_predict$percentage[i]}
    
    if(all_predict$agree[i] == "ONLY_DEEP"){all_predict$proba_select[i] <- all_predict$proba[i]}
    
    #IF AGREE WE PUT PROBA == 100
    if(all_predict$agree[i] == "AGREE"){all_predict$proba_select[i] <- 100}
    
    if(all_predict$agree[i] == "NOT AGREE"){all_predict$proba_select[i] <- NA}
    
   
  }
  return(all_predict)
}
