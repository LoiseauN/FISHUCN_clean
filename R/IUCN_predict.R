#' Predict IUCN status running 200 models
#'
#' This functions predicts the IUCN status of new species based on 200 models
#'
#' @param data_split The output from the data_prep function
#' @param data_topredict A data frame with species as rownames, traits as columns and an empty IUCN column to predict
#' @param data_species A data frame with species as column names in the same order as previous data frame, and a traits as column names
#'
#' @return A data frame with the predicted status for each species for each loop
#'
#' @export


IUCN_predict = function(data_split,data_topredict,data_species){
  
  #Creating x models based on the number of splits of downsampled data
  
  ranger_loop = mclapply(1:length(data_split),function(i){
    
    mclapply(1:10,function(p){
      
      #Randomizing each sub dataframe 
      train <- data_split[[i]][sample(nrow(data_split[[i]])),]
      
      #Creating the model and predicting to new data
      mod = ranger::ranger(IUCN ~ ., data = train, probability = F,
                           importance ="permutation",num.trees = 1000,mtry = 3)
      
      score=predict(mod,data=data_topredict)
      
      #Getting the score predictions
      model_preds = list(data.frame(rank = paste(i,".",p),
                                     score$predictions))
      
    })
    
  })
  
  #Getting the predictions of each loop in dataframe format
  preds = do.call(rbind,do.call(rbind,do.call(rbind,ranger_loop)))
  
  #Binding predictions with species information
  data_predicted = cbind(preds,data_species)
  
  
}


