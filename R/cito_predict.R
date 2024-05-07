#' Predict IUCN status running 200 models
#'
#' This functions predicts the IUCN status of new species based on 200 models
#'
#' @param data_split The output from the data_prep function
#' @param data Your full data or the output from the miss forest function
#' @param loops The number of models to run on each downsampled dataset. Recommended : 10 or 100
#'
#' @return A data frame with the predicted status for each species for each loop
#'
#' @export


IUCN_predict_deep = function(data_split,data,loops){
  #
  #data_split = data_splited_deep_RF
  #data = data_noNA
  #loops= 10
  
  if (any(is.na(data[,!colnames(data)=="IUCN"]))) {
    stop("Your traits still contain NAs. Please remove them to continue")
  }
  
  #Species informations
  data_species = data %>%
    filter(is.na(IUCN))%>%
    drop_na(-IUCN)%>%
    dplyr::select(-IUCN) %>%
    rownames_to_column("species")
  
  #Create data to predict on
  data_topredict = data %>%
    filter(is.na(IUCN))%>%
    drop_na(-IUCN)
  
  #Creating x models based on the number of splits of downsampled data
  
  ranger_loop = mclapply(1:length(data_split),function(i){
    
    mclapply(1:loops,function(l){
     
       print(paste(i,".",l))
      #Randomizing each sub dataframe 
      train <- data_split[[i]][[l]]$train
      
      #Creating the model and predicting to new data
    
      mod <- dnn(IUCN ~ ., data = train, loss= "softmax",lr_scheduler = scheduler,plot = F,
                 optimizer = opt) 
      
      score=predict(mod,newdata = data_topredict, type = "class")
      
      #Getting the score predictions
      model_preds = list(data.frame(rank = paste(i,".",l),
                                    score = score))
      return(model_preds)
    })
    
  })
  
  #Getting the predictions of each loop in dataframe format
  preds = do.call(rbind,do.call(rbind,do.call(rbind,ranger_loop)))
  
  #Binding predictions with species information
  data_predicted = cbind(preds,data_species)
  
  
}


