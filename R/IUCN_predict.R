
#This functions predicts the IUCN status of new species 
#On each downsampled data, we run 10 models for a total number of 10*x models
#It uses the splitted data created with the data_prep function
#Entry : 
# - The list of splitted downsampled data from the data_prep function
# - A dataframe with species as rownames, columns with same traits as the training data and an empty IUCN column to predict
# - A data frame with species as COLUMN names in the same order as the previous dataframe, and traits as column names
#Returns : 
# A dataframe with the predicted status for each species for each model loop

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

#Based on the results of the IUCN predictions, let's calculate a consensus of all models based on a baseline you decide
#Entry : 
# - The dataframe for the IUCN_pred function
# - The numer of splits in your data (length of the first function)
# - A consensus number ranging from 50 to 99 % 
#Returns : 
# A dataframe with the species, its IUCN classification, the number of models that classified it in this class and the percentage

IUCN_consensus = function(data_predicted,splits,baseline){
  
  #Model predictions according to class
  Predicted_percentage= data_predicted %>%
    group_by(species)%>%
    count(score.predictions)%>%
    #Adding percentage of perdiction in each class
    mutate(percentage=(n*100)/(splits*10))%>%
    rename(IUCN="score.predictions")
  
  #Keeping only species where prediction is >80%
  IUCN_final_preds = Predicted_percentage %>%
    filter(percentage>=baseline)
}


