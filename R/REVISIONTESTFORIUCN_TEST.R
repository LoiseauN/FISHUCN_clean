#Splitting data with NA filled out by missForest or with original data with no NA
split = data_prep(data_noNA)



split <- cross_val_split(split,10)


data_splited_deep_RF <- split


save(data_splited_deep_RF,file = here::here("outputs","data_splited_deep_RF.RData"))



#' Test the model on complete data
#'
#' This functions tests the model on your data based on complete data using cross validation
#'
#' @param data_split The output from the data_prep function
#' @param loops The number of models to run on each downsampled dataset. Recommended : 10 
#' 
#' @return A data frame with variable importance of each trait by permutation
#' @return A data frame with model performance : accuracy, Kappa, TSS
#' @return A data frame with Balanced accuracy across categoris
#'
#' @export


IUCN_test = function(data_split,loops){
  
  # data_split = 
  ranger_loop = mclapply(1:length(data_split),function(i){
    
    mclapply(1:loops,function(l){
      
      #SHuffling data, then Splitting into training and test data
     
      train <- data_split[[i]][[l]]$train
      test <- data_split[[i]][[l]]$test
      
      #Creating the model and predicting to test data
      mod = ranger(IUCN ~ ., data = train , probability = F,
                   importance ="permutation",num.trees = 1000,mtry = 3)
      
      score=predict(mod,data=test)
      
      #Find species with FALSE POSITIVE AND FALSE NEGATIVE
      mat_CM <- data.frame(species = rownames(test),
                           reference= test$IUCN,
                           prediction = score$predictions)
      mat_CM$error_type <- NA
      for (j in 1:nrow(mat_CM)) {
        if(mat_CM$reference[j] == mat_CM$prediction[j] ) mat_CM$error_type[j] <- "TP"
        if(mat_CM$reference[j] == "NThr" &  mat_CM$prediction[j] == "Thr") mat_CM$error_type[j] <- "FP"
        if(mat_CM$reference[j] == "Thr" &  mat_CM$prediction[j] == "NThr") mat_CM$error_type[j] <- "FN"
      }
    
      
      CM=confusionMatrix(score$predictions, test$IUCN)
      
      model_output = list(variable_importance = rownames_to_column(as.data.frame(importance(mod))),
                          confusion_matrix =  CM$table, 
                          detailled_confusion_matrix = mat_CM,
                          metric  = CM$byClass)
      
    })
    
  })
  
  #Getting variable importance from output list
  rel_inf = ranger_loop %>%
    #Transfomring list into large dataframe
    flatten_df() %>%
    #Recuperating column names
    unnest(cols=c())%>%
    #Keeping only variables and their importance
    dplyr::select(rowname,importance.mod.)%>%
    na.omit()%>%
    #Getting mean variable importance over all iterations
    group_by(rowname)%>%
    dplyr::summarize(importance.mod.=mean(importance.mod.))
  
  
  #Getting model performance from output list
  preds = ranger_loop %>%
    #Transfomring list into large dataframe
    flatten_df() %>%
    #Recuperating column names
    unnest(cols=c())%>%
    dplyr::rename(Balanced_Accuracy="Balanced Accuracy")%>%
    #Keeping only variables and their importance
    dplyr::select(Accuracy:TSS)%>%
    na.omit()%>%
    map_dbl(mean)%>%
    as_tibble(rownames=NA)%>%
    rownames_to_column("Metric")
  
  
  #Getting balanced accuracy from output list
  preds_class = ranger_loop %>%
    #Transfomring list into large dataframe
    flatten_df() %>%
    #Recuperating column names
    unnest(cols=c())%>%
    dplyr::rename(Balanced_Accuracy="Balanced Accuracy")%>%
    #Keeping only variables and their importance
    dplyr::select(Balanced_Accuracy)%>%
    na.omit()%>%
    dplyr::summarize(Balanced_Accuracy=mean(Balanced_Accuracy))
  
  save(rel_inf, file = here::here("outputs", "rel_inf.RData"))
  save(preds, file = here::here("outputs", "preds.RData"))
  
  output = list(rel_inf,preds,preds_class)
  
}



