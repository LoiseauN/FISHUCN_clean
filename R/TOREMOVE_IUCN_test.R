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


#IUCN_test = function(data_split,loops){
  
  # data_split = 
# ranger_loop = mclapply(1:length(data_split),function(i){
    
#   mclapply(1:loops,function(p){
      
      #SHuffling data, then Splitting into training and test data
#     split <- initial_split(data_split[[i]][sample(nrow(data_split[[i]])),], prop = 0.8)
      
#     train <- training(split)
#     test <- testing(split)
      
      #Creating the model and predicting to test data
#      mod = ranger(IUCN ~ ., data = train , probability = F,
#                  importance ="permutation",num.trees = 1000,mtry = 3)
      
                   #      score=predict(mod,data=test)
      
#      CM=confusionMatrix(score$predictions, test$IUCN)
      
#      model_output = list(data.frame(variable_importance <- rownames_to_column(as.data.frame(importance(mod)))),
#                          data.frame(Accuracy = CM$overall[1],
#                                     Kappa = CM$overall[2],
#                                     TSS = CM$byClass[1]+CM$byClass[2]-1),
#                          Balanced_Accuracy = CM$byClass[11])
      
#   })
    
#  })
  
  #Getting variable importance from output list
#  rel_inf = ranger_loop %>%
    #Transfomring list into large dataframe
#flatten_df() %>%
#    #Recuperating column names
#   unnest(cols=c())%>%
#   #Keeping only variables and their importance
#   dplyr::select(rowname,importance.mod.)%>%
#   na.omit()%>%
#   #Getting mean variable importance over all iterations
#   group_by(rowname)%>%
#   dplyr::summarize(importance.mod.=mean(importance.mod.))
  
  
  #Getting model performance from output list
    #preds = ranger_loop %>%
    ##Transfomring list into large dataframe
    #flatten_df() %>%
    ##Recuperating column names
    #unnest(cols=c())%>%
    #dplyr::rename(Balanced_Accuracy="Balanced Accuracy")%>%
    #Keeping only variables and their importance
    #dplyr::select(Accuracy:TSS)%>%
    #na.omit()%>%
    #map_dbl(mean)%>%
    #as_tibble(rownames=NA)%>%
    #rownames_to_column("Metric")
  

  #Getting balanced accuracy from output list
  #preds_class = ranger_loop %>%
    ##Transfomring list into large dataframe
    #flatten_df() %>%
    ##Recuperating column names
    #unnest(cols=c())%>%
    #dplyr::rename(Balanced_Accuracy="Balanced Accuracy")%>%
    ##Keeping only variables and their importance
    #dplyr::select(Balanced_Accuracy)%>%
    #na.omit()%>%
    # dplyr::summarize(Balanced_Accuracy=mean(Balanced_Accuracy))
  
  #save(rel_inf, file = here::here("outputs", "rel_inf.RData"))
  #save(preds, file = here::here("outputs", "preds.RData"))
  
  #  output = list(rel_inf,preds,preds_class)

  #}
