


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

# create learning rate scheduler object



# Build and train  Network

IUCN_deep_cito = function(data_split,loops){
  
  
  # data_split = data_splited_deep_RF
  ranger_loop = lapply(1:length(data_split),function(i){
    
    
    lapply(1:loops,function(l){
      print(paste0("i =", i,"_","l =",l))
      #SHuffling data, then Splitting into training and test data
      
      train <- data_split[[i]][[l]]$train
      test <- data_split[[i]][[l]]$test
      
      #Creating the model and predicting to test data
      #mod <- dnn(IUCN~., data = train, loss= "softmax",lr = 0.05,
       #          epochs = 150L, verbose = T, plot = T) #lr_scheduler = scheduler,plot = T)
      mod <- dnn(IUCN~., data = train, loss= "softmax",lr_scheduler = scheduler,plot = F,
                 optimizer = opt) 
      #mod <- dnn(IUCN~., data = train, loss= "softmax", lr_scheduler = scheduler,plot = F)
      

      score <- predict(mod, newdata = test, type = "class")
      
      #Find species with FALSE POSITIVE AND FALSE NEGATIVE
      mat_CM <- data.frame(species = rownames(test),
                           reference= test$IUCN,
                           prediction = score)
      
      mat_CM$error_type <- NA
      for (j in 1:nrow(mat_CM)) {
        if(mat_CM$reference[j] == mat_CM$prediction[j] ) mat_CM$error_type[j] <- "TP"
        if(mat_CM$reference[j] == "NThr" &  mat_CM$prediction[j] == "Thr") mat_CM$error_type[j] <- "FP"
        if(mat_CM$reference[j] == "Thr" &  mat_CM$prediction[j] == "NThr") mat_CM$error_type[j] <- "FN"
      }
      
      
      CM=confusionMatrix(data = score, reference = test$IUCN)
      
      model_output = list(#variable_importance = rownames_to_column(as.data.frame(mod$variable.importance)),
                          confusion_matrix =  CM$table, 
                          detailled_confusion_matrix = mat_CM,
                          metric  = CM$byClass)
      return(model_output)
      
    })
    
  })
  return(ranger_loop)
}
#################
#FONCTIONNE PAS TROP MAL
opt <- config_optimizer(type = "adagrad",
                        lr_decay = 1e-04,
                        weight_decay = 0.1,
                        verbose = TRUE)
#
scheduler <- config_lr_scheduler(type = "step",
                                 step_size = 30,
                                 gamma = 0.15,
                                 verbose = TRUE)

pred_deep_cito <- IUCN_deep_cito(data_splited_deep_RF,loop = 10)

save(pred_deep_cito,file = here::here("outputs/pred_deep_cito.RData"))

#Give the accuracy ! 
performance_RF <- IUCN_performance_RF(pred_deep_cito,10)
plot_performance_RF(performance_RF)

metric_performance <- IUCN_metric_performance_RF(pred_deep_cito,10)
plot_metric_RF(metric_performance)


