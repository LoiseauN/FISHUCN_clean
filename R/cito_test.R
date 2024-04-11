install.packages("cito")
library("torch")
install_torch()
library(cito)


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
scheduler <- config_lr_scheduler(type = "step",
                                 step_size = 30,
                                 gamma = 0.15,
                                 verbose = TRUE)

IUCN_deep_test = function(data_split,loops){
  
  
  # data_split = data_splited_deep_RF
  ranger_loop = mclapply(1:length(data_split),function(i){
    
    mclapply(1:loops,function(l){
      
      #SHuffling data, then Splitting into training and test data
      
      train <- data_split[[i]][[l]]$train
      test <- data_split[[i]][[l]]$test
      
      #Creating the model and predicting to test data
      mod <- dnn(IUCN~., data = train, loss= "softmax",batchsize = 30,lr_scheduler = scheduler)
      

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
      
      
    CM=confusionMatrix(data = score$predictions, reference = test$IUCN)
      
      model_output = list(variable_importance = rownames_to_column(as.data.frame(mod$variable.importance)),
                          confusion_matrix =  CM$table, 
                          detailled_confusion_matrix = mat_CM,
                          metric  = CM$byClass)
      return(model_output)
      
    })
    
  },mc.cores = 4)
  return(ranger_loop)
}


pred_deep_cito <- IUCN_deep_test(data_splited_deep_RF,loop = 10)

save(here::here("ouputs","pred_deep_cito.RData"))


# create learning rate scheduler object
scheduler <- config_lr_scheduler(type = "step",
                                 step_size = 30,
                                 gamma = 0.15,
                                 verbose = TRUE)

# Build and train  Network
nn.fit <- dnn(IUCN~., data = data_splited_deep_RF[[2]][[1]]$train, loss= "binomial",batchsize = 30,lr_scheduler = scheduler)
#summary(nn.fit)

res_predict <- predict(nn.fit, newdata = data_splited_deep_RF[[2]][[1]]$test, type = "class")

example <- caret::confusionMatrix(data=res_predict, reference = data_splited_deep_RF[[1]][[1]]$test$IUCN)
example
analyze_training(nn.fit)


PDP(nn.fit)

data <- datasets::iris
nn.fit.val <- dnn(Sepal.Length~., data = data, epochs = 32,
              loss= "mse", validation = 0.2)


plot(data$Sepal.Length,predict(nn.fit.val))

str(nn.fit.val)

example <- confusionMatrix(data=predicted_value, reference = expected_value)