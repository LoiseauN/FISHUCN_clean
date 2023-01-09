#' IUCN deep predict
#' 
#' This function run the python script to train the models and predict IUCN classifications
#'
#' @return no return data
#' @export

IUCN_deep_predict = function(){
  python_file_train = here::here("Python/train.py")
  system(paste0("python", python_file_train))
}

#' Deep learning prediction
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

IUCN_deep = function(data_predicted,baseline){
  #data_predicted <- IUCN_preds_deep
  #baseline <- 80
  
  #Keeping only species where prediction is >80%
  IUCN_deep_preds = data_predicted %>%
    filter(percentage>=baseline)
 
  save(IUCN_deep_preds, file = here::here("outputs", "IUCN_deep_preds.Rdata"))
  return(IUCN_deep_preds)
  
  
}