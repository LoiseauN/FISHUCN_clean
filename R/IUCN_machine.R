#' Consensus of IUCN status based on the 200 models
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

IUCN_machine = function(data_predicted,splits,baseline){
  #data_predicted <- run_IUCN
  #splits<- length(split)
  #baseline <- 70
  
  #Model predictions according to class
  Predicted_percentage= data_predicted %>%
    group_by(species)%>%
    dplyr::count(score.predictions)%>%
    #Adding percentage of perdiction in each class
    dplyr::mutate(percentage=(n*100)/(splits*10))%>%
    dplyr::rename(IUCN="score.predictions")
  
  save(Predicted_percentage, file = "outputs/Predicted_percentage.Rdata")
  
  #Keeping only species where prediction is >80%
  IUCN_final_preds = Predicted_percentage %>%
    filter(percentage>=baseline)
  
  save(IUCN_final_preds, file = here::here("outputs", "IUCN_final_preds.Rdata"))
  
  return(IUCN_final_preds)
  
}
