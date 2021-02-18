#' Create train data with all classified species
#'
#' This functions selects all the classified species for training
#' 
#' @param data_full The output from the missForest_applied function
#' 
#' @return A dataframe with all classified species
#'
#' @export


create_train_C = function(data_full){
  
  data_NC = data_full %>%
    filter(IUCN=="C")%>%
    na.omit()
  
}