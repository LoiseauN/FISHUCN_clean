#' Create train data with all non classified species
#'
#' This functions selects all the non classified species for training
#' 
#' @param data_full The output from the missForest_applied function
#' 
#' @return A dataframe with all non classified species
#'
#' @export


create_train_NC = function(data_full){
  
  data_NC = data_full %>%
    filter(IUCN=="NC")%>%
    na.omit()
  
}

