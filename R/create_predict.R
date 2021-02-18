
#' Create data to predict IUCN status on
#'
#' This functions selects all the species for which the IUCN info is not available
#' 
#' @param data_full The output from the missForest_applied function
#' 
#' @return A dataframe with all species to predict IUCN status on
#'
#' @export

create_predict = function(data_full){
  
  data_test = data_full %>%
    filter(is.na(IUCN))%>%
    drop_na(-IUCN)
  
}
