
#' Create data with only species information
#'
#' This functions creates data with species information
#' 
#' @param data_full The output from the missForest_applied function
#' 
#' @return A dataframe with species inforamtion
#'
#' @export

create_species = function(data_full){
  
  data_test = data_full %>%
    filter(is.na(IUCN))%>%
    drop_na(-IUCN)%>%
    dplyr::select(-IUCN) %>%
    rownames_to_column("species")
    
  
}
