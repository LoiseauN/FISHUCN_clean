#' Try out the missForest performance
#'
#' This functions tests the missForest on complete data using cross validation
#' 
#' @param data Your data with species as rownames, traits as column names and the IUCN column. 
#' In this column, Species with no info are labelled NA and the others according to their status: CR,LC,NT...
#' 
#' @return Your dataframe with IUCN column transformed into two categories : NT and T. Thr corresponds to species 
#' in CR, EN or VU category and NThr species in LC or NT category 
#'
#' @export


IUCN_split = function(data){
  
  #Check IUCN column is a factor
  
  data$IUCN = as.factor(data$IUCN)

  
  #Check if NA is in dataset
  
  if (any(is.na(data[,!colnames(data)=="IUCN"]))) {
    warning("We found some NA in your dataset. You can impute some of these NAs using the missforest_Test and the missForest function, 
  or delete them from your data set to run the model.")
  }
  
  return(data)
  
}
