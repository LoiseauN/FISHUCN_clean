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
#' 

IUCN_divide = function(data){
  
  #Check IUCN column is a factor
  
  #data$IUCN = as.factor(data$IUCN)
  
  #Divide IUCN classification into two classes : Thr and NThr
  
  data_output = data %>%
    rownames_to_column("species")%>%
    mutate(IUCN = ifelse(IUCN == "LC" |IUCN == "NT" | IUCN == "nt","NThr","Thr"),
           IUCN = as.factor(IUCN))%>%
    column_to_rownames("species")
  
  return(data_output)
  
}
