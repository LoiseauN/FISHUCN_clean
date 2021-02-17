#' Run missForest based on its performance
#'
#' This functions runs the missForest on your data based on its performance for each trait
#' 
#' @param data_tofill Your data with the NA with species as rownames, traits as columns 
#' @param baseline Between 0.5 and 1, baseline of how well the missForest performed to predict
#' @param mf_test The output from the missForest_test function
#' 
#' @return Your data with NAs filled out for traits where the missForest performed well
#'
#' @export

missForest_applied = function(data_tofill,baseline,mf_test){
  
  #Keeping only traits where missForest performed above baseline
  mf_perf = mf_test %>%
    rownames_to_column("trait")%>%
    filter(V1>baseline)
  
  #Imputing data
  impute = missForest(data_tofill,variablewise = T, verbose = T)
  
  #Data frame with imputed values for values where missForest performance is good
  impute_data = impute$ximp %>%
    dplyr::select(all_of(mf_perf$trait))%>%
    rownames_to_column("species")
  
  #Data frames with missing values without the values we can impute with miss forest
  NA_data = data_tofill %>%
    dplyr::select(!all_of(mf_perf$trait))%>%
    rownames_to_column("species")
  
  #Merge imputed data with NA data 
  data_complete = NA_data %>%
    left_join(impute_data,by="species")%>%
    column_to_rownames("species")
  
}

