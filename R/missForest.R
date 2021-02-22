#' Run missForest based on its performance
#'
#' This functions runs the missForest on your data based on its performance for each trait
#' 
#' @param data_tofill The output from the IUCN_predict function
#' @param baseline Between 0.5 and 1, baseline of how well the missForest performed to predict
#' @param mf_test The output from the missForest_test function
#' 
#' @return Your data with NAs filled out for traits where the missForest performed well
#'
#' @export

missForest_applied = function(data_tofill,baseline,mf_test){
  
  #Keeping IUCN status for merge
  IUCN_formerge = data_tofill %>%
    dplyr::select(IUCN)%>%
    rownames_to_column("species")
  
  #Selecting out IUCN column for missforest
  data_tofill_noIUCN = data_tofill %>%
    dplyr::select(-IUCN)
  
  #FILTER OUT FACTORS WITH OVER 53 CATEGORIES
  #Get unique length of all the factor traits
  factor_length = data.frame()
  for (i in 1:length(data_tofill_noIUCN[,]))
  {
    #If the trait is your factor the get the length
    if (is.factor(data_tofill_noIUCN[,i])==TRUE) 
    { 
      data_frame = data.frame(id = colnames(data_tofill_noIUCN[i]),
                              length = length(unique(data_tofill_noIUCN[,i])))
      factor_length = rbind(factor_length,data_frame)
    }
  }
  
  #IF any of the factors has over 53 categories filter it out of the data
  if(any(factor_length>53)){
    
    over53 = factor_length %>%
      filter(length>53)
    
    data_prepped = data_tofill_noIUCN %>%
      dplyr::select(-(all_of(over53$id)))
    
    for_merge = data_tofill_noIUCN %>%
      dplyr::select(all_of(over53$id))%>%
      rownames_to_column("species")
    #Else keep it as it is
  }else{
    data_prepped = data_tofill_noIUCN
  }
  
  #Keeping only traits where missForest performed above baseline
  mf_perf = mf_test %>%
    rownames_to_column("trait")%>%
    filter(V1>baseline)
  
  #Imputing data
  impute = missForest(data_prepped,variablewise = T, verbose = T)
  
  #Data frame with imputed values for values where missForest performance is good
  impute_data = impute$ximp %>%
    dplyr::select(all_of(mf_perf$trait))%>%
    rownames_to_column("species")
  
  #Data frames with missing values without the values we can impute with miss forest
  NA_data = data_prepped %>%
    dplyr::select(!all_of(mf_perf$trait))%>%
    rownames_to_column("species")
  
  #Merge imputed data with NA data 
  data_complete = NA_data %>%
    left_join(impute_data,by="species")%>%
    left_join(for_merge,by="species")%>%
    left_join(IUCN_formerge,by="species")%>%
    column_to_rownames("species")
  
}
