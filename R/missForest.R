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
#' @exports



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
    #If the trait is a factor the get the length
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
    
    data_final = data_tofill_noIUCN %>%
      dplyr::select(-(all_of(over53$id)))
    
    for_merge = data_tofill_noIUCN %>%
      dplyr::select(all_of(over53$id))%>%
      rownames_to_column("species")
    
    warning("Some of your traits had more than 53 categories. These traits were filtered out during the missForest and then re-added to your data later")
    
    #Else keep it as it is
  }else{
    data_final = data_tofill_noIUCN
  }
  
  #Keeping only traits where missForest performed above baseline
  mf_perf = mf_test %>%
    rownames_to_column("trait")%>%
    filter(V1>baseline)
  
  data_notfill = data_final %>%
    dplyr::select(!one_of(mf_perf$trait))%>%
    rownames_to_column("species")
  
  data_prepped = data_final %>%
    dplyr::select(one_of(mf_perf$trait))
  
  #Warning message if somes traits cannot be imputed
  if(length(mf_perf)!=length(mf_test)){
    warning("According to your baseline, the missForest performance was too low to impute some of your traits. Therefore, NAs are still present in
            your data base. Rows with NAs will be deleted in order to run the model.")
  }
  
  #Imputing data
  impute = missForest(data_prepped,variablewise = T, verbose = T)
  
  #Data frame with imputed values for values where missForest performance is good
  impute_data = impute$ximp %>%
    rownames_to_column("species")%>%
    left_join(data_notfill,by="species")

  #Merge imputed data with NA data 
  data_complete = impute_data %>%
    left_join(for_merge,by="species")%>%
    #Don't keep variables with too many NA and not used in the model that missforest can't predict 
    dplyr::select(-c(LongevityWild)) %>%
    na.omit()%>%
    left_join(IUCN_formerge,by="species")%>%
    column_to_rownames("species")
  
}
  

