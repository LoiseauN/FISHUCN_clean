#' Try out the missForest performance
#'
#' This functions tests the missForest on complete data using cross validation
#' 
#' @param data_noNA Data with species as rownames, traits as columns and IUCN column
#' 
#' @return A dataframe with R² or Accuracy from missForest cv according to the trait type 
#'
#' @export
#' 
#' 

missForest_test = function(data){
  
  #Deleting NA in data for test of missForest and substracting IUCN column
  data_noNA = data %>% 
    dplyr::select(-IUCN)%>%
    na.omit()
  
  #Get unique length of all the factor traits
  factor_length = data.frame()
  for (i in 1:length(data_noNA[,]))
  {
    #If the trait is your factor the get the length
    if (is.factor(data_noNA[,i])==TRUE) 
    { 
      data_frame = data.frame(id = colnames(data_noNA[i]),
                              length = length(unique(data_noNA[,i])))
      factor_length = rbind(factor_length,data_frame)
    }
  }
  
  #IF any of the factors has over 53 categories filter it out of the data
  if(any(factor_length>53)){
    
    over53 = factor_length %>%
      filter(length>53)
    
    data_prepped = data_noNA %>%
      select(-(all_of(over53$id)))
  #Else keep it as it is
  }else{
    data_prepped = data_noNA
  }
  
  #Imputing 20% of NA in complete data 
  data_NA = data_prepped %>%
    prodNA(0.2)
  
  #Now let's impute missing values using missForest
  data_mf = missForest(data_NA,verbose=T,variablewise=T)
  
  #Dataframe with all predictions
  preds = data_mf$ximp
  
  #Merge with complete data
  data_imp = data_NA %>%
    merge(preds,by="row.names")%>%
    column_to_rownames("Row.names")%>%
    merge(data_noNA,by="row.names")
  
  all = list()
  
  #For each of the traits, checking the missForest performance
  for(i in colnames(data_prepped)){
  #   
     #For each traits, keep only rows with NA in it
     temp = data_imp %>%
       filter(is.na(data_imp[,paste(i,".x",sep="")]))
     
     #List with one dataframes for each traits containing only NAs
     all[[i]] = temp
     
  }
  
  #Empty list where we will put missforest performance
  test_results = list()
  
  #Looping through all our traits
  for(p in colnames(data_prepped)){
    
    #IF TRAITS IS NUMERIC THEN LINEAR REGRESSION BETWEEN known and predicted by missforests to get performance
    if(is.numeric(data_prepped[,p])) {
      
      lm_test = lm(unlist(all[[p]][paste(p,".y",sep="")])~unlist(all[[p]][p]),all[[p]])
      
      #Saving adjusted r squared into list
      test_results[[p]] = summary(lm_test)$adj.r.squared
   
    #IF traits is categorical then get mean of same observations between predicted and known   
    } else {
      
      m_test = mean(unlist(all[[p]][paste(p,".y",sep="")])==unlist(all[[p]][p]))
      
      #Saving mean into list
      test_results[[p]] = m_test
    }
    
  }
  
  tests_results_final = as.data.frame(do.call(rbind,test_results))
  
  return(tests_results_final)
  
  save(tests_results_final, file = here::here("outputs", "tests_results_final.Rdata"))
  
}

