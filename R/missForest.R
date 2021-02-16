#Loading packages
pkgs <- c("tidyverse","missForest","gbm","tidymodels","caret","visdat","analytics","mice","VIM","Amelia","Hmisc","mi","parallel")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#Loading necessary data
load(file=file.path(data_dir,"FB_vars.Rdata"))

#Preparing data to impute. We delete Genus and Family because too many categories to predict
lelz = FB_vars %>%
  dplyr::select(-c(IUCN,Genus,Family,Env_1))%>%
  head(500)

#This function tests the missForest algorithm on your data without the NAs in order to see which variables we can estimate
#Entry :
#- A dataframe with species as rownames, traits as columns and NO NA
#Output : 
# A dataframe with, for each traits, a R² or mean according to type of traits giving missforest performance

FB_imp = FB_vars %>%
  #We don't use the IUCN values because that's what we want to predict
  dplyr::select(-c(IUCN,Env_1,Genus,Family))%>%
  na.omit()

#Producing 20% of NA in dataset
FB_imp_na = FB_imp %>%
  prodNA(0.2)

missForest_test = function(data_noNA){
  
  #Imputing 20% of NA in complete data 
  data_NA = data_noNA %>%
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
  for(i in colnames(data_noNA)){
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
  for(p in colnames(data_noNA)){
    
    #IF TRAITS IS NUMERIC THEN LINEAR REGRESSION BETWEEN known and predicted by missforests to get performance
    if(is.numeric(data_noNA[,p])) {
      
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
  
}

test = missForest_test(FB_imp)

#This function fills out missing data for variables for which the missforest performed well enough
#Entry : 
# - The data with species as rownames, traits as columns 
# - A baseline value for minimum missForest performance
# - The output from the missForest test function 
#Output
# A data frame where all traits fo rwhich missforest performance was good are imputed

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



