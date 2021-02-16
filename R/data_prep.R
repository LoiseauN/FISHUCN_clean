#This function creates a list of X downsampled versions of your complete data with even number of classified and non classified species
#It will be used to train the models to predict on new data
#Entry
# - a dataframe with species as rownames, traits as columns and a IUCN column with all non classified species
# - a dataframe with species as rownames, traits as column and a IUCN column with all classified species

data_prep <- function(data_NC,data_C){
  
  #Variable with optimal number of folds for downsampling
  x = floor(length(data_NC$IUCN)/length(data_C$IUCN))
  
  #Randomly shuffle the data
  data_NC_shuffled <-data_NC[sample(nrow(data_NC)),]
  
  #Create x equally size folds of non classified species
  folds <- cut(seq(1,nrow(data_NC_shuffled)),breaks=x,labels=FALSE)
  
  data_NC_split = split(data_NC_shuffled,folds)
  
  #Looping through splitted NC list and append classified species to each list species to each list
  data_split = mclapply(1:length(data_NC_split),function(i){
    
    data_NC_split[[i]] = rbind(data_NC_split[[i]],data_C)
    
  })
  
}

