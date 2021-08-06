#' Split data into downsampled data for model
#'
#' This function downsamples your data to have even number of classified and non classified species into even bits based on your data
#' This dataset will be used to train the model
#'
#' @param data_NC Your filled out data with no NA with all non classified species
#' @param data_C Your filled out data with no NA with all classified species
#' 
#' @return A list with each downsampled dataframe from your data
#'
#' @export
#' 
#' 
#' 
#' 
data_prep <- function(data){
  
  #Create threatened and non threatened species
  data_Thr = data %>%
    dplyr::filter(IUCN=="Thr")%>%
    na.omit()
  
  data_NThr = data %>%
    dplyr::filter(IUCN=="NThr")%>%
    na.omit()
  
  #Variable with optimal number of folds for downsampling
  x = floor(length(data_NThr$IUCN)/length(data_Thr$IUCN))
  
  #Randomly shuffle the data
  data_NThr_shuffled <-data_NThr[sample(nrow(data_NThr)),]
  
  #Create x equally size folds of non classified species
  folds <- cut(seq(1,nrow(data_NThr_shuffled)),breaks=x,labels=FALSE)
  
  data_NThr_split = split(data_NThr_shuffled,folds)
  
  #Looping through splitted NC list and append classified species to each list species to each list
  data_split = mclapply(1:length(data_NThr_split),function(i){
    
    data_NThr_split[[i]] = rbind(data_NThr_split[[i]],data_Thr)
    
  })
  
}