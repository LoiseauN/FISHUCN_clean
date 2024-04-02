#' Function to format the datasplit for python
#'
#' @param data_split The output from the data_prep function
#' @param loops The number of models to run on each downsampled dataset. Recommended : 10 
#' 
#' @return A data frame for python
#'
#' @export

loop_prep_ANN = function(data_split,loops){
  #data_split  <- data_splited_deep_RF
  res <- do.call(rbind,mclapply(1:length(data_split),function(i){
  
    do.call(rbind,mclapply(1:loops,function(l){
    
    train <- data.frame(species = rownames(data_split[[i]][[l]]$train),
                        type = rep("train",nrow(data_split[[i]][[l]]$train)),
                        bloc = i,
                        iteration = l)
    test <- data.frame(species = rownames(data_split[[i]][[l]]$test),
                       type = rep("test",nrow(data_split[[i]][[l]]$test)),
                       bloc = i,
                       iteration = l)
    
    tab <- rbind(train,test)
    }))
  
  
}))
  
  return(res)
} 
