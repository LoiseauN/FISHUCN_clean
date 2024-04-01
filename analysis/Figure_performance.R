
IUCN_performance_RF = function(data,loops){ 
  
  #data = test_IUCN
  
  do.call(rbind,mclapply(1:length(data),function(i){
 
   tableCM <-do.call(rbind, mclapply(1:loops,function(l){
    
    #Randomizing each sub dataframe 
    CM_table <- data.frame(data[[i]][[l]]$confusion_matrix) %>%
      janitor::clean_names()
      
    CM_table$error_type <- NA
    for (j in 1:nrow(CM_table)) {
      if(CM_table$reference[j] == CM_table$prediction[j] ) CM_table$error_type[j] <- "TP"
      if(CM_table$reference[j] == "NThr" &  CM_table$prediction[j] == "Thr") CM_table$error_type[j] <- "FP"
      if(CM_table$reference[j] == "Thr" &  CM_table$prediction[j] == "NThr") CM_table$error_type[j] <- "FN"
    }
    
    CM_table<- CM_table[,-c(1,2)]
    
    CM_table <- aggregate(. ~ error_type, data = CM_table, sum)
    
    CM_table$percentage <- NA 
    for (j in 1:nrow(CM_table)) {
      CM_table$percentage[j] <- (CM_table$freq[j] / sum(CM_table$freq))*100
    }
    
    CM_table$modelID <- paste0(i,"_",l)
    
    return(CM_table)
    
  }))
   return(tableCM)
   
  }))

}

performance_RF <- IUCN_performance_RF(test_IUCN,10)


# Violin plot basique
performance_RF$error_type <- factor(performance_RF$error_type , level =c("TP","FP","FN"))

dp <- ggplot(performance_RF, aes(x=error_type, y=percentage, fill=error_type)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Performance",x="Prediction", y = "Percentage")
  dp + theme_classic()

dp + scale_fill_hp_d(option = "Ravenclaw") + theme_minimal()
