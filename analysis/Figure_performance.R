
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



# Violin plot basique
plot_performance_RF <- function(data){
  data$error_type <- factor(data$error_type , level =c("TP","FP","FN"))

dp <- ggplot(data, aes(x=error_type, y=percentage, fill=error_type)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Performance",x="Prediction", y = "Percentage")
  dp + theme_classic()

dp + scale_fill_hp_d(option = "Ravenclaw") + theme_minimal()
}








##Metric





IUCN_metric_performance_RF = function(data,loops){ 
  
  #data = test_IUCN
  
  do.call(rbind,mclapply(1:length(data),function(i){
    
    table_metric <-do.call(rbind, mclapply(1:loops,function(l){
      
      #Randomizing each sub dataframe 
      metric_table <- data.frame(metric = names(data[[i]][[l]]$metric),
                                  value = data[[i]][[l]]$metric) %>%
        janitor::clean_names()
      
      metric_table$modelID <- paste0(i,"_",l)
      
      return(metric_table)
      
    }))
    return(table_metric)
    
  }))
  
}


plot_metric_RF = function(data){
# Violin plot basique
dp <- ggplot(data, aes(x=metric, y=value, fill=metric)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Metric Random Forest",x="Metric", y = "Value")+
  scale_fill_hp_d(option = "Ravenclaw") + theme_minimal()
dp
}




