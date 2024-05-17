#' Predict IUCN status running 200 models
#'
#' This functions predicts the IUCN status of new species based on 200 models
#'
#' @param data_split The output from the data_prep function
#' @param data Your full data or the output from the miss forest function
#' @param loops The number of models to run on each downsampled dataset. Recommended : 10 or 100
#'
#' @return A data frame with the predicted status for each species for each loop
#'
#' @export
IUCN_importance_pd = function(data_split,data,loops){
  #
  #data_split = data_splited_deep_RF
  #data = data_noNA
  #loops= 10
  
  if (any(is.na(data[,!colnames(data)=="IUCN"]))) {
    stop("Your traits still contain NAs. Please remove them to continue")
  }
  
  #Species informations
  data_species = data %>%
    filter(is.na(IUCN))%>%
    drop_na(-IUCN)%>%
    dplyr::select(-IUCN) %>%
    rownames_to_column("species")
  
  #Create data to predict on
  data_topredict = data %>%
    filter(is.na(IUCN))%>%
    drop_na(-IUCN)
  
  #Creating x models based on the number of splits of downsampled data
  
  ranger_loop = mclapply(1:length(data_split),function(i){ #
    
    mclapply(1:loops,function(l){ #
      
      #Randomizing each sub dataframe 
      train <- data_split[[i]][[l]]$train
      
      #Creating the model and predicting to new data
      mod = ranger::ranger(IUCN ~ ., data = train, probability = F,
                           importance ="permutation",num.trees = 1000,mtry = 3)
      
      importance <- data.frame(var = names(mod$variable.importance),
                          importance = mod$variable.importance)
      #Prediction
      #score=predict(mod,data=data_topredict)
      
      #Getting the score predictions
      #model_preds = list(data.frame(rank = paste(i,".",l),
      #                              score$predictions))
      
      #reorder the most important
      importance <- importance%>% arrange(desc(importance))
      importance$rank = paste(i,".",l)
      
      # only the first fourth drivers
      all_partial <- do.call(rbind,lapply(1:nrow(importance), function(x){
       
        pd = edarf::partial_dependence(mod, importance[x,1], 
                                       data = train, interaction =F) 
        colnames(pd)[1] <- "value_var"
        
        pd$var = importance[x,1]
        
        pd$rank = paste(i,".",l)
        
        return(pd)
        
        }))
        
        res <- list(#predict =model_preds,
                    importance = importance,
                    partial = all_partial)
        
        return(res)
      
    })
    
  })
  
  #Getting the predictions of each loop in dataframe format
  #preds = do.call(rbind,do.call(rbind,do.call(rbind,ranger_loop)))
  
    #Binding predictions with species information
  #data_predicted = cbind(preds,data_species)
  
  all_res_partial <- do.call(rbind,mclapply(1:length(data_split),function(i){ #
    
   do.call(rbind, mclapply(1:loops,function(l){
      mat <- ranger_loop[[i]][[l]]$partial
    }))
  }))
  
  all_res_importance <- do.call(rbind,mclapply(1:length(data_split),function(i){ #
    
    do.call(rbind, mclapply(1:loops,function(l){
      mat <- ranger_loop[[i]][[l]]$importance
    }))
  }))

  all_res <- list(partial = all_res_partial,
                  importance = all_res_importance)
  
  return(all_res)
  
}

#' Generate partial plot
#' @param output_importance_pd
#' 
#' @return A plot in pdf format with the partial plot 
#'
#' @export
#' 
#' 
#' 


var_partial = function(data){
  data = data[[1]]
  
  pal <- c("#B35800","#A8BCC1","#42A5D9","#3573A4")
  data <- data %>% 
    mutate(var = recode_factor(var, "Troph" = "Trophic",
                                   "BodyShapeI" = 'Body Shape' ,
                                   "Aquarium" = "Interest for aquarium",
                                   "Depth_max" = "Depth max",
                                   "DistrArea" = "Range size",
                                   "K" = "Growth rate",
                                   "Habitat" = "Position in water column",
                                   "PriceCateg" = "Price Category",
                                   "ReproMode" = "Reproduction mode",
                                   "RepGuild1" = "Reproductive guild"))%>% 
  dplyr::filter(var %in% c("Range size","Length","Growth rate","Depth max"))
  
  data$value_var <- as.numeric(data$value_var)
  
  all_partial <- lapply(1:length(unique(data$var)), function(x){
  #variable <- c("Range size","Length","Growth rate","Depth max")
  select <- data[data$var == unique(data$var)[x],]
  part_plot <- ggplot(select,aes(x=select$value_var,y=Thr, group = rank)) + #00AFBB"
    geom_line(aes(group = rank),color=pal[x],alpha=0.05) + 
    geom_smooth(aes(group = 1),color=pal[x],fill=pal[x]) +
    ylim(0.2,0.8) +
    ylab("Probability")+
    xlab(unique(data$var)[x]) +
    theme(legend.position="none",
          panel.grid.minor = element_blank(),
          axis.title=element_text(size=15),
          axis.text = element_text(size = 15)) +
    theme_bw()
      
  return(part_plot)
})
  
}
      
    
###################VARIABLE IMPORTANCE    
#' Generate plot of variable importance
#'
#' Based on the test of your model, this function generates a plot of variable importance based on permutation
#'
#' @param output_importance_pd Data frame with variable importance 
#' 
#' @return A plot in pdf format with the variable importance plot 
#'
#' @export
#' 
#' 
     
var_imp = function(data){
      data = data[[2]]
      
      rel_inf <- do.call(rbind,lapply(1:length(unique(data$rank)), function(i){
        
        dat <- data[data$rank == unique(data$rank)[i],]
        tot <- sum(dat$importance)
        
        dat$importance_rel <- NA
        for (j in 1:nrow(dat)){
          dat$importance_rel[j] <- (dat$importance[j]/tot)*100
        }
        return(dat)
      }))
     
       #Plot of variable importance
    
      rel_inf <- rel_inf %>% 
        mutate(var = recode_factor(var, "Troph" = "Trophic",
                                       "BodyShapeI" = 'Body Shape' ,
                                       "Aquarium" = "Interest for aquarium",
                                       "Depth_max" = "Depth max",
                                       "Depth_min" = "Depth min",
                                       "DistrArea" = "Range size",
                                       "K" = "Growth rate",
                                       "Habitat" = "Position in water column",
                                       "PriceCateg" = "Price Category",
                                       "ReproMode" = "Reproduction mode",
                                       "RepGuild1" = "Reproductive guild"))
      
      
      plot_importance <- rel_inf %>%
        mutate(var=fct_reorder(var, importance_rel,
                              na.rm=TRUE))%>%
        ggplot( aes(x=var, y=importance_rel,fill=var))+ 
        geom_boxplot()+ 
        harrypotter::scale_fill_hp_d(option = "Ravenclaw") + 
        scale_x_discrete() +
        coord_flip()+
        xlab("") +
        ylab("Relative importance (%)") +
        theme_bw() +
        theme(
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position="none",
          axis.title=element_text(size=15),
          axis.text=element_text(size=15)
        ) 
      return(plot_importance)
    }
    