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


IUCN_predict = function(data_split,data,loops){
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
  
  ranger_loop = mclapply(1:length(data_split),function(i){
    
    mclapply(1:loops,function(l){
      
      #Randomizing each sub dataframe 
      train <- data_split[[i]][[l]]$train
      
      #Creating the model and predicting to new data
      mod = ranger::ranger(IUCN ~ ., data = train, probability = F,
                           importance ="permutation",num.trees = 1000,mtry = 3)
      
      importance = mod$variable.importance
      
      score=predict(mod,data=data_topredict)
      
      #Getting the score predictions
      model_preds = list(data.frame(rank = paste(i,".",l),
                                    score$predictions))
      
    })
    
  })
  
  #Getting the predictions of each loop in dataframe format
  preds = do.call(rbind,do.call(rbind,do.call(rbind,ranger_loop)))
  
  #Binding predictions with species information
  data_predicted = cbind(preds,data_species)
  
  
}






#' Generate partial plot
#'

#' @param data_split 
#' 
#' @return A plot in pdf format with the partial plot 
#'
#' @export
#' 
#' 
var_partial = function(data_split,loop){
  # data =  data_noNA
  # model = test_IUCN[[1]]
  
  # data_split = data_splited_deep_RF
  ranger_loop = mclapply(1:length(data_split),function(i){
    
    mclapply(1:loops,function(l){
      
      
      
      pal <- c("#B35800","#A8BCC1","#42A5D9","#3573A4")
      
      data = na.omit(data)
      #Creating the model and predicting to new data
      mod = ranger::ranger(IUCN ~ ., data = data, probability = F,
                           importance ="permutation",num.trees = 1000,mtry = 3)
      
      var <- data.frame(mod$variable.importance)
      var <- model[order(model$importance.mod.,decreasing = TRUE),]
      
      name_var <- var %>% 
        mutate(rowname = recode_factor(rowname, "Troph" = "Trophic",
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
      
      name_var <- c(name_var$rowname[c(1:4)])
      
      var <- data.frame(var[c(1:4),1])
      
      all_partial <- lapply(1:nrow(var), function(x){
        pd = edarf::partial_dependence(mod, var[x,1], 
                                       data = data, interaction =F)
        
        part_plot <- ggplot(pd,aes(x=pd[,var[x,]],y=Thr)) + #00AFBB"
          geom_smooth(color=pal[x],fill=pal[x]) +
          ylim(0,0.5)+
          theme_bw()+
          ylab("Probability")+
          xlab(name_var[x])+
          theme(legend.position="none",
                panel.grid.minor = element_blank(),
                axis.title=element_text(size=15),
                axis.text = element_text(size = 15))
        # }
        return(part_plot)
        
      })
      
      
    }
    
   
    
###################VARIABLE IMPORTANCE    
    
    
    
    #' Generate plot of variable importance
    #'
    #' Based on the test of your model, this function generates a plot of variable importance based on permutation
    #'
    #' @param rel_inf Data frame with variable importance 
    #' 
    #' @return A plot in pdf format with the variable importance plot 
    #'
    #' @export
    #' 
    #' 
    
    var_imp = function(rel_inf){
      
      #Plot of variable importance
      tot <- sum(rel_inf[,2])
      
      for (i in 1:nrow(rel_inf)){
        rel_inf[i,2] <- (rel_inf[i,2]/tot)*100
        
      }
      
      
      
      rel_inf <- rel_inf %>% 
        mutate(rowname = recode_factor(rowname, "Troph" = "Trophic",
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
      
      rel_inf %>%
        arrange(importance.mod.) %>%
        tail(20) %>%
        mutate(rowname=factor(rowname, rowname)) %>%
        ggplot( aes(x=rowname, y=importance.mod.,fill=importance.mod.))+ 
        geom_bar(stat="identity", position="dodge")+ 
        harrypotter::scale_fill_hp(option = "Ravenclaw") + 
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
      
      
      
      # ggsave(file = here::here("figures", "Figure2.pdf"), 
      #       width = 11.7, height = 8.3)
      
    }
    