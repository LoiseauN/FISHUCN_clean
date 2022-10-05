
data_zonation_list <- list()
#keep only pecies already in IUCN 
data_zonation_list[[1]] <- subset(data_zonation,data_zonation$selected_species_IUCNonly == 1)

#keep only predicted and species already in IUCN with complementary
data_zonation_list[[2]] <- subset(data_zonation,data_zonation$selected_species_complementary_W_IUCN == 1)

#keep only predicted and species already in IUCN with consensus
data_zonation_list[[3]] <- subset(data_zonation,data_zonation$selected_species_consensus_W_IUCN == 1)

#all species
data_zonation_list[[4]] <- data_zonation

names(data_zonation_list) <- c("IUCNonly","complementary", "consensus","all")

#data_zonation$scenario1_NoWeight <- 1
#data_zonation$scenario2_IUCNalone <- NA
#data_zonation$scenario3_IUCN_and_Predict_complementarity <- NA
#data_zonation$scenario3_IUCN_and_Predict_consensus <- NA
data_final_zonation <- lapply(1: length(data_zonation_list), function(x){
  
  data <- data_zonation_list[[x]]
  print(x)
  for(i in 1:nrow(data)){
    
    
    if(data$IUCN_cat[i] %in% "Thr"){
      data$scenario2_IUCNalone[i] <- 6
      data$scenario3_IUCN_and_Predict_complementarity[i] <-  6 
      data$scenario3_IUCN_and_Predict_consensus[i] <-  6}
    
    else if (data$IUCN_cat[i] %in% "NThr"){
      data$scenario2_IUCNalone[i] <- 1
      data$scenario3_IUCN_and_Predict_complementarity[i] <- 1
      data$scenario3_IUCN_and_Predict_consensus[i] <- 1
    }
    
    else{ 
      #Non information at all
      if(is.na(data$predict[i])){
        
        data$scenario2_IUCNalone[i] <- 2
        data$scenario3_IUCN_and_Predict_complementarity[i] <- 2 # ou 1.5 
        data$scenario3_IUCN_and_Predict_consensus[i] <- 2
      }
      
      else if(data$predict[i] %in% "NThr"){
        
        #Non Threatened predict par model
        if (!data$agree[i]  %in% "AGREE"){ 
          data$scenario2_IUCNalone[i] <- 1
          data$scenario3_IUCN_and_Predict_complementarity[i] <- data[data$species %in% data$species[i],]$proba_rescale
          data$scenario3_IUCN_and_Predict_consensus[i] <- data[data$species %in% data$species[i],]$proba_rescale
          
        }
        else { 
          
          data$scenario2_IUCNalone[i] <- 1
          data$scenario3_IUCN_and_Predict_complementarity[i] <- 1
          data$scenario3_IUCN_and_Predict_consensus[i] <- 1
        }
      }
      
      else if(data$predict[i] %in% "Thr"){
        
        if (!data$agree[i]  %in% "AGREE"){ 
          
          data$scenario2_IUCNalone[i] <- 1
          data$scenario3_IUCN_and_Predict_complementarity[i] <- data[data$species %in% data$species[i],]$proba_rescale
          data$scenario3_IUCN_and_Predict_consensus[i] <- data[data$species %in% data$species[i],]$proba_rescale
          
        } 
        
        else { 
          
          data$scenario2_IUCNalone[i] <- 1
          data$scenario3_IUCN_and_Predict_complementarity[i] <- 5
          data$scenario3_IUCN_and_Predict_consensus[i] <- 5
          
        }
      }
      
    }
  }  
  return(data)
})



names(data_final_zonation) <- c("main", "sensibility1","sensibility2")
a <- boxplot(data_final_zonation[[1]]$scenario1_NoWeight,
                                 data_final_zonation[[1]]$scenario2_IUCNalone,
                                 data_final_zonation[[1]]$scenario3_IUCN_and_Predict)

b <- boxplot(data_final_zonation[[1]]$scenario1_NoWeight,
             data_final_zonation[[1]]$scenario2_IUCNalone,
             data_final_zonation[[1]]$scenario3_IUCN_and_Predict)






dat_network <- data.frame(data_final_zonation$main[,c("species","IUCN_alone","predict")])

dat_network <- addLevel(dat_network, "Threatened")
dat_network <- addLevel(dat_network, "Non Threatened")
dat_network <- addLevel(dat_network, "No Status")


for (i in 1:ncol(dat_network)){dat_network[,i] <- as.factor(as.character(dat_network[,i]))}


dat_network<-as.data.frame(sapply(dat_network,
                                  mapvalues, from = c("Thr"), 
                                  to = c("Threatened")))

dat_network<-as.data.frame(sapply(dat_network,
                                  mapvalues, from = c("NThr"), 
                                  to = c("Non Threatened")))

not_in_model <- data.frame(species = rownames(FB_vars[!rownames(FB_vars)%in% dat_network$species,]),
                           IUCN_alone= rep(NA, length(rownames(FB_vars[!rownames(FB_vars)%in% dat_network$species,]))),
                           predict=rep(NA, length(rownames(FB_vars[!rownames(FB_vars)%in% dat_network$species,]))))



dat_network <- rbind(dat_network,not_in_model)
#dat_network<-as.data.frame(sapply(dat_network,
#                                 mapvalues, from = c("LC","NT","nt","NC"), 
#                                to = c("Non Threatened","Non Threatened",
#                                      "Non Threatened","Non Threatened")))





dat_network$IUCN_final <- NA

for (i in 1:nrow (dat_network)){
  if(is.na(dat_network$IUCN_alone[i])){ dat_network$IUCN_final[i]=dat_network$predict[i]
  
  }else{
    
    dat_network$IUCN_final[i]=dat_network$IUCN_alone[i]
    
  }
}

dat_network<-as.data.frame(sapply(dat_network,
                                  mapvalues, from = c(NA), 
                                  to = c("No Status")))

not_in_model <- data.frame(species = rownames(FB_final[!rownames(FB_final)%in% dat_network$species,]),
                           IUCN_alone= rep(NA, length(rownames(FB_final[!rownames(FB_final)%in% dat_network$species,]))),
                           predict=rep(NA, length(rownames(FB_final[!rownames(FB_final)%in% dat_network$species,]))))



dat_network <- rbind(dat_network,not_in_model)

df <- data.frame('id' = rep(dat_network$species,2),
                 'stage' = as.factor(c(rep("Before Prediction",nrow(dat_network)), rep("After Prediction",nrow(dat_network)))),
                 'group' = as.factor(c(dat_network$IUCN_alone,dat_network$IUCN_final)))
df <- transform(df,
                group = factor(group, rev(levels(group))))
df <- transform(df,
                stage = factor(stage, rev(levels(stage))))

plot_net <- 
  ggplot(df, aes(x = stage, stratum = group, alluvium = id, fill = group, label = stage)) +
  scale_x_discrete(expand = c(.15, .15)) +
  geom_flow(color="white") +
  scale_fill_manual(values = c("firebrick1", "forestgreen", "grey35"), name = "IUCN status", 
                    guide = guide_legend(reverse = TRUE))+
  geom_stratum(alpha = 1,color="white") +
  geom_text(stat = "stratum",
            aes(label = percent(after_stat(prop), accuracy = .1)))+
  theme_bw()+
  xlab("") +ylab("Number of species")

plot_net









var_partial = function(data_split,var,names){
  
  # names = c("Range size (log)","Max Length (log)")
  data_split <- split
  #SHuffling data, then Splitting into training and test data
  split <- initial_split(data_split[[i]][sample(nrow(data_split[[i]])),], prop = 0.8)
  
  train <- training(split)
  test <- testing(split)
  
  #train <- train %>%
  #  rename(DistrArea       = DistrArea,
  #        Max Length     = Max_length,
  #      Water Column   = Env_2,
  #     Climate        = Climate,
  #    Reproduction   = Repro.Mode,
  #    Fertility      = Repro.Fertil,
  #    Price Category = PriceCateg,
  #    Body Shape     = BodyShapeI,
  #    Aquarium       = Aquarium,
  #‡    Growth rate    = K)
  
  #Creating the model and predicting to test data
  mod = ranger(IUCN ~ ., data = train , probability = F,
               importance ="permutation",num.trees = 1000,mtry = 3)
  
  #ranger::partial(mod, 
  #       pred.var = c("DistrArea"), 
  #      trim.outliers = TRUE, chull = TRUE, parallel = TRUE,
  #     grid.resolution = 30,  paropts = list(.packages = "ranger"))
  
  
  all_partial <- lapply(1:length(var), function(x){
    pd = edarf::partial_dependence(mod, var[x], 
                                   data = train, interaction =F)
    
    #data <- pd %>% pivot_longer(-var[x])
    #colnames(data)[1] <- "variable"
    
    
    part_plot <- ggplot(pd,aes(x=pd[,var[x]],y=Thr)) + #00AFBB"
      geom_smooth(color="#FC4E07",fill="#FC4E07") +
      ylim(0.1,0.9)+
      theme_bw()+
      ylab("Probability")+
      xlab(names[x])+
      theme(legend.position="none")
    
    return(part_plot)
    
  })
  
  #all_partial <- gridExtra::grid.arrange(all_partial[[1]],
  #                                       all_partial[[2]],
  #                                       ncol=1)
  
}







