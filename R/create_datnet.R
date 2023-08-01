create_dat_network <- function(data){ 
  
  load(file = here::here("outputs/data_zonation.RData"))
  
dat_network <- data.frame(data_zonation[,c("species","IUCN_cat","predict_complementary")])

dat_network <- addLevel(dat_network, "Threatened")
dat_network <- addLevel(dat_network, "Non Threatened")
dat_network <- addLevel(dat_network, "No Status")


for (i in 1:ncol(dat_network)){
  dat_network[,i] <- as.factor(as.character(dat_network[,i]))}


dat_network<-as.data.frame(sapply(dat_network,
                                  mapvalues, from = c("Thr"), 
                                  to = c("Threatened")))

dat_network<-as.data.frame(sapply(dat_network,
                                  mapvalues, from = c("NThr"), 
                                  to = c("Non Threatened")))

#not_in_model <- data.frame(species = rownames(FB_vars[!rownames(FB_vars)%in% dat_network$species,]),
#                           IUCN_alone= rep(NA, length(rownames(FB_vars[!rownames(FB_vars)%in% dat_network$species,]))),
#                           predict=rep(NA, length(rownames(FB_vars[!rownames(FB_vars)%in% dat_network$species,]))))

#dat_network <- rbind(dat_network,not_in_model)


dat_network$IUCN_final <- NA

for (i in 1:nrow (dat_network)){
  if(is.na(dat_network$IUCN_cat[i])){ dat_network$IUCN_final[i]=dat_network$predict_complementary[i]
  
  }else{
    
    dat_network$IUCN_final[i]=dat_network$IUCN_cat[i]
    
  }
}

dat_network<-as.data.frame(sapply(dat_network,
                                  mapvalues, from = c(NA), 
                                  to = c("No Status")))

#not_in_model <- data.frame(species = rownames(FB_final[!rownames(FB_final)%in% dat_network$species,]),
#                           IUCN_alone= rep(NA, length(rownames(FB_final[!rownames(FB_final)%in% dat_network$species,]))),
#                          predict=rep(NA, length(rownames(FB_final[!rownames(FB_final)%in% dat_network$species,]))))



#dat_network <- rbind(dat_network,not_in_model)
}
