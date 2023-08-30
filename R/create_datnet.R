create_dat_network <- function(){ 
  
  load(file = here::here("outputs/data_zonation.RData"))
  
dat_network <- data.frame(data_zonation[,c("species","IUCN_cat","predict_complementary")])

dat_network <- addLevel(dat_network, "Threatened")
dat_network <- addLevel(dat_network, "Non Threatened")
dat_network <- addLevel(dat_network, "No Status")
dat_network <- addLevel(dat_network, "Unpredictable")


for (i in 1:ncol(dat_network)){
  dat_network[,i] <- as.factor(as.character(dat_network[,i]))}


dat_network<-as.data.frame(sapply(dat_network,
                                  mapvalues, from = c("Thr"), 
                                  to = c("Threatened")))

dat_network<-as.data.frame(sapply(dat_network,
                                  mapvalues, from = c("NThr"), 
                                  to = c("Non Threatened")))



dat_network$IUCN_final <- NA

for (i in 1:nrow (dat_network)){
  if(is.na(dat_network$IUCN_cat[i])){ dat_network$IUCN_final[i]=dat_network$predict_complementary[i]
  
  }else{
    
    dat_network$IUCN_final[i]=dat_network$IUCN_cat[i]
    
  }
}


for (i in 1:nrow (dat_network)){
  if(is.na(dat_network$IUCN_cat[i]) & is.na(dat_network$IUCN_final[i])) { dat_network$predict_complementary[i]= "No Status"}
  
  if(is.na(dat_network$IUCN_cat[i])){ dat_network$IUCN_cat[i]= "No Status"}
  }
  

dat_network$IUCN_final[is.na(dat_network$IUCN_final)] <- "No Status"


#Unpredictible is species with too much NA
dat_network$predict_complementary_and_unpredictable <- dat_network$predict_complementary
for (i in 1:nrow (dat_network)){
  
    if(dat_network$species[i] %in% rownames(FB_nonselec_NS) & dat_network$predict_complementary[i] == "No Status"){ dat_network$predict_complementary_and_unpredictable[i] = "Unpredictable"
  
  }else{
  }

  }

return(dat_network)
}
