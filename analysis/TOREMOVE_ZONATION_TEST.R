
#--- Prepare for scenario  


#IF WE WANT TO REMOVE SPECIES WITH DATA DEFICIENCY
# data_zonation$selected_species_IUCNonly <- NA
#data_zonation$selected_species_complementary_W_IUCN <- NA
#data_zonation$selected_species_consensus_W_IUCN <- NA

#for(i in 1:nrow(data_zonation)){ 

#print(i)

#if(!is.na(data_zonation$IUCN_cat[i]) ) data_zonation$selected_species_IUCNonly[i] <- 1

#if(!is.na(data_zonation$IUCN_cat[i]) | !is.na(data_zonation$predict_complementary[i])) 
#data_zonation$selected_species_complementary_W_IUCN[i] <- 1


#if(!is.na(data_zonation$IUCN_cat[i]) | !is.na(data_zonation$predict_consensus[i])) 
#data_zonation$selected_species_consensus_W_IUCN[i] <- 1
# }

#data_zonation <- data_zonation[!rownames(data_zonation) %in% sort(rownames(FB_final))[c(1:90)],]

#data_zonation$IUCN_alone <- NA

# ----------  Change 
#for(i in 1:nrow(data_zonation)){ 

#   print(i)

#if(is.na(data_zonation$IUCN_cat[i])) data_zonation$IUCN_alone[i] <- NA  
#else if(data_zonation$IUCN_cat[i] =="LC") data_zonation$IUCN_alone[i] <- "NThr"
#else if(data_zonation$IUCN_cat[i] =="NT") data_zonation$IUCN_alone[i] <- "NThr"
#else if(data_zonation$IUCN_cat[i] =="nt") data_zonation$IUCN_alone[i] <- "NThr"
#else if(data_zonation$IUCN_cat[i] =="VU") data_zonation$IUCN_alone[i] <- "Thr"
#else if(data_zonation$IUCN_cat[i] =="EN") data_zonation$IUCN_alone[i] <- "Thr"
#else if(data_zonation$IUCN_cat[i] =="CR") data_zonation$IUCN_alone[i] <- "Thr"

#}