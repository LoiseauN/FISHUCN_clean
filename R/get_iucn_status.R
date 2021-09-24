
IUCN_status <- do.call(rbind,lapply(1:nrow(FB_vars),function(i){
  print(i)
  sp <- gsub("_"," ",FB_vars$species[i])
  status <- rredlist::rl_history(sp, key ="73d6c97e1bc80791af1167c8bbd7416ac3043d28b4633c51765eff87a9cb2da3")$result
  if(length(status) == 0) { status <- data.frame(species = FB_vars$species[i],
                                                 IUCN_status = "DD")
  }else{status <- data.frame(species = FB_vars$species[i],
                             IUCN_status = status[1,"code"])
  }
  
}))


#Convert DD to NA
IUCN_status mutate(IUCN_status = na_if(IUCN_status,"DD"),
                   
#Remove EW ad EX
      
  #IUCN variable : If Least Concern or Near Threatened put NonThreatened, if other category then Threatnen
  #0 is NON CLASSE and 1 is CLASSE
  #mutate(IUCN = ifelse(IUCN_status == "LC" |IUCN_status == "NT" | IUCN_status == "nt","NC","C"),
  #      IUCN = as.factor(IUCN))%>%
  #Deleting IUCN status column 
  #dplyr::select(-IUCN_status)%>%
  #Filter out species that are numbers with no information at all
  


save(IUCN_status, file = "outputs/IUCN_status.Rdata")

