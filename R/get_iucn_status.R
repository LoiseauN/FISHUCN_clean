
#' Try out the missForest performance
#'
#' This functions tests the missForest on complete data using cross validation
#' 
#' @param data The output from the IUCN predict function
#' 
#' @return A dataframe with R² or Accuracy from missForest cv according to the trait type 
#'
#' @export
#' 
#' 

get_iucn_status = function(data){
  
IUCN_status <- do.call(rbind,lapply(1:nrow(data),function(i){
  print(i)
  sp <- gsub("_"," ",data$species[i])
  status <- rredlist::rl_history(sp, key ="73d6c97e1bc80791af1167c8bbd7416ac3043d28b4633c51765eff87a9cb2da3")$result
  if(length(status) == 0) { status <- data.frame(species = data$species[i],
                                                 IUCN_status = "DD")
  }else{status <- data.frame(species = data$species[i],
                             IUCN_status = status[1,"code"])
  }
  
}))

#Remove EW ad EX
IUCN_status <- IUCN_status[!IUCN_status$IUCN_status %in% c("EW","EX"),]

#Convert in THR and NonTh
IUCN_status$IUCN_status <- str_replace(IUCN_status$IUCN_status, "NT",'NThr')
IUCN_status$IUCN_status <- str_replace(IUCN_status$IUCN_status, "LC",'NThr')
IUCN_status$IUCN_status <- str_replace(IUCN_status$IUCN_status, "LR/lc",'NThr')
IUCN_status$IUCN_status <- str_replace(IUCN_status$IUCN_status, "LR/nt",'NThr')

IUCN_status$IUCN_status <- str_replace(IUCN_status$IUCN_status, "VU",'Thr')
IUCN_status$IUCN_status <- str_replace(IUCN_status$IUCN_status, "CR",'Thr')
IUCN_status$IUCN_status <- str_replace(IUCN_status$IUCN_status, "EN",'Thr')

#Convert DD to NA
IUCN_status <- IUCN_status %>% mutate(IUCN_status = na_if(IUCN_status,"DD"))


  #IUCN variable : If Least Concern or Near Threatened put NonThreatened, if other category then Threatnen
  #0 is NON CLASSE and 1 is CLASSE
  #mutate(IUCN = ifelse(IUCN_status == "LC" |IUCN_status == "NT" | IUCN_status == "nt","NC","C"),
  #      IUCN = as.factor(IUCN))%>%
  #Deleting IUCN status column 
  #dplyr::select(-IUCN_status)%>%
  #Filter out species that are numbers with no information at all
 return(IUCN_status) 
}
