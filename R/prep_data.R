

prep_data = function(distribution,speciestraits,elasmo){
  #example
  #distribution <- FishDistribArea_all
  #speciestraits <- species_traits
  #elasmo <- FamilyElasmo
  
distribution = distribution %>% mutate(species = gsub("_", "-", species))

#Extracting Trophic level
TrophicLevel = rfishbase::ecology(version="19.04",fields=c("Species","FoodTroph")) %>%
  dplyr::rename(species="Species") %>%
  collect()

#Adapting to our data
TrophicLevel$species <- gsub(" ","-",TrophicLevel$species)

#Getting Family and genus information
family_genus = rfishbase:: load_taxa(version="19.04") %>%
  dplyr::select(Species,Genus,Family)%>%
  dplyr::rename(species="Species")%>%
  collect()

#Adapting to our data
family_genus$species <- gsub(" ","-",family_genus$species)

#Getting commercial fish base information
fishbasecomm = species(version="19.04") %>%
  dplyr::select(Species,PriceCateg,Aquarium,Importance,
                LongevityWild,BodyShapeI,DepthRangeDeep,DepthRangeComDeep,
                CommonLength,Length)%>%
  dplyr::rename(species="Species")

fishbasecomm$species = gsub(" ","-",fishbasecomm$species)

#Joining fishbase data with our species distribution data
FB_scrapped = speciestraits %>%
  rownames_to_column("species")%>%
  left_join(distribution,by="species")%>%
  left_join(family_genus,by="species")%>%
  left_join(fishbasecomm,by="species") %>%
  #left_join(TrophicLevel, by ="species") %>%
  filter(!(Family %in% elasmo$Family))

FB_scrapped <- unique(FB_scrapped)

#
trait_sup <- data.frame(species = rownames(Fish_trait_Metawebproject),
                        FoodTroph = Fish_trait_Metawebproject$FoodTroph,
                        DepthRangeShallow = Fish_trait_Metawebproject$DepthRangeShallow,
                        DepthRangeDeep = Fish_trait_Metawebproject$DepthRangeDeep,
                        Schooling= Fish_trait_Metawebproject$Schooling)
trait_sup$species = gsub("_","-",trait_sup$species)


FB_scrapped <- FB_scrapped[,!colnames(FB_scrapped) %in% c("FoodTroph","Depth_max","DepthRangeDeep","DepthRangeComDeep")]

#FB_scrapped <- merge(FB_scrapped,trait_sup,by="species",all=T)

FB_scrapped = FB_scrapped %>%
   left_join(trait_sup,by="species")%>%
  column_to_rownames("species")

#Keeping variables we need for machine learning
FB_vars = FB_scrapped %>%
  dplyr::select(c(species,Max_length,Env_1,Env_2,Climate,Resilience,Vul,FoodTroph,Repro.Mode,DistrArea,IUCN_status,
                  Repro.Fertil,Genus,Family,PriceCateg,Importance, LongevityWild,BodyShapeI,Length,CommonLength,
                  DepthRangeDeep,DepthRangeShallow, Aquarium,Schooling)) %>% #Depth_max,
  mutate(Max_length = arm::rescale(log(Max_length+1)),
         Env_1 = as.factor(Env_1),
         Env_2 = as.factor(Env_2),
         Climate = as.factor(Climate),
         Resilience = as.factor(Resilience),
         Vul = arm::rescale(log(Vul+1)),
         FoodTroph = arm::rescale(log(as.numeric(FoodTroph)+1)),
         Repro.Mode = as.factor(Repro.Mode),
         DistrArea = arm::rescale(log(DistrArea+1)),
         IUCN_status = as.factor(IUCN_status),
         Repro.Fertil = as.factor(Repro.Fertil),
         #Depth_max = arm::rescale(log(Depth_max+1)),
         Genus = as.factor(Genus),
         Family= as.factor(Family),
         Schooling = as.factor(Schooling),
         PriceCateg = as.factor(PriceCateg),
         Aquarium = as.factor(Aquarium),
         Importance = as.factor(Importance),
         LongevityWild = arm::rescale(log(as.numeric(LongevityWild)+1)),
         BodyShapeI = as.factor(BodyShapeI),
         DepthRangeDeep = arm::rescale(log(as.numeric(DepthRangeDeep)+1)),
         DepthRangeShallow = arm::rescale(log(as.numeric(DepthRangeShallow)+1)),
         Length =  arm::rescale(log(as.numeric(Length)+1)),
         CommonLength = arm::rescale(log(as.numeric(CommonLength)+1)))%>%
  #Convert DD to NA
  mutate(IUCN_status = na_if(IUCN_status,"DD"),
         PriceCateg = na_if(PriceCateg,"unknown"))%>%
  #IUCN variable : If Least Concern or Near Threatened put NC, if other category then C
  #0 is NON CLASSE and 1 is CLASSE
  mutate(IUCN = ifelse(IUCN_status == "LC" |IUCN_status == "NT" | IUCN_status == "nt","NC","C"),
         IUCN = as.factor(IUCN))%>%
  #Deleting IUCN status column 
  dplyr::select(-IUCN_status)%>%
    #Filter out species that are numbers with no information at all
  filter_all(any_vars(!is.na(.)))

  
return(FB_vars)

}
