prep_data = function(distribution,speciestraits,elasmo){

distribution = FishDistribArea
speciestraits  = species_traits
elasmo = FamilyElasmo
  
distribution = distribution %>% mutate(species = gsub("_", "-", species))

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
  filter(!(Family %in% elasmo$Family))

#Keeping variables we need for machine learning
FB_vars = FB_scrapped %>%
  dplyr::select(c(species,Max_length,Env_1,Env_2,Climate,Resilience,Vul,Trophic_Level,Repro.Mode,DistrArea,IUCN_status,
                  Depth_max,Repro.Fertil,Genus,Family,PriceCateg,Importance, LongevityWild,BodyShapeI,Length,CommonLength,
                  DepthRangeDeep,DepthRangeComDeep, Aquarium))%>%
  mutate(Max_length = arm::rescale(log(Max_length+1)),
         Env_1 = as.factor(Env_1),
         Env_2 = as.factor(Env_2),
         Climate = as.factor(Climate),
         Resilience = as.factor(Resilience),
         Vul = arm::rescale(log(Vul+1)),
         Trophic_Level = arm::rescale(log(as.numeric(Trophic_Level)+1)),
         Repro.Mode = as.factor(Repro.Mode),
         DistrArea = arm::rescale(log(DistrArea+1)),
         IUCN_status = as.factor(IUCN_status),
         Repro.Fertil = as.factor(Repro.Fertil),
         Depth_max = arm::rescale(log(Depth_max+1)),
         Genus = as.factor(Genus),
         Family= as.factor(Family),
         PriceCateg = as.factor(PriceCateg),
         Aquarium = as.factor(Aquarium),
         Importance = as.factor(Importance),
         LongevityWild = arm::rescale(log(as.numeric(LongevityWild)+1)),
         BodyShapeI = as.factor(BodyShapeI),
         DepthRangeDeep = arm::rescale(log(as.numeric(DepthRangeDeep)+1)),
         DepthRangeComDeep = arm::rescale(log(as.numeric(DepthRangeComDeep)+1)),
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
  column_to_rownames("species")%>%
  #Filter out species that are numbers with no information at all
  filter_all(any_vars(! is.na(.)))

return(FB_vars)

}