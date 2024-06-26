
prep_data = function(distribution,speciestraits,elasmo){
  #example
  #distribution <- FishDistribArea_all
  #speciestraits <- species_traits
  #elasmo <- FamilyElasmo
  
distribution = distribution %>% mutate(species = gsub("_", "-", species))
#distribution <- distribution %>%  column_to_rownames("species")

#Extracting Trophic level
#TrophicLevel = rfishbase::ecology(version="19.04",fields=c("Species","FoodTroph")) %>%
 #  dplyr::rename(species="Species") %>%
  # collect()""

TrophicLevel = rfishbase::estimate(version="19.04",fields=c("Species","Troph","K")) %>%
  dplyr::rename(species="Species") %>%
  collect()

#Adapting to our data because trophic, repro and climate were not well informated using web scrapping
#Trophic
TrophicLevel$species <- gsub(" ","-",TrophicLevel$species)


#Repro
repro <- rfishbase::reproduction(version="19.04")%>%
  dplyr::rename(species="Species") %>%
  collect()

repro$species <- gsub(" ","-",repro$species)

repro <-  repro[,colnames(repro) %in% c("species", "RepGuild1","RepGuild2","ParentalCare","ReproMode","Fertilization")]

#Climate
Climate_bis =  rfishbase::ecosystem(version="19.04") %>%
  dplyr::rename(species="Species") %>%
  collect()

Climate_bis <- Climate_bis[,colnames(Climate_bis) %in% c("species", "Climate")]

Climate_fishbase <- do.call(rbind,lapply(1:length(unique(Climate_bis$species)), function(i){
  sub <- Climate_bis[Climate_bis$species == unique(Climate_bis$species)[i],]

  if(sum(is.na(sub$Climate)) == nrow(sub)){
    sub <- data.frame(species = unique(Climate_bis$species)[i],
                      Climate = NA)
    

  }else{ 
         
  sub <- data.frame(species = unique(Climate_bis$species)[i],
                    Climate = names(sort(table(sub$Climate),decreasing = TRUE))[1]) }
  return(sub)
}))



Climate_fishbase$Climate[Climate_fishbase$Climate == 'Tropical'] <- 'tropical'
Climate_fishbase$Climate[Climate_fishbase$Climate == 'Subtropical'] <- 'subtropical'
Climate_fishbase$Climate[Climate_fishbase$Climate == 'Boreal'] <- 'boreal'
Climate_fishbase$Climate[Climate_fishbase$Climate == 'Temperate'] <- 'temperate'

Climate_fishbase$species <- gsub(" ","-",Climate_fishbase$species)
#Getting Family and genus information
family_genus = rfishbase:: load_taxa(version="19.04") %>%
  dplyr::select(Species,Genus,Family)%>%
  dplyr::rename(species="Species")%>%
  collect()

#Adapting to our data
family_genus$species <- gsub(" ","-",family_genus$species)
#family_genus <- family_genus %>%  column_to_rownames("species")

speciestraits <- speciestraits[,!colnames(speciestraits)%in%c("Repro.Mode","Repro.Fertil", "Repro.ParentCare","Climate")]


FB_scrapped = speciestraits %>%
  rownames_to_column("species")%>%
  left_join(distribution,by="species")%>%
 left_join(family_genus,by="species")%>%
  left_join(TrophicLevel, by ="species")%>%
  left_join(Climate_fishbase, by ="species") %>%
  left_join(repro, by ="species") %>%
  filter(!(Family %in% elasmo$Family))

FB_scrapped$species <- gsub("-","_",FB_scrapped$species )




trait_sup <- data.frame(species = rownames(Fish_trait_Metawebproject),
                        Schooling= Fish_trait_Metawebproject$Schooling,
                        Aquarium= Fish_trait_Metawebproject$Aquarium,
                        LongevityWild= Fish_trait_Metawebproject$LongevityWild,
                        Length= Fish_trait_Metawebproject$Length,
                        PriceCateg= Fish_trait_Metawebproject$PriceCateg,
                        BodyShapeI= Fish_trait_Metawebproject$BodyShapeI,
                        Habitat = Fish_trait_Metawebproject$DemersPelag)

#FB_scrapped <- FB_scrapped[,!colnames(FB_scrapped) %in% c("Depth_max","DepthRangeDeep","DepthRangeComDeep")]

FB_scrapped = FB_scrapped %>%
   left_join(trait_sup,by="species")

#Keeping variables we need for machine learning (remove with too much NA)
FB_vars = FB_scrapped %>%
  dplyr::select(c(species,Length,Habitat, Climate,Troph,ReproMode,Fertilization,RepGuild1,DistrArea,
                  Genus,Family,PriceCateg,BodyShapeI,#Importance,
                  Aquarium,K)) %>%
  mutate(Length = Length,
         Habitat = as.factor(Habitat),
         Climate = as.factor(Climate),
         Troph = as.numeric(Troph),
         ReproMode = as.factor(ReproMode),
         RepGuild1 = as.factor(RepGuild1),
         DistrArea = log10(DistrArea+1),
         Fertilization = as.factor(Fertilization),
         Genus = as.factor(Genus),
         Family= as.factor(Family),
         K = as.numeric(K),
         PriceCateg = as.factor(PriceCateg),
         Aquarium = as.factor(Aquarium),
         #Importance = as.factor(Importance),
         BodyShapeI = as.factor(BodyShapeI))%>%
  mutate(PriceCateg = na_if(PriceCateg,"unknown"))%>%
  mutate(Habitat = na_if(Habitat,"no_data"))%>%
  filter_all(any_vars(!is.na(.)))


FB_vars$PriceCateg <- factor(FB_vars$PriceCateg, order = TRUE, levels =c( 'low', 'medium', 'high', 'very high'))
FB_vars$Habitat <- factor(FB_vars$Habitat, order = TRUE, levels =c( 'benthopelagic', 
                                                                    'bathypelagic', 
                                                                    'pelagic-neriti', 
                                                                    'pelagic-oceanic',
                                                                    'pelagic',
                                                                    'bathydemersal',
                                                                    'demersal',
                                                                    'reef-associated'))
 
#get_status

return(FB_vars)

}

#sapply(Fish_trait_Metawebproject, function(y) sum(length(which(is.na(y)))))
#sapply(FB_vars, function(y) sum(length(which(is.na(y)))))

