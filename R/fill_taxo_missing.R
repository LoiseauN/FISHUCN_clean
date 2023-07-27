fill_taxo <- function(data){ 

  
FB_IUCN_taxo_na = FB_IUCN_more %>% filter(is.na(Genus)) 

#Add Genus and calculate the rest with taxize
FB_IUCN_taxo_na = FB_IUCN_taxo_na %>% mutate(Genus = sub("\\_.*", "", rownames(FB_IUCN_taxo_na))) %>% dplyr::select(-Family)

#taxo =  classification(FB_IUCN_taxo_na$Genus, db = "ncbi") %>% 
 # rbind() %>% 
  #filter(rank == "family")%>% 
  #dplyr::select(name, query) %>% 
  #dplyr::rename(Family = "name",
   #             Genus = "query")
#save(taxo,file = here::here("outputs", "taxo.RData"))
load(file = here::here("outputs", "taxo.RData"))

taxo <- unique(taxo)
FB_IUCN_taxo_nona <- FB_IUCN_taxo_na %>% left_join(taxo,by="Genus")
rownames(FB_IUCN_taxo_nona) <- rownames(FB_IUCN_taxo_na)

FB_IUCN_taxo_nona = FB_IUCN_taxo_nona[!duplicated(FB_IUCN_taxo_nona), ]

#Manually filling out the rest
FB_IUCN_taxo_nona["Bembrops_greyae","Family"] = "Percophidae"
FB_IUCN_taxo_nona["Verilus_anomalus","Family"] = "Acropomatidae"
FB_IUCN_taxo_nona["Lissonanchus_lusherae","Family"] = "Gobiesocidae"
FB_IUCN_taxo_nona["Morwong_fuscus","Family"] = "Cheilodactylidae"
FB_IUCN_taxo_nona["Pseudogoniistius_nigripes","Family"] = "Cheilodactylidae"
FB_IUCN_taxo_nona["Eques_lanceolatus","Family"] = "Sciaenidae"
FB_IUCN_taxo_nona["Pteropelor_noronhai","Family"] = "Scorpaenidae"

return(FB_IUCN_taxo_nona)
}
