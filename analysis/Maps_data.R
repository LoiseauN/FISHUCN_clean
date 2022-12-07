remotes::install_github("ropensci/rnaturalearthhires")
install.packages(c("rnaturalearth", "rnaturalearthdata"))

#'-------------------------------------------
source(here::here("R","map_function.R"))

# import layers world
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# projection
mol   <- paste0("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 ", "+units=m +no_defs")
world <- sf::st_transform(world, crs=mol)


#My data
mask.full=raster::raster(here::here("data","mask.full.tif"))

var = c("Rthr","Rnothr","Rnostatus","Rfinalthr","Rfinalnothr","Rfinalnostatus","DeltaThr","DeltaRank")

all_map <- lapply(1:length(var),function(x){
  
mask = mask.full
df <- all_geo_res[,var[x]]
df[is.na(df)] <- 0
mask[all_geo_res$ID] = df

#raster to stars
mask <- stars::st_as_stars(mask)
mask.full.polygon <- sf::st_as_sf(mask,as.point = F)
mask.full.polygon <-  fortify(mask.full.polygon)

mask.full.polygon <- sf::st_transform(mask.full.polygon, crs=mol)

#'--------------------------------------------------------@Threatened
  if (var[x] =="Rthr" )  {  title =  "IUCN Threatened" 
  map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full)) +
    scale_fill_gradient(low="#00AFBB",high="#FC4E07", space ="Lab" , 
                        limits = c(min(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T), 
                        max(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T)))+
    scale_colour_gradient(low="#00AFBB",high="#FC4E07", space ="Lab" , 
                          limits = c(min(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T), 
                                     max(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T)))+
    geom_sf(data = world, fill = "black", color = "black", size = 0.1) +
   geom_graticules(mol) +
   geom_mapframe(mol, colour = "white", size = 2.0) +
   geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  
  ggthemes::theme_map(base_family = "serif") +
  theme(legend.position = "bottom", 
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18),
        legend.text     = element_text(face = "plain", size = 12)) #+
  #guides(fill = guide_legend(nrow = 1))
  ggsave(file = here::here("figures/IUCN_Threatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)
  rm(map)
  }
  
#'--------------------------------------------------------@Non-Threatened
else if (var[x] =="Rnothr" )  {  title =  "IUCN Non-Threatened"  
map <- ggplot(world) +
geom_sf(data = mask.full.polygon, aes(fill = mask.full), color = NA) +
  #scale_fill_manual(name = "mask.full", values = my_colors) +
  scale_fill_hp(option = "Ravenclaw", 
                limits = c(min(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T), 
                           max(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T)))+
  
  geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  
  ggthemes::theme_map(base_family = "serif") +
  theme(legend.position = "bottom", 
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18),
        legend.text     = element_text(face = "plain", size = 12)) #+
#guides(fill = guide_legend(nrow = 1))
ggsave(file = here::here("figures/IUCN_NonThreatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

#'--------------------------------------------------------@No-Status
else if  (var[x] =="Rnostatus" )  {  title =  "IUCN No-Status"  
map <- ggplot(world) +
geom_sf(data = mask.full.polygon, aes(fill = mask.full), color = NA) +
  #scale_fill_manual(name = "mask.full", values = my_colors) +
  scale_fill_hp(option = "Ravenclaw", 
                limits = c(min(c(all_geo_res$Rnostatus,all_geo_res$Rfinalnostatus),na.rm=T),
                           max(c(all_geo_res$Rnostatus,all_geo_res$Rfinalnostatus),na.rm=T)))+
  
  geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  
  ggthemes::theme_map(base_family = "serif") +
  theme(legend.position = "bottom", 
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18),
        legend.text     = element_text(face = "plain", size = 12)) #+
#guides(fill = guide_legend(nrow = 1))
ggsave(file = here::here("figures/IUCN_NoStatus.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

#'--------------------------------------------------------@IUCNThreatenedpredicted

else if  (var[x] =="Rfinalthr" )  {  title =  "IUCN + predicted Threatened"  
map <- ggplot(world) +
geom_sf(data = mask.full.polygon, aes(fill = mask.full), color = NA) +
  #scale_fill_manual(name = "mask.full", values = my_colors) +
  scale_fill_hp(option = "Ravenclaw", 
                limits = c(min(c(all_geo_res$Rthr,all_geo_res$Rfinalthr),na.rm=T), 
                           max(c(all_geo_res$Rthr,all_geo_res$Rfinalthr),na.rm=T)))+
  
  geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  
  ggthemes::theme_map(base_family = "serif") +
  theme(legend.position = "bottom", 
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18),
        legend.text     = element_text(face = "plain", size = 12)) #+
#guides(fill = guide_legend(nrow = 1))

ggsave(file = here::here("figures/IUCNandpredictedThreatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

#'--------------------------------------------------------@IUCNNonThreatenedpredicted

else if  (var[x] =="Rfinalnothr" )  {  title =  "IUCN + predicted  Non-Threatened"   
map <- ggplot(world) +
geom_sf(data = mask.full.polygon, aes(fill = mask.full), color = NA) +
  #scale_fill_manual(name = "mask.full", values = my_colors) +
  scale_fill_hp(option = "Ravenclaw", 
                limits = c(min(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T),
                           max(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T)))+
  geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  
  ggthemes::theme_map(base_family = "serif") +
  theme(legend.position = "bottom", 
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18),
        legend.text     = element_text(face = "plain", size = 12)) #+
#guides(fill = guide_legend(nrow = 1))
ggsave(file = here::here("figures/IUCNandpredictedNonThreatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

#'--------------------------------------------------------@IUCNNo-Statuspredicted

else if  (var[x] =="Rfinalnostatus" )  {  title =  "IUCN + predicted  No-Status"  
map <- ggplot(world) +
geom_sf(data = mask.full.polygon, aes(fill = mask.full), color = NA) +
  #scale_fill_manual(name = "mask.full", values = my_colors) +
  #scale_fill_hp(option = "Ravenclaw", 
  #              limits = c(min(c(all_geo_res$Rnostatus,all_geo_res$Rfinalnostatus),na.rm=T),
  #                         max(c(all_geo_res$Rnostatus,all_geo_res$Rfinalnostatus),na.rm=T)))+
  geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  
  ggthemes::theme_map(base_family = "serif") +
  theme(legend.position = "bottom", 
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18),
        legend.text     = element_text(face = "plain", size = 12)) #+
#guides(fill = guide_legend(nrow = 1))
ggsave(file = here::here("figures/IUCNandpredictedNoStatus.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

#'--------------------------------------------------------@DeltaThreatened

min(mask.full.polygon$mask.full[log10(mask.full.polygon$mask.full+1)>0])


else if  (var[x] =="DeltaThr" )  {  title =  "Delta Richness Threatened"  
map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = log10(mask.full+1), color = log10(mask.full+1))) +
  #viridis::scale_fill_viridis(option = "inferno")+
  scale_fill_gradient2(low = "white",mid="#00AFBB", midpoint = 0,
                      high="#FC4E07", space ="Lab" )+
  scale_colour_gradient2(low = "white",mid="#00AFBB", midpoint = 0,
                       high="#FC4E07", space ="Lab" )+

  
  geom_sf(data = world, fill = "black", color = "black", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  
  ggthemes::theme_map(base_family = "serif") +
  theme(legend.position = "bottom", 
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18),
        legend.text     = element_text(face = "plain", size = 12)) #+
#guides(fill = guide_legend(nrow = 1))
ggsave(file = here::here("figures/DeltaRichness.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

else if  (var[x] =="DeltaRank" )  {  title =  "Delta Rank"  
map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = scale(mask.full), color = scale(mask.full))) +
  #viridis::scale_fill_viridis(option = "inferno")+
  scale_fill_gradient2(midpoint= 0, low="#00AFBB", mid="white",
                        high="#FC4E07", space ="Lab" )+
  scale_color_gradient2(midpoint= 0, low="#00AFBB", mid="white",
                       high="#FC4E07", space ="Lab" )+
  #scale_fill_manual(name = "mask.full", values = my_colors) +
  #scale_fill_hp(option = "Ravenclaw", 
  #              limits = c(min(all_geo_res$DeltaRank),na.rm=T),
  #                         max(all_geo_res$DeltaRank,na.rm=T))+
  geom_sf(data = world, fill = "black", color = "black", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  
  ggthemes::theme_map(base_family = "serif") +
  theme(legend.position = "bottom", 
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18),
        legend.text     = element_text(face = "plain", size = 12)) #+
#guides(fill = guide_legend(nrow = 1))
ggsave(file = here::here("figures/DeltaRank.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

  
})




