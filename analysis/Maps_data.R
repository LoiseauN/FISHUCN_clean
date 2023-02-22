remotes::install_github("ropensci/rnaturalearthhires")
install.packages(c("rnaturalearth", "rnaturalearthdata"))

#'-------------------------------------------
source(here::here("R","map_function.R"))

# import layers world
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# projection
mol   <- paste0("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 ", "+units=m +no_defs")
world <- sf::st_transform(world, crs=mol)

# import layers border
mollBorder <- st_read(here::here("data","mollBorder","mollBorder.shp"))


#My data
mask.full=raster::raster(here::here("data","mask.full.tif"))
all_geo_res2$Perthrbefore <- all_geo_res2$Rthr/(all_geo_res2$Rthr+all_geo_res2$Rnothr+all_geo_res2$Rnostatus)
all_geo_res2$Perthrfinal <- all_geo_res2$Rfinalthr/(all_geo_res2$Rfinalthr+all_geo_res2$Rfinalnothr+all_geo_res2$Rfinalnostatus)

all_geo_res2$Pernothrbefore <- all_geo_res2$Rnothr/(all_geo_res2$Rthr+all_geo_res2$Rnothr+all_geo_res2$Rnostatus)
all_geo_res2$Pernothrfinal <- all_geo_res2$Rfinalnothr/(all_geo_res2$Rfinalthr+all_geo_res2$Rfinalnothr+all_geo_res2$Rfinalnostatus)

all_geo_res2$Pernostatusbefore <- all_geo_res2$Rnostatus/(all_geo_res2$Rthr+all_geo_res2$Rnothr+all_geo_res2$Rnostatus)
all_geo_res2$Pernostatusfinal <- all_geo_res2$Rfinalnostatus/(all_geo_res2$Rfinalthr+all_geo_res2$Rfinalnothr+all_geo_res2$Rfinalnostatus)



all_geo_res2[is.na(all_geo_res2)] <- 0


var = c("Rthr","Rnothr","Rnostatus","Rfinalthr","Rfinalnothr",
        "Rfinalnostatus","rankSc1","rankSc2","DeltaThr","DeltaRank","Perthrbefore","Pernothrfinal")



all_map <- lapply(1:length(var),function(x){

  if(! var[x] %in% c("DeltaRank","rankSc1","rankSc2")){ 
    mask = mask.full
    df <- all_geo_res2[,var[x]]
    df[is.na(df)] <- 0
    mask[all_geo_res2$ID] = df
  }

  else{ 
    mask = mask.full
    df <- all_geo_res2[,var[x]]
    df[is.na(df)] <- 0
    df <- data.frame(df,getValues(mask.full))
    colnames(df) <- c(var[x],"MPA")
    df[,var[x]][df$MPA==100] <- -1000000
    mask[all_geo_res2$ID] = df[,1]
  }
    
  #raster to stars
  CellsIn = exactextractr::exact_extract(mask,mollBorder, include_cell = TRUE)
  
  NumCell <- seq(1,length(mask),1)
  
  CellsToRemove <- NumCell[!NumCell%in%CellsIn[[1]]$cell]

  mask[CellsToRemove] = NA
  
  mask <- stars::st_as_stars(mask, na.omit = F)
  mask <- sf::st_transform(mask, crs=mol, na.omit = F)
  mask.full.polygon <- sf::st_as_sf(mask,as.point = F, na.omit = F)
  mask.full.polygon <-  fortify(mask.full.polygon, na.omit = F)
  mask.full.polygon <- sf::st_transform(mask.full.polygon, crs=mol, na.omit = F)
   
  if(var[x] %in% c("DeltaRank","rankSc1","rankSc2")) {mask.full.polygon$mask.full[mask.full.polygon$mask.full == -1000000] <- NA}
    
#'--------------------------------------------------------@Threatened
if (var[x] =="Rthr" )  {  title =  "BEFORE Threatened" 
pal <- wesanderson::wes_palette("Zissou1", max(c(all_geo_res2$Rthr,all_geo_res2$Rfinalthr),na.rm=T), type = "continuous")

map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full), color = NA) +
  scale_fill_gradientn(colours = pal, 
                        limits = c(min(c(all_geo_res2$Rthr,all_geo_res2$Rfinalthr)), 
                                   max(c(all_geo_res2$Rthr,all_geo_res2$Rfinalthr))))+
  scale_colour_gradientn(colours = pal, 
                        limits = c(min(c(all_geo_res2$Rthr,all_geo_res2$Rfinalthr)), 
                                   max(c(all_geo_res2$Rthr,all_geo_res2$Rfinalthr))))+
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
  ggsave(file = here::here("figures/IUCN_Threatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)
  rm(map)
  }
  
#'--------------------------------------------------------@Non-Threatened
pal <- wes_palette("Zissou1", max(c(all_geo_res2$Rnothr,all_geo_res2$Rfinalnothr),na.rm=T), type = "continuous")

 if (var[x] =="Rnothr" )  {  title =  "BEFORE Non-Threatened"  
map <- ggplot(world) +
geom_sf(data = mask.full.polygon, aes(fill = mask.full), color = NA) +
  scale_fill_gradientn(colours = pal, 
                       limits = c(min(c(all_geo_res2$Rnothr,all_geo_res2$Rfinalnothr),na.rm=T), 
                                  max(c(all_geo_res2$Rnothr,all_geo_res2$Rfinalnothr),na.rm=T)))+
  scale_colour_gradientn(colours = pal, 
                       limits = c(min(c(all_geo_res2$Rnothr,all_geo_res2$Rfinalnothr),na.rm=T), 
                                   max(c(all_geo_res2$Rnothr,all_geo_res2$Rfinalnothr),na.rm=T)))+
 
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

ggsave(file = here::here("figures/IUCN_NonThreatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

#'--------------------------------------------------------@No-Status
 if  (var[x] =="Rnostatus" )  {  title =  "BEFORE No-Status"  
pal <- wes_palette("Zissou1", max(c(all_geo_res2$Rnostatus,all_geo_res2$Rfinalnostatus),na.rm=T), type = "continuous")

map <- ggplot(world) +
geom_sf(data = mask.full.polygon, aes(fill = mask.full), color = NA) +
  scale_fill_gradientn(colours = pal, 
                        limits = c(min(c(all_geo_res2$Rnostatus,all_geo_res2$Rfinalnostatus),na.rm=T), 
                                   max(c(all_geo_res2$Rnostatus,all_geo_res2$Rfinalnostatus),na.rm=T)))+
  scale_colour_gradientn(colours = pal, 
                         limits = c(min(c(all_geo_res2$Rnostatus,all_geo_res2$Rfinalnostatus),na.rm=T), 
                                  max(c(all_geo_res2$Rnostatus,all_geo_res2$Rfinalnostatus),na.rm=T)))+
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

ggsave(file = here::here("figures/IUCN_NoStatus.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

#'--------------------------------------------------------@IUCNThreatenedpredicted

 if  (var[x] =="Rfinalthr" )  {  title =  "AFTER Threatened"  
pal <- wes_palette("Zissou1", max(c(all_geo_res2$Rthr,all_geo_res2$Rfinalthr),na.rm=T), type = "continuous")

map <- ggplot(world) +
geom_sf(data = mask.full.polygon, aes(fill = mask.full), color = NA) +
  scale_fill_gradientn(colours = pal, 
                        limits = c(min(c(all_geo_res2$Rthr,all_geo_res2$Rfinalthr),na.rm=T), 
                                   max(c(all_geo_res2$Rthr,all_geo_res2$Rfinalthr),na.rm=T)))+
  scale_colour_gradientn(colours = pal, 
                        limits = c(min(c(all_geo_res2$Rthr,all_geo_res2$Rfinalthr),na.rm=T), 
                                   max(c(all_geo_res2$Rthr,all_geo_res2$Rfinalthr),na.rm=T)))+
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

ggsave(file = here::here("figures/IUCNandpredictedThreatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

    
#'--------------------------------------------------------@IUCNNonThreatenedpredicted
if  (var[x] =="Rfinalnothr" )  {title =  "AFTER Non-Threatened"   
pal <- wes_palette("Zissou1",  max(c(all_geo_res2$Rnothr,all_geo_res2$Rfinalnothr),na.rm=T), type = "continuous")

map <- ggplot(world) +
geom_sf(data = mask.full.polygon, aes(fill = mask.full), color = NA) +
  scale_fill_gradientn(colours = pal, 
                         limits = c(min(c(all_geo_res2$Rnothr,all_geo_res2$Rfinalnothr),na.rm=T), 
                                    max(c(all_geo_res2$Rnothr,all_geo_res2$Rfinalnothr),na.rm=T)))+
  scale_colour_gradientn(colours = pal, 
                         limits = c(min(c(all_geo_res2$Rnothr,all_geo_res2$Rfinalnothr),na.rm=T), 
                                    max(c(all_geo_res2$Rnothr,all_geo_res2$Rfinalnothr),na.rm=T)))+
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

ggsave(file = here::here("figures/IUCNandpredictedNonThreatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

#'--------------------------------------------------------@IUCNNo-Statuspredicted

 if  (var[x] =="Rfinalnostatus" )  {  title =  "AFTER No-Status"  
pal <- wes_palette("Zissou1",  max(c(all_geo_res2$Rnostatus,all_geo_res2$Rfinalnostatus),na.rm=T), type = "continuous")

map <- ggplot(world) +
geom_sf(data = mask.full.polygon, aes(fill = mask.full), color = NA) +
  scale_fill_gradientn(colours = pal, 
                         limits = c(min(c(all_geo_res2$Rnostatus,all_geo_res2$Rfinalnostatus),na.rm=T), 
                                    max(c(all_geo_res2$Rnostatus,all_geo_res2$Rfinalnostatus),na.rm=T)))+
  scale_colour_gradientn(colours = pal, 
                        limits = c(min(c(all_geo_res2$Rnostatus,all_geo_res2$Rfinalnostatus),na.rm=T), 
                                   max(c(all_geo_res2$Rnostatus,all_geo_res2$Rfinalnostatus),na.rm=T)))+
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

ggsave(file = here::here("figures/IUCNandpredictedNoStatus.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}



#'--------------------------------------------------------@RANKBEFORE

if  (var[x] =="rankSc1" )  {  title =  "BEFORE Rank"  
pal <- wes_palette("Zissou1",  max(c(all_geo_res2$rankSc1,all_geo_res2$rankSc2),na.rm=T), type = "continuous")

map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full)) +
  scale_fill_gradientn(colours = pal, na.value = "#458B00B3",
                       limits = c(min(c(all_geo_res2$rankSc1,all_geo_res2$rankSc2),na.rm=T), 
                                  max(c(all_geo_res2$rankSc1,all_geo_res2$rankSc2),na.rm=T)))+
  scale_colour_gradientn(colours = pal, na.value = "#458B00B3",
                         limits = c(min(c(all_geo_res2$rankSc1,all_geo_res2$rankSc2),na.rm=T), 
                                    max(c(all_geo_res2$rankSc1,all_geo_res2$rankSc2),na.rm=T)))+
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

ggsave(file = here::here("figures/RankBefore.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}



#'--------------------------------------------------------@RANKAFTER

if  (var[x] =="rankSc2" )  {  title =  "AFTER Rank"  
pal <- wes_palette("Zissou1",  max(c(all_geo_res2$rankSc1,all_geo_res2$rankSc2),na.rm=T), type = "continuous")

map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full)) +
  scale_fill_gradientn(colours = pal, na.value = "#458B00B3",
                       limits = c(min(c(all_geo_res2$rankSc1,all_geo_res2$rankSc2),na.rm=T), 
                                  max(c(all_geo_res2$rankSc1,all_geo_res2$rankSc2),na.rm=T)))+
  scale_colour_gradientn(colours = pal, na.value = "#458B00B3",
                         limits = c(min(c(all_geo_res2$rankSc1,all_geo_res2$rankSc2),na.rm=T), 
                                    max(c(all_geo_res2$rankSc1,all_geo_res2$rankSc2),na.rm=T)))+
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

ggsave(file = here::here("figures/RankAfter.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}
#'--------------------------------------------------------@DeltaThreatened

#min(mask.full.polygon$mask.full[log10(mask.full.polygon$mask.full+1)>0])


 if  (var[x] =="DeltaThr" )  {  title =  "Delta Richness Threatened"  
map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full)) +
  #viridis::scale_fill_viridis(option = "inferno")+
  scale_fill_gradient2(low = "white",mid="#00AFBB", midpoint = 0,
                      high="#FC4E07", space ="Lab" )+
  scale_colour_gradient2(low = "white",mid="#00AFBB", midpoint = 0,
                       high="#FC4E07", space ="Lab" )+

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
ggsave(file = here::here("figures/DeltaRichness.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

#'--------------------------------------------------------@DeltaRankzonation
 if  (var[x] =="DeltaRank" )  {  title =  "Delta Rank" 
# adjustcolor( "chartreuse4", alpha.f = 0.7)
map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
   scale_fill_gradient2(midpoint= 0, low="#00AFBB", mid="white",
                        high="#FC4E07", space ="Lab",na.value = "#458B00B3")+
  scale_color_gradient2(midpoint= 0, low="#00AFBB", mid="white",
                       high="#FC4E07", space ="Lab",na.value = "#458B00B3")+
  #scale_fill_manual(name = "mask.full", values = my_colors) +
  #scale_fill_hp(option = "Ravenclaw", 
  #              limits = c(min(all_geo_res2$DeltaRank),na.rm=T),
  #                         max(all_geo_res2$DeltaRank,na.rm=T))+
  geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  
  ggthemes::theme_map(base_family = "serif") +
  theme(legend.position = "bottom", 
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18),
        legend.text     = element_text(face = "plain", size = 12),
        legend.key.width=unit(1.5,"cm")) #+
#guides(fill = guide_legend(nrow = 1))
ggsave(file = here::here("figures/DeltaRank.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

#'--------------------------------------------------------@PerTHRbefore
if (var[x] =="Perthrbefore" )  {  title =  "Percentage THR IUCN" 
map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full)) +
  scale_fill_gradient(low="#00AFBB",high="#FC4E07", space ="Lab" , 
                      limits = c(min(c(all_geo_res2$PerTHRbefore,all_geo_res2$PerTHRfinal),na.rm=T), 
                                 max(c(all_geo_res2$PerTHRbefore,all_geo_res2$PerTHRfinal),na.rm=T)))+
  scale_colour_gradient(low="#00AFBB",high="#FC4E07", space ="Lab" , 
                        limits = c(min(c(all_geo_res2$PerTHRbefore,all_geo_res2$PerTHRfinal),na.rm=T), 
                                   max(c(all_geo_res2$PerTHRbefore,all_geo_res2$PerTHRfinal),na.rm=T)))+
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
ggsave(file = here::here("figures/PerTHRbefore.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

    
#'--------------------------------------------------------@PerTHRfinal
if (var[x] =="Perthrfinal" )  {  title =  "Percentage IUCN + predicted THR " 
map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full)) +
  scale_fill_gradient(low="#00AFBB",high="#FC4E07", space ="Lab" , 
                      limits = c(min(c(all_geo_res2$PerTHRbefore,all_geo_res2$PerTHRfinal),na.rm=T), 
                                 max(c(all_geo_res2$PerTHRbefore,all_geo_res2$PerTHRfinal),na.rm=T)))+
  scale_colour_gradient(low="#00AFBB",high="#FC4E07", space ="Lab" , 
                        limits = c(min(c(all_geo_res2$PerTHRbefore,all_geo_res2$PerTHRfinal),na.rm=T), 
                                   max(c(all_geo_res2$PerTHRbefore,all_geo_res2$PerTHRfinal),na.rm=T)))+
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
ggsave(file = here::here("figures/PerTHRfinal.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

  
})




