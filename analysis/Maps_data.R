remotes::install_github("ropensci/rnaturalearthhires")
#install.packages(c("rnaturalearth", "rnaturalearthdata"))
library("rnaturalearth")
library("rnaturalearthdata")

#'-------------------------------------------
source(here::here("R","map_function.R"))
load(here::here("outputs","all_geo_res.RData"))
# import layers world
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# projection
mol   <- paste0("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 ", "+units=m +no_defs")
world <- sf::st_transform(world, crs=mol)

# import layers border
mollBorder <- st_read(here::here("data","mollBorder","mollBorder.shp"))

# mask
mask.full=raster::raster(here::here("data","mask.full.tif"))

#My data
#all_geo_res[is.na(all_geo_res)] <- 0

var = c("richness_finalNT",
        "richness_finalNS",
        "richness_finalTH",
        "richness_initNT",
        "richness_initNS",
        "richness_initTH",
        "DeltaRank_SameWeight")



all_map <- lapply(1:length(var),function(x){

  if(! var[x] %in% c("DeltaRank_SameWeight")){ 
  
    mask = mask.full
    df <- all_geo_res[,var[x]]
    df[is.na(df)] <- 0
    mask[all_geo_res$ID] = df
    
  }

  else{ 
    #load(here::here("outputs","all_geo_res.RData"))
    mask = mask.full
    df <- all_geo_res[,var[x]]
    #df[is.na(df)] <- 0
    df <- data.frame(df,getValues(mask.full))
    colnames(df) <- c(var[x],"MPA")
    df[,var[x]][df$MPA==100] <- -1000000
    mask[all_geo_res$ID] = df[,1]
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
   
  if(var[x] %in% c("DeltaRank_SameWeight")) {
    mask.full.polygon$mask.full[mask.full.polygon$mask.full == -1000000] <- NA}
  
#'--------------------------------------------------------@Threatened
if (var[x] =="richness_initTH" || var[x] =="richness_finalTH" )  {  
  
  map <- ggplot(world) +
    geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
    #scale_alpha(range=c(0.5,0.5))+
    #scale_fill_manual(name = "mask.full", values = my_colors) +

    scale_fill_hp(option = "Ravenclaw", 
                  limits = c(min(c(all_geo_res$richness_initTH,all_geo_res$richness_finalTH)), 
                             max(c(all_geo_res$richness_initTH,all_geo_res$richness_finalTH))))+
    scale_color_hp(option = "Ravenclaw", 
                  limits = c(min(c(all_geo_res$richness_initTH,all_geo_res$richness_finalTH)), 
                             max(c(all_geo_res$richness_initTH,all_geo_res$richness_finalTH))))+
    geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
    geom_graticules(mol) +
    geom_mapframe(mol, colour = "white", size = 2.0) +
    #geom_mapframe(mol, colour = "black", size = 0.4) +
    
    ylab(" ") +
    xlab(" ") +
    
    #ggthemes::theme_map(base_family = "serif") +
    theme_bw()+
    theme(legend.position = "bottom",#c(0.85, 0.1),
          legend.direction = "horizontal",
          legend.title    = element_blank(), 
          plot.title      = element_text(face = "bold",  size = 18, hjust = 1),
          legend.text     = element_text(face = "plain", size = 8),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          #legend.key.height= unit(1.5, 'cm'),
          legend.key.width= unit(2, 'cm'),
          axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold")
          ) 
  if (var[x] =="richness_initTH")   { ggsave(file = here::here("figures/IUCN_InitialThreatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
  
  if (var[x] =="richness_finalTH")  { ggsave(file = here::here("figures/IUCN_FinalThreatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
  #rm(map)
  }
  
#'--------------------------------------------------------@Non-Threatened


 if (var[x] =="richness_initNT" || var[x] == "richness_finalNT")  { 
map <- ggplot(world)+
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
  #scale_alpha(range=c(0.5,0.5))+
  #scale_fill_manual(name = "mask.full", values = my_colors) +
  
  scale_fill_hp(option = "Ravenclaw", 
                limits = c(min(c(all_geo_res$richness_initNT,all_geo_res$richness_finalNT)), 
                           max(c(all_geo_res$richness_initNT,all_geo_res$richness_finalNT))))+
  scale_color_hp(option = "Ravenclaw", 
                 limits = c(min(c(all_geo_res$richness_initNT,all_geo_res$richness_finalNT)), 
                            max(c(all_geo_res$richness_initNT,all_geo_res$richness_finalNT))))+
  geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  #geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ylab(" ") +
  xlab(" ") +
  
  #ggthemes::theme_map(base_family = "serif") +
  theme_bw()+
  theme(legend.position = "bottom",#c(0.85, 0.1),
        legend.direction = "horizontal",
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18, hjust = 1),
        legend.text     = element_text(face = "plain", size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        #legend.key.height= unit(1.5, 'cm'),
        legend.key.width= unit(2, 'cm'),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")
  ) 
if (var[x] =="richness_initNT")   { ggsave(file = here::here("figures/IUCN_InitialNonThreatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)}

if (var[x] =="richness_finalNT")  { ggsave(file = here::here("figures/IUCN_FinalNonThreatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)}

#rm(map)
}

#'--------------------------------------------------------@No-Status


 if  (var[x] =="richness_initNS" || var[x] == "richness_finalNS")  {  
   map <- ggplot(world) +
     geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
     #scale_alpha(range=c(0.5,0.5))+
     #scale_fill_manual(name = "mask.full", values = my_colors) +
     
     scale_fill_hp(option = "Ravenclaw", 
                   limits = c(min(c(all_geo_res$richness_initNS,all_geo_res$richness_finalNS)), 
                              max(c(all_geo_res$richness_initNS,all_geo_res$richness_finalNS))))+
     scale_color_hp(option = "Ravenclaw", 
                    limits = c(min(c(all_geo_res$richness_initNS,all_geo_res$richness_finalNS)), 
                               max(c(all_geo_res$richness_initNS,all_geo_res$richness_finalNS))))+
     geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
     geom_graticules(mol) +
     geom_mapframe(mol, colour = "white", size = 2.0) +
     #geom_mapframe(mol, colour = "black", size = 0.4) +
     
     ylab(" ") +
     xlab(" ") +
     
     #ggthemes::theme_map(base_family = "serif") +
     theme_bw()+
     theme(legend.position = "bottom",#c(0.85, 0.1),
           legend.direction = "horizontal",
           legend.title    = element_blank(), 
           plot.title      = element_text(face = "bold",  size = 18, hjust = 1),
           legend.text     = element_text(face = "plain", size = 8),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border = element_blank(),
           #legend.key.height= unit(1.5, 'cm'),
           legend.key.width= unit(2, 'cm'),
           axis.text=element_text(size=14),
           axis.title=element_text(size=14,face="bold")
     ) 
   
   if (var[x] =="richness_initNS")   { ggsave(file = here::here("figures/IUCN_InitialNonStatus.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
   
   if (var[x] =="richness_finalNS")  { ggsave(file = here::here("figures/IUCN_FinalNonStatus.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
#rm(map)
}

#'--------------------------------------------------------@DeltaRankzonation
 if  (var[x] =="DeltaRank_SameWeight" )  {  

# adjustcolor( "chartreuse4", alpha.f = 0.7)
map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
   scale_fill_gradient2(midpoint= 0, low="#00AFBB", mid="white",
                        high="#FC4E07", space ="Lab",na.value = "palegreen3")+
  scale_color_gradient2(midpoint= 0, low="#00AFBB", mid="white",
                       high="#FC4E07", space ="Lab",na.value = "palegreen3")+
  geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  #geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ylab(" ") +
  xlab(" ") +
  
  #ggthemes::theme_map(base_family = "serif") +
  theme_bw()+
  theme(legend.position = "bottom",#c(0.85, 0.1),
        legend.direction = "horizontal",
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18, hjust = 1),
        legend.text     = element_text(face = "plain", size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        #legend.key.height= unit(1.5, 'cm'),
        legend.key.width= unit(2, 'cm'),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")
  ) 

#guides(fill = guide_legend(nrow = 1))
ggsave(file = here::here("figures/Figure6b.png"),map,width = 12, height = 8, units= "in",dpi= 300)
#rm(map)
 }
  

})
