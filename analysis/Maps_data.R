
#'-------------------------------------------
source(here::here("R","map_function.R"))
load(here::here("outputs","all_geo_res.RData"))
# import layers world
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# projection
mol   <- paste0("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 ", "+units=m +no_defs")
world <- sf::st_transform(world, crs=mol)

## Mollweide projection - Pacific-centered ----
#mol <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#world <-  robinmap(center = 160, crs = mol)

# import layers border
mollBorder <- sf::st_read(here::here("data","mollBorder","mollBorder.shp"))

# mask
mask.full=raster::raster(here::here("data","mask.full.tif"))

#My data
#all_geo_res[is.na(all_geo_res)] <- 0

all_geo_res$deltaTH <- all_geo_res$richness_finalTHR - all_geo_res$richness_initTHR
all_geo_res$deltaNT <- all_geo_res$richness_finalNonTHR - all_geo_res$richness_initNonTHR
all_geo_res$deltaNS <- all_geo_res$richness_finalNoStatus - all_geo_res$richness_initNoStatus

all_geo_res <-  all_geo_res %>%
  mutate(cate_DeltaRank_SameWeight = cut(DeltaRank_SameWeight, 100)) 

all_geo_res <- all_geo_res %>% 
  mutate(
    cate_DeltaRank_SameWeight = 
      DeltaRank_SameWeight %>%
      Hmisc::cut2(g=500) # Note, cut2 comes from the Hmisc package
  )
all_geo_res$cate_DeltaRank_SameWeight %>% 
  summary()


#all_geo_res$cate_DeltaRank_SameWeight <- cut(all_geo_res$DeltaRank_SameWeight, breaks = 100)



var = c( #"deltaTH",
  # "deltaNT",
  #"deltaNS"#,
  "richness_finalNonTHR",
   "richness_finalNoStatus",
   "richness_finalTHR",
    "richness_initNonTHR",
    "richness_initNoStatus",
    "richness_initTHR"# ,
  #     "DeltaRank_SameWeight")
#  "cate_DeltaRank_SameWeight")
#,        "richness_unpredictable"
)


all_map <- lapply(1:length(var),function(x){
print(paste0(x,", ", round(x/length(var),1)*100, "%"))
  
  if(! var[x] %in% c("DeltaRank_SameWeight","DeltaRank_Proba","cate_DeltaRank_SameWeight")){ 
  
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
   
  if(var[x] %in% c("DeltaRank_SameWeight","DeltaRank_Proba","cate_DeltaRank_SameWeight")) {
    mask.full.polygon$mask.full[mask.full.polygon$mask.full == -1000000] <- NA}
  
#'--------------------------------------------------------@Threatened
if (var[x] =="richness_initTHR" || var[x] =="richness_finalTHR" )  {  
  
  map <- ggplot(world) +
    geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
    scale_colour_gradientn(name  = "Richness Threatened",
      colours = colorRampPalette(rev(brewer.pal(n = 8, name = "RdBu")))(100)[-c(2:15, 85:100)], 
             limits = c(min(c(all_geo_res$richness_initTHR,all_geo_res$richness_finalTHR),na.rm = T), 
                      max(c(all_geo_res$richness_initTHR,all_geo_res$richness_finalTHR),na.rm = T))) +                                       
    
    scale_fill_gradientn(name  = "Richness Threatened",
      colours = colorRampPalette(rev(brewer.pal(n = 8, name = "RdBu")))(100)[-c(2:15, 85:100)], 
             limits = c(min(c(all_geo_res$richness_initTHR,all_geo_res$richness_finalTHR),na.rm = T), 
                      max(c(all_geo_res$richness_initTHR,all_geo_res$richness_finalTHR),na.rm = T))) +

    #scale_fill_hp(option = "Ravenclaw", 
           #       limits = c(min(c(all_geo_res$richness_initTH,all_geo_res$richness_finalTH)), 
                #             max(c(all_geo_res$richness_initTH,all_geo_res$richness_finalTH))))+
   # scale_color_hp(option = "Ravenclaw", 
        #          limits = c(min(c(all_geo_res$richness_initTH,all_geo_res$richness_finalTH)), 
           #                  max(c(all_geo_res$richness_initTH,all_geo_res$richness_finalTH))))+
    geom_sf(data = world, fill = "white", color = "#bebebe", size = 0.1) +
    geom_graticules(mol) +
    geom_mapframe(mol, colour = "white", size = 2.0) +
    #geom_mapframe(mol, colour = "black", size = 0.4) +
    
    ylab(" ") +
    xlab(" ") +
    
    #ggthemes::theme_map(base_family = "serif") +
    theme_bw()+
    theme(legend.position = "bottom",#c(0.85, 0.1),
          legend.direction = "horizontal",
          legend.title    =  element_text(face = "plain", size = 32,vjust = 1,hjust = 0.5),
          legend.text     = element_text(face = "plain", size = 32),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          #legend.key.height= unit(1.5, 'cm'),
          legend.key.width= unit(4, 'cm'),
          axis.text=element_text(size=18),
          axis.title=element_text(size=20),
          legend.box="horizontal") +
          guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
          size = guide_legend(title.position="top", title.hjust = 0.5))
  
  if (var[x] =="richness_initTHR")   { ggsave(file = here::here("figures/IUCN_InitialThreatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
  
  if (var[x] =="richness_finalTHR")  { ggsave(file = here::here("figures/IUCN_FinalThreatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
  
  }
  
#'--------------------------------------------------------@Non-Threatened
 if (var[x] =="richness_initNonTHR" || var[x] == "richness_finalNonTHR")  { 
  map <- ggplot(world)+
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
  scale_colour_gradientn(name  = "Richness Not-Threatened", 
                         colours = colorRampPalette(rev(brewer.pal(n = 8, name = "RdBu")))(100)[-c(2:15, 85:100)], 
                         limits = c(min(c(all_geo_res$richness_initNonTHR,all_geo_res$richness_finalNonTHR),na.rm = T), 
                                    max(c(all_geo_res$richness_initNonTHR,all_geo_res$richness_finalNonTHR),na.rm = T)))+                                       
  scale_fill_gradientn(name  = "Richness Not-Threatened", 
                        colours = colorRampPalette(rev(brewer.pal(n = 8, name = "RdBu")))(100)[-c(2:15, 85:100)],
                       limits = c(min(c(all_geo_res$richness_initNonTHR,all_geo_res$richness_finalNonTHR),na.rm = T), 
                                 max(c(all_geo_res$richness_initNonTHR,all_geo_res$richness_finalNonTHR),na.rm = T))) +
  geom_sf(data = world, fill = "white", color = "#bebebe", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +

  ylab(" ") +
  xlab(" ") +
  
  theme_bw()+
    theme(legend.position = "bottom",#c(0.85, 0.1),
          legend.direction = "horizontal",
          legend.title    =  element_text(face = "plain", size = 32,vjust = 1,hjust = 0.5),
          legend.text     = element_text(face = "plain", size = 32),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          #legend.key.height= unit(1.5, 'cm'),
          legend.key.width= unit(4, 'cm'),
          axis.text=element_text(size=18),
          axis.title=element_text(size=20),
          legend.box="horizontal") +
          guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
          size = guide_legend(title.position="top", title.hjust = 0.5))
  
if (var[x] =="richness_initNonTHR")   {ggsave(file = here::here("figures/IUCN_InitialNonThreatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)}

if (var[x] =="richness_finalNonTHR")  {ggsave(file = here::here("figures/IUCN_FinalNonThreatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
}

#'--------------------------------------------------------@No-Status


 if  (var[x] =="richness_initNoStatus" || var[x] == "richness_finalNoStatus")  {  
   map <- ggplot(world) +
     geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
     scale_colour_gradientn(name  = "Richness DDNE",
       colours = colorRampPalette(rev(brewer.pal(n = 8, name = "RdBu")))(100)[-c(2:15, 85:100)], 
                     limits = c(min(c(all_geo_res$richness_initNoStatus,all_geo_res$richness_finalNoStatus),na.rm = T), 
                               max(c(all_geo_res$richness_initNoStatus,all_geo_res$richness_finalNoStatus),na.rm = T)))+                                       
     
     scale_fill_gradientn(name  = "Richness DDNE",
       colours = colorRampPalette(rev(brewer.pal(n = 8, name = "RdBu")))(100)[-c(2:15, 85:100)], 
                     limits = c(min(c(all_geo_res$richness_initNoStatus,all_geo_res$richness_finalNoStatus),na.rm = T), 
                               max(c(all_geo_res$richness_initNoStatus,all_geo_res$richness_finalNoStatus),na.rm = T))) +
     
    # scale_fill_hp(option = "Ravenclaw", 
     #              limits = c(min(c(all_geo_res$richness_initNS,all_geo_res$richness_finalNS)), 
      #                        max(c(all_geo_res$richness_initNS,all_geo_res$richness_finalNS))))+
    # scale_color_hp(option = "Ravenclaw", 
      #              limits = c(min(c(all_geo_res$richness_initNS,all_geo_res$richness_finalNS)), 
     #                          max(c(all_geo_res$richness_initNS,all_geo_res$richness_finalNS))))+
     geom_sf(data = world, fill = "white", color = "#bebebe", size = 0.1) +
     geom_graticules(mol) +
     geom_mapframe(mol, colour = "white", size = 2.0) +
     #geom_mapframe(mol, colour = "black", size = 0.4) +
     
     ylab(" ") +
     xlab(" ") +
     
     #ggthemes::theme_map(base_family = "serif") +
     theme_bw()+
     theme(legend.position = "bottom",#c(0.85, 0.1),
           legend.direction = "horizontal",
           legend.title    =  element_text(face = "plain", size = 32,vjust = 1,hjust = 0.5),
           legend.text     = element_text(face = "plain", size = 32),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border = element_blank(),
           #legend.key.height= unit(1.5, 'cm'),
           legend.key.width= unit(4, 'cm'),
           axis.text=element_text(size=18),
           axis.title=element_text(size=20),
           legend.box="horizontal") +
           guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
           size = guide_legend(title.position="top", title.hjust = 0.5))
   
   if (var[x] =="richness_initNoStatus")   { ggsave(file = here::here("figures/IUCN_InitialNonStatus.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
   
   if (var[x] =="richness_finalNoStatus")  { ggsave(file = here::here("figures/IUCN_FinalNonStatus.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
#rm(map)
}
  
  
  
  
  #'--------------------------------------------------------@DeltaRICHNESS
  if  (var[x] =="deltaTH" || var[x] == "deltaNT" || var[x] == "deltaNS")  {  
  
    if (var[x] =="deltaTH")   { title = "Threatened after - before"}
  if (var[x] =="deltaNT")  {title = "Non Threatened after - before"}
  if (var[x] =="deltaNS")  { title = "DDNE after - before"}
   
    if  (var[x] =="deltaTH" || var[x] == "deltaNT"  ) {colormap = c("white",colorRampPalette(brewer.pal(n = 8, name = "Oranges"))(100))}
    if  (var[x] =="deltaNS") { colormap = c("white",rev(colorRampPalette(brewer.pal(n = 8, name = "Blues"))(100)))}
    
     map <- ggplot(world) +
      geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
      
    
      #scale_colour_gradientn(name  = title,
      #                       colours = colorRampPalette(rev(brewer.pal(n = 8, name = "RdBu")))(100)[-c(2:15, 85:100)])+
    
     # scale_fill_gradientn(name  = title,
      #                   colours = colorRampPalette(rev(brewer.pal(n = 8, name = "RdBu")))(100)[-c(2:15, 85:100)])+

      scale_colour_gradientn(name  = title,
                              colours = colormap)+
       
      scale_fill_gradientn(name  = title,
                           colours = colormap)+
     
  
    geom_sf(data = world, fill = "white", color = "#bebebe", size = 0.1) +
      geom_graticules(mol) +
      geom_mapframe(mol, colour = "white", size = 2.0) +
      #geom_mapframe(mol, colour = "black", size = 0.4) +
      
      ylab(" ") +
      xlab(" ") +
      
      #ggthemes::theme_map(base_family = "serif") +
      theme_bw()+
      theme(legend.position = "bottom",#c(0.85, 0.1),
            legend.direction = "horizontal",
            legend.title    =  element_text(face = "plain", size = 32,vjust = 1,hjust = 0.5),
            legend.text     = element_text(face = "plain", size = 32),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            #legend.key.height= unit(1.5, 'cm'),
            legend.key.width= unit(4, 'cm'),
            axis.text=element_text(size=18),
            axis.title=element_text(size=20),
            legend.box="horizontal") +
      guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
             size = guide_legend(title.position="top", title.hjust = 0.5))
    
    if (var[x] =="deltaTH")   { ggsave(file = here::here("figures/deltaTH.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
    
    if (var[x] =="deltaNT")  { ggsave(file = here::here("figures/deltaNT.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
    
    if (var[x] =="deltaNS")  { ggsave(file = here::here("figures/deltaNS.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
  }
  
  
  
  
  
  
  
  
  
  
#'--------------------------------------------------------@DeltaRankzonation
 if  (var[x] =="DeltaRank_SameWeight" || var[x] == "DeltaRank_Proba"|| var[x] == "cate_DeltaRank_SameWeight")  {  
   mask.full.polygon.plot <- mask.full.polygon %>% 
     mutate(mask.full = Hmisc::cut2(mask.full,g=500), include.lowest = T)
   
   colormap = c(rev(colorRampPalette(brewer.pal(n = 8, name = "Blues"))(334)),'white',colorRampPalette(brewer.pal(n = 8, name = "Oranges"))(164))
   
   map <- ggplot(world) +
     geom_sf(data = mask.full.polygon.plot, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
     
     scale_colour_manual(na.value = "#66CDAA66",name  = "Rank after - rank before",
                         values = colormap)+
     
     scale_fill_manual(na.value = "#66CDAA66",name  = "Rank after - rank before",
                       values = colormap)+
     
     geom_sf(data = world, fill = "white", color = "#bebebe", size = 0.1) +
     geom_graticules(mol) +
     geom_mapframe(mol, colour = "white", size = 2.0) +
     #geom_mapframe(mol, colour = "black", size = 0.4) +
     
     ylab(" ") +
     xlab(" ") +
     
     #ggthemes::theme_map(base_family = "serif") +
     theme_bw()+
     theme(legend.position = "none",#bottom",#c(0.85, 0.1),
           #legend.direction = "horizontal",
           #legend.title    =  element_text(face = "plain", size = 32,vjust = 1,hjust = 0.5),
           #legend.text     = element_text(face = "plain", size = 32),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border = element_blank(),
           #legend.key.height= unit(1.5, 'cm'),
           #legend.key.width= unit(4, 'cm'),
           axis.text=element_text(size=18),
           axis.title=element_text(size=20),
           #legend.box="horizontal"
     ) +
     guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
            size = guide_legend(title.position="top", title.hjust = 0.5))
   
   #ggsave(file = here::here("figures/cate_Figure6b.png"),map,width = 12, height = 8, units= "in",dpi= 300)
   
   
   
 pals::pal.bands(c(rev(colorRampPalette(brewer.pal(n = 8, name = "Blues"))(334)),'white',colorRampPalette(brewer.pal(n = 8, name = "Oranges"))(164)),
           main = "Rank after - rank before")
   
   
if (var[x] =="DeltaRank_SameWeight")   { ggsave(file = here::here("figures/Figure6b.png"),map,width = 12, height = 8, units= "in",dpi= 300)}

if (var[x] =="cate_DeltaRank_SameWeight")   { ggsave(file = here::here("figures/cate_Figure6b.png"),map,width = 12, height = 8, units= "in",dpi= 300)}


if (var[x] =="DeltaRank_Proba")  { ggsave(file = here::here("figures/Figure6Supplentary.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
}
 
  
#'--------------------------------------------------------@RichnessUnpredictable
  if  (var[x] =="richness_unpredictable")  {  
    map <- ggplot(world) +
      geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
      scale_colour_gradientn(name  = "Richness unpredictable",
                             colours = colorRampPalette(rev(brewer.pal(n = 8, name = "RdBu")))(100)[-c(2:15, 85:100)])+                                       
      
      scale_fill_gradientn(name  = "Richness unpredictable",
                           colours = colorRampPalette(rev(brewer.pal(n = 8, name = "RdBu")))(100)[-c(2:15, 85:100)]) +
      
      # scale_fill_hp(option = "Ravenclaw", 
      #              limits = c(min(c(all_geo_res$richness_initNS,all_geo_res$richness_finalNS)), 
      #                        max(c(all_geo_res$richness_initNS,all_geo_res$richness_finalNS))))+
      # scale_color_hp(option = "Ravenclaw", 
      #              limits = c(min(c(all_geo_res$richness_initNS,all_geo_res$richness_finalNS)), 
      #                          max(c(all_geo_res$richness_initNS,all_geo_res$richness_finalNS))))+
      geom_sf(data = world, fill = "white", color = "#bebebe", size = 0.1) +
      geom_graticules(mol) +
      geom_mapframe(mol, colour = "white", size = 2.0) +
      #geom_mapframe(mol, colour = "black", size = 0.4) +
      
      ylab(" ") +
      xlab(" ") +
      
      #ggthemes::theme_map(base_family = "serif") +
      theme_bw()+
      theme(legend.position = "bottom",#c(0.85, 0.1),
            legend.direction = "horizontal",
            legend.title    =  element_text(face = "plain", size = 32,vjust = 1,hjust = 0.5),
            legend.text     = element_text(face = "plain", size = 32),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            #legend.key.height= unit(1.5, 'cm'),
            legend.key.width= unit(4, 'cm'),
            axis.text=element_text(size=18),
            axis.title=element_text(size=20),
            legend.box="horizontal") +
      guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
             size = guide_legend(title.position="top", title.hjust = 0.5))
    
    if (var[x] =="richness_unpredictable")   { ggsave(file = here::here("figures/IUCN_unpredictable.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
    
    #rm(map)
  }
  })
