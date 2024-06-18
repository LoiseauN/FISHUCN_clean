
#'-------------------------------------------
source(here::here("R","map_function.R"))
load(here::here("outputs","all_geo_res_consensus.RData"))
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
#all_geo_res_consensus[is.na(all_geo_res_consensus)] <- 0

all_geo_res_consensus$deltaTH <- all_geo_res_consensus$richness_finalTHR - all_geo_res_consensus$richness_initTHR
all_geo_res_consensus$deltaNT <- all_geo_res_consensus$richness_finalNonTHR - all_geo_res_consensus$richness_initNonTHR
all_geo_res_consensus$deltaNS <- all_geo_res_consensus$richness_finalNoStatus - all_geo_res_consensus$richness_initNoStatus


var = c( "deltaTH",
         "deltaNT",
         "deltaNS")


all_map <- lapply(1:length(var),function(x){
  print(paste0(x,", ", round(x/length(var),1)*100, "%"))
  
 
    
    mask = mask.full
    df <- all_geo_res_consensus[,var[x]]
    df[is.na(df)] <- 0
    mask[all_geo_res_consensus$ID] = df
    

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
    
    if (var[x] =="deltaTH")   { ggsave(file = here::here("figures/deltaTH_consensus.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
    
    if (var[x] =="deltaNT")  { ggsave(file = here::here("figures/deltaNT_consensus.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
    
    if (var[x] =="deltaNS")  { ggsave(file = here::here("figures/deltaNS_consensus.png"),map,width = 12, height = 8, units= "in",dpi= 300)}
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
  
  
})
