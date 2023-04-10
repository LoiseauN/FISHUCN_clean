remotes::install_github("ropensci/rnaturalearthhires")
#install.packages(c("rnaturalearth", "rnaturalearthdata"))
library("rnaturalearth")
library("rnaturalearthdata")

#'-------------------------------------------
source(here::here("R","map_function.R"))

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
all_geo_res$Perthrbefore <- all_geo_res$Rthr/(all_geo_res$Rthr+all_geo_res$Rnothr+all_geo_res$Rnostatus)
all_geo_res$Perthrfinal <- all_geo_res$Rfinalthr/(all_geo_res$Rfinalthr+all_geo_res$Rfinalnothr+all_geo_res$Rfinalnostatus)

all_geo_res$Pernothrbefore <- all_geo_res$Rnothr/(all_geo_res$Rthr+all_geo_res$Rnothr+all_geo_res$Rnostatus)
all_geo_res$Pernothrfinal <- all_geo_res$Rfinalnothr/(all_geo_res$Rfinalthr+all_geo_res$Rfinalnothr+all_geo_res$Rfinalnostatus)

all_geo_res$Pernostatusbefore <- all_geo_res$Rnostatus/(all_geo_res$Rthr+all_geo_res$Rnothr+all_geo_res$Rnostatus)
all_geo_res$Pernostatusfinal <- all_geo_res$Rfinalnostatus/(all_geo_res$Rfinalthr+all_geo_res$Rfinalnothr+all_geo_res$Rfinalnostatus)

all_geo_res$richness <-  all_geo_res$Rthr+all_geo_res$Rnothr+all_geo_res$Rnostatus


all_geo_res[is.na(all_geo_res)] <- 0


var = c("Rthr","Rnothr","Rnostatus","Rfinalthr","Rfinalnothr",
        "Rfinalnostatus","DeltaRank")



all_map <- lapply(1:length(var),function(x){

  if(! var[x] %in% c("DeltaRank","rankSc1","rankSc2")){ 
  
    mask = mask.full
    df <- all_geo_res[,var[x]]
    df[is.na(df)] <- 0
    mask[all_geo_res$ID] = df
    
  }

  else{ 
    mask = mask.full
    df <- all_geo_res[,var[x]]
    df[is.na(df)] <- 0
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
   
  if(var[x] %in% c("DeltaRank","rankSc1","rankSc2")) {
    mask.full.polygon$mask.full[mask.full.polygon$mask.full == -1000000] <- NA}
  #pal <- wes_palette("Zissou1", max(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T), type = "continuous")
  
#'--------------------------------------------------------@Threatened
if (var[x] =="Rthr" || var[x] =="Rfinalthr" )  {  
  
  
  map <- ggplot(world) +
    geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
    scale_fill_distiller(palette='RdYlBu',limits = c(min(c(all_geo_res$Rthr,all_geo_res$Rfinalthr)), 
                                                    max(c(all_geo_res$Rthr,all_geo_res$Rfinalthr)))) + 
    scale_color_distiller(palette='RdYlBu',limits = c(min(c(all_geo_res$Rthr,all_geo_res$Rfinalthr)), 
                                                      max(c(all_geo_res$Rthr,all_geo_res$Rfinalthr)))) + 
    scale_alpha(range=c(0.5,0.5))+
    #scale_fill_manual(name = "mask.full", values = my_colors) +
    #scale_fill_hp(option = "Ravenclaw", 
    #              limits = c(min(all_geo_res$DeltaRank),na.rm=T),
    #                         max(all_geo_res$DeltaRank,na.rm=T))+
    geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
    geom_graticules(mol) +
    geom_mapframe(mol, colour = "white", size = 2.0) +
    #geom_mapframe(mol, colour = "black", size = 0.4) +
    
    
    ylab(" ") +
    xlab(" ") +
    
    #ggthemes::theme_map(base_family = "serif") +
    theme_bw()+
    theme(legend.position = c(0.85, 0.1),
          legend.direction = "horizontal",
          legend.title    = element_blank(), 
          plot.title      = element_text(face = "bold",  size = 18, hjust = 1),
          legend.text     = element_text(face = "plain", size = 8),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          # legend.key.height= unit(1.5, 'cm'),
          #legend.key.width= unit(2, 'cm')
          #axis.text=element_text(size=14),
          #axis.title=element_text(size=14,face="bold")
          ) 
  #ggsave(file = here::here("figures/IUCN_Threatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)
  #rm(map)
  }
  
#'--------------------------------------------------------@Non-Threatened

 if (var[x] =="Rnothr" || var[x] == "Rfinalnothr")  { 
map <- ggplot(world) +
geom_sf(data = mask.full.polygon, aes(fill = mask.full), color = NA) +
  scale_fill_gradientn(colours = pal, 
                       limits = c(min(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T), 
                                  max(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T)))+
  scale_colour_gradientn(colours = pal, 
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

#ggsave(file = here::here("figures/IUCN_NonThreatened.png"),map,width = 12, height = 8, units= "in",dpi= 300)
#rm(map)
}

#'--------------------------------------------------------@No-Status
 if  (var[x] =="Rnostatus" var[x] =="Rfinalnostatus")  {  title =  "BEFORE No-Status"  
pal <- wes_palette("Zissou1", max(c(all_geo_res$Rnostatus,all_geo_res$Rfinalnostatus),na.rm=T), type = "continuous")

map <- ggplot(world) +
geom_sf(data = mask.full.polygon, aes(fill = mask.full), color = NA) +
  scale_fill_gradientn(colours = pal, 
                        limits = c(min(c(all_geo_res$Rnostatus,all_geo_res$Rfinalnostatus),na.rm=T), 
                                   max(c(all_geo_res$Rnostatus,all_geo_res$Rfinalnostatus),na.rm=T)))+
  scale_colour_gradientn(colours = pal, 
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

ggsave(file = here::here("figures/IUCN_NoStatus.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}





#'--------------------------------------------------------@DeltaRankzonation
 if  (var[x] =="DeltaRank" )  {  title =  "Change in prioritization ranking" 
# adjustcolor( "chartreuse4", alpha.f = 0.7)
map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
   scale_fill_gradient2(midpoint= 0, low="#00AFBB", mid="white",
                        high="#FC4E07", space ="Lab",na.value = "#458B00B3")+
  scale_color_gradient2(midpoint= 0, low="#00AFBB", mid="white",
                       high="#FC4E07", space ="Lab",na.value = "#458B00B3")+
  #scale_fill_manual(name = "mask.full", values = my_colors) +
  #scale_fill_hp(option = "Ravenclaw", 
  #              limits = c(min(all_geo_res$DeltaRank),na.rm=T),
  #                         max(all_geo_res$DeltaRank,na.rm=T))+
  geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  ylab(" ") +
  xlab(" ") +
  
  #ggthemes::theme_map(base_family = "serif") +
  theme_bw()+
  theme(legend.position = c(0.85, 0.1),
        legend.direction = "horizontal",
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18, hjust = 1),
        legend.text     = element_text(face = "plain", size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
       # legend.key.height= unit(1.5, 'cm'),
        #legend.key.width= unit(2, 'cm')
        #axis.text=element_text(size=14),
        #axis.title=element_text(size=14,face="bold")
        ) 

#guides(fill = guide_legend(nrow = 1))
ggsave(file = here::here("figures/Figure6b.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}



    

  
})




