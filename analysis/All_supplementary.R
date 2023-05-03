#ANN and RF outputs using consensus




#Phylogeny



#Ranking Proba
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



var = c("DeltaRank_SameWeight")

mask = mask.full
df <- all_geo_res[,var[x]]
df[is.na(df)] <- 0
df <- data.frame(df,getValues(mask.full))
colnames(df) <- c(var[x],"MPA")
df[,var[x]][df$MPA==100] <- -1000000
mask[all_geo_res$ID] = df[,1]


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



#'---------------------------------------------------------------------@Checkperfamily
load(file = here::here("outputs", "FB_final.RData"))

res <- merge(dat_network, FB_final[,c("Genus","Family")], by.x="species", by.y="row.names")


res <- res[!is.na(res$predict_complementary) | res$IUCN_final == "No Status", ]

res$IUCN_final <- as.factor(res$IUCN_final )
res$Family <- as.factor(res$Family )
res <- as.data.frame.matrix(t(table(res$IUCN_final,res$Family)))
res$Family <- rownames(res)
res$richness <- res[,1] + res[,2] + res[,3]


res <- reshape2::melt(res, id.vars=c("Family","richness"))
res <-  res[order(res$richness,decreasing = T),]
res <- res[res$richness>0,]

ggplot(res,aes(x = reorder(Family, richness), y= value, fill = variable)) +
  geom_bar(stat = "identity",position ="stack")+ 
  coord_flip()

ggplot(df_long, aes(x = as.numeric(reorder(Family, richness)), y = value, fill = id))+
  geom_bar(stat = "identity", position = "dodge", width = 0.7)+
  scale_fill_manual(values=c("red", "black")) + 
  theme_bw()+
  facet_zoom(xy = Site_reorder%in%c("site14", "site9", "site8", "site1", "site11"), horizontal=FALSE) + 
  scale_x_continuous(
    breaks = 1:length(levels(df_long$Site_reorder)),
    label = levels(df_long$Site_reorder)
  )+
  labs(x = "", y = " ")+
  theme(legend.position = c(0.90, 0.90), legend.background = element_rect(fill="transparent"),
        legend.title = element_blank(), axis.text.x=element_text(angle=55, vjust=1,  hjust=1,size = 8))
  
  
  

  library(tidyr) 
  library(dplyr)
  library(ggplot2)
  library(ggforce)
  df_reorder <-  transform(df, 
                           Site_reorder = reorder(Site, -measured))
  df_long <- df_reorder %>% gather("id", "value", 2:3) 
  
  
  ggplot(df_long, aes(x = Site_reorder, y = value, fill = id))+
    geom_bar(stat = "identity", position = "dodge", width = 0.7)+
    scale_fill_manual(values=c("red", "black")) + 
    theme_bw()+
    facet_zoom(x = Site_reorder%in%c("site14", "site9", "site8", "site1", "site11"))+
    labs(x = "", y = " ")+
    theme(legend.position = c(0.90, 0.90), legend.background = element_rect(fill="transparent"),
          legend.title = element_blank(), axis.text.x=element_text(angle=55, vjust=1,  hjust=1,size = 8))
