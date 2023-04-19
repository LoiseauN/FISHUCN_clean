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



DeltaRank_Proba