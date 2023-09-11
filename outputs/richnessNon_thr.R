library(raster)
library(tidyverse)



# final
IUCN_cat_NONthr <- filter(dat_network,IUCN_final=="Non Threatened")

rasterFish <- list.files(here::here('raster_sp','rasterFish'),pattern=".tif$",full.names=TRUE)

rasterFishIUCN_cat_NONthr <- rasterFish[unlist(lapply(IUCN_cat_NONthr$species,grep,rasterFish))]

rast = stack(rasterFishIUCN_cat_NONthr)

beginCluster(53)

R_IUCNcat_NONthr = raster::clusterR(rast,fun = calc,args=list(sum,na.rm=T),verbose=TRUE)

writeRaster(R_IUCNcat_NONthr,"richness_finalNonTHR.tif",format="GTiff")

endCluster()


# intit
IUCN_cat_NONthr <- filter(dat_network,IUCN_cat=="Non Threatened")

rasterFish <- list.files(here::here('raster_sp','rasterFish'),pattern=".tif$",full.names=TRUE)

rasterFishIUCN_cat_NONthr <- rasterFish[unlist(lapply(IUCN_cat_NONthr$species,grep,rasterFish))]

rast = stack(rasterFishIUCN_cat_NONthr)

beginCluster(53)

R_IUCNcat_NONthr = raster::clusterR(rast,fun = calc,args=list(sum,na.rm=T),verbose=TRUE)

writeRaster(R_IUCNcat_NONthr,"richness_initNonTHR.tif",format="GTiff")

endCluster()

