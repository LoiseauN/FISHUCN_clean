rm(list = ls())
# load libraries
library(tidyr)
library(dplyr)

library(raster)
library(rgdal)

library(ggplot2)


# set wd for Z outputs
source(here::here("R","zonation_function.R"))

process_out_zonation <- function(nb_scenario){ 

    # nb_scenario = number of scenario
path = here::here("outputs", "output_zonation")

res <- lapply(1:nb_scenario, function(x){ 
  
## 3. Maps #####
# produce rasters with (arbitrary threshold) top priority cells within & outside PAs

# Retrieve PA mask. 
mask <- raster(here::here("outputs", "output_zonation","maskSea.tif"))
summary(mask)
table(mask[]) # proportion of cells that have >100 ha protection: start here for small sites
#  0 are  unprotected cells, randomly sampled (same number as protected cells)
# this map does not cover the full extent 

#setwd("ZonationOUT/")
#source("../scripts/functions.R")

rank.files <- list.files(path, pattern = "rank.compressed.tif$",full.names=TRUE)

R0 <- raster(rank.files[x])

mask.full <- raster::mask(R0, mask, maskvalue = 1, updatevalue = 100  )
max(mask.full[], na.rm = T)
mask.full[mask.full < 100] <- 0

info <- table(mask.full[])
prop.prot <- info[[2]]/sum(info) ## to know where to start ranking non-protected cells



#Top5.expand <-  reclassify(R0, c(0, 1-prop.prot-0.05, 0,  1-prop.prot-0.05, 1-prop.prot, 1,   1-prop.prot, 1, 2)) 
#table(Top5.expand[])
#writeRaster(Top5.expand, file = "top5_expandSc3b.tif", overwrite = T)

rast <- setValues(R0,seq(1,6508832))

Zvalues <- reclassify(R0,c(1-prop.prot,1,2))

RankingScenario <- rank(Zvalues[which(Zvalues[]<2)])
rastCells <- rast[which(Zvalues[]<2)]

ranking = data.frame(RankingScenario = RankingScenario ,ID = rastCells)
if (x = 1) {colnames(ranking)[2] <- "IUCN_weigth"}
else{colnames(ranking)[1]  <- "Predict_IUCN_same_weigth"}


}
return(ranking)
)

}

