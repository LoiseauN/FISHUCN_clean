#' Merging all resultats
#'
#' This script to merge all data for each cell. 
#'
#' @author  Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com},
#'         
#'
#' @date 

#Read in all TIFF files of richness per status
files <- list.files(here::here("outputs", "tif_outputs"), pattern = "\\.tif$",
                    full.names = TRUE)

#Load zonation outputs
load(here::here("outputs", "Zrank_main.RData"))

#Combine all TIFF files into a single raster stack
ras  <- raster::stack(files)

#Get the x,y coordinates of all raster cells
xy   <- raster::xyFromCell(ras, 1:raster::ncell(ras))
vals <- ras[]

#Combine the x,y coordinates and cell values into a dataframe
dat <- data.frame("cell_id" = 1:nrow(xy), xy, vals)

#Computed overall richness
dat$richness <-  dat$richness_finalNonTHR+dat$richness_finalNoStatus+dat$richness_finalTHR

#Remove any rows from "dat" where "richness" is less than 
dat <- dat[which(dat[ , "richness"] > 0), ]

#Convert "dat" to a spatial dataframe called "dat_sf" with coordinate reference system (CRS) information.
dat_sf <- sf::st_as_sf(dat, coords = c("x", "y"), crs = raster::proj4string(ras))
dat_sf <- sf::st_transform(dat_sf, crs = 4326)

#Extract the coordinate data
dat <- data.frame(sf::st_coordinates(dat_sf), sf::st_drop_geometry(dat_sf))

#Rename the columns
colnames(dat)<- c("long","lat","ID",
                 "richness_finalNT",
                  "richness_finalNS",
                  "richness_finalTH",
                  "richness_initNT",
                  "richness_initNS",
                  "richness_initTH",
                  "richness")

#Merge data with zonation outputs : Zrank_main
all_geo_res <- merge(dat,Zrank_main, by ="ID",all.x = T)

#Compute difference in zonation rank
all_geo_res$DeltaRank <- all_geo_res$rankSc2-all_geo_res$rankSc1
all_geo_res[is.na(all_geo_res)] <- 0

#Save the final dataset
save(all_geo_res,file=here::here("outputs","all_geo_res.RData"))
