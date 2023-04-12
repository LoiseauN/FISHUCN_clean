rastlist <- list.files(here::here("outputs/tif_outputs"), pattern='.tif', 
                       all.files=TRUE, full.names=FALSE)

#allrasters <- lapply(rastlist, raster)

#first import all files in a single folder as a list 
rastlist <- list.files(path = here::here("outputs/tif_outputs"), pattern='.tif', 
                       all.files=TRUE, full.names=FALSE)

rastlist <- unlist(lapply(1:length(rastlist), function(x){
  paste0(here::here("outputs/tif_outputs"),"/",rastlist[x])
}))

allrasters <- stack(rastlist)

# proj.4 projection description
newproj <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"

#simplest approach
pr1 <- projectRaster(r, crs=newproj)

#Having Lat and long
crs(allrasters) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
projection(allrasters)

# Convert raster to SpatialPointsDataFrame
r.pts <- rasterToPoints(allrasters, spatial=TRUE)
proj4string(r.pts)

# reproject sp object
#geo.prj <- paste0("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84")
geo.prj <- "EPSG:4326"
r.pts <- spTransform(r.pts, CRS(geo.prj)) 
proj4string(r.pts)

# Assign coordinates to @data slot, display first 6 rows of data.frame
r.pts@data <- data.frame(r.pts@data, long=coordinates(r.pts)[,1],
                         lat=coordinates(r.pts)[,2])                         

nrow(r.pts@data)
#df <-  data.frame(getValues(allrasters))
df  <- data.frame(ID = seq(from = 1, to = nrow(r.pts@data),by=1),
                  Rthr = r.pts@data$richness_initTHR,
                  Rnothr = r.pts@data$richness_initNonTHR,
                  Rnostatus = r.pts@data$richness_initNoStatus,
                  Rfinalthr = r.pts@data$richness_finalTHR,
                  Rfinalnothr = r.pts@data$richness_finalNonTHR,
                  Rfinalnostatus = r.pts@data$richness_finalNoStatus,
                  long = r.pts@data$long,
                  lat = r.pts@data$lat
                  )



#df  <-  merge(Zrank_main,df,by = "ID",all.x=T)

df  <-  merge(Zrank_main,df,by = "ID",all=T)
df$DeltaRank   <-  df$rankSc2-df$rankSc1
df$DeltaThr  <-   df$Rfinalthr-df$Rthr



df  <-  merge(df,all_geo_res[,c("ID","MPA")],by = "ID",all.x=T)

df$richness <-  df$Rthr+df$Rnothr+df$Rnostatus



all_geo_res <- df
save(all_geo_res,file=here::here("outputs","all_geo_res.RData"))


