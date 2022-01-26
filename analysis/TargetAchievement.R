
library(maptools)
library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(sf)



setwd("~/Documents/Postdoc MARBEC/FISHUCN/last/FISHUCN_clean/data")
#mapMPA<-readOGR(dsn=".",layer="WDPA_Feb2018_marine-shapefile-polygons")
mapMPA<-st_read(dsn=".",layer="WDPA_Feb2018_marine-shapefile-polygons")

setwd("~/Documents/Postdoc MARBEC/ESTHETIC/TargetAchievement")
fish_esth<-read.csv2("acc_syn_worms.csv")

#Load data from Camille Albouy paper.
load("MatPa_Final_ok.rdata")

# ------------------------------------------------------------------------------------------------ Species in Albouy et al. 
allnames_esth <- unique(c(as.character(fish_esth$acc_sci_name),as.character(fish_esth$name)))

fish_select <- cbind(MatPa_Final_ok$Longitude,MatPa_Final_ok$Latitude,
                     MatPa_Final_ok[,colnames(MatPa_Final_ok) %in% allnames_esth])
colnames(fish_select)[c(1,2)]<-c("long","lat")

#keep only "real" protected Areas
mapMPA_SPA <- subset(mapMPA,IUCN_CAT == "Ia" | IUCN_CAT == "Ib" | IUCN_CAT == "II")
#mapMPA_SPA<-spTransform(mapMPA_SPA,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
#mapMPA_SPA <- gBuffer(mapMPA_SPA, byid=TRUE, width=0)
mapMPA_SPA <- st_transform(mapMPA_SPA, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
mapMPA_SPA <- st_buffer(mapMPA_SPA, 0)

coverMPA <- pbmclapply(3:ncol(fish_select), function(i)  {  

        r <- rasterFromXYZ(as.data.frame(fish_select[,c(1,2,i)])[, c("long", "lat", colnames(fish_select)[i])],crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
        pol <- rasterToPolygons(r, fun=function(x){x==1})
        pol <- st_as_sf(pol)
        pol<-st_transform(pol, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

        distrib <- sum(st_area(pol))
        protected <- sum(st_area(st_intersection(pol,mapMPA_SPA))) 
        
        
        #distrib <- sum(area(pol))
        #protected <- sum(area(intersect(pol,mapMPA_SPA))) 

        res <- data.frame(distrib,protected)
        
},mc.cores=3)

coverMPA <- do.call(rbind,coverMPA)


## FUNCTION TO COMPUTE THE CONSERVATION TARGET 
target_func <- function(SR, qt, log=TRUE){
  SR_i <- SR
  qt_i <- qt
  
  if(log) {
    SR <- log(SR)
    qt <- log(qt)
  }	
  dat <- data.frame(matrix(c(100,10,qt), ncol=2, dimnames=list(NULL, c("Target", "SR"))))
  lm_target <- lm(Target~SR, data=dat)
  Tar <- predict(lm_target,newdata=as.data.frame(SR))
  Tar[SR_i<=qt_i[1]] <- 100
  Tar[SR_i>=qt_i[2]] <- 10
  return(Tar)
}