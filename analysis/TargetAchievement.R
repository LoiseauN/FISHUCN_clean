
library(maptools)
library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(sf)


load("~/Documents/Postdoc MARBEC/FISHUCN/last/FISHUCN_clean/data/PctMPAI_IV.RData")

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
PctMPAI_IV$perc_cover <- as.numeric(as.character(PctMPAI_IV$perc_cover))

# -------------------------------------------------------------------------------------------- Compute target achievement
# Correct target with the range of species following  Jones et al. 2020
#  < 10,000 km2, 100% coverage 
#  > 390,000 km2 the target was reduced to 10% coverage,
#coverMPA <- do.call(rbind,coverMPA_SPA)
#rownames(coverMPA)<-colnames(fish_select)[-c(1,2)]
#transmort in Km 2 
#TO CHECK
MPA_Protect <- FishDistribArea_all[FishDistribArea_all$species %in% data_zonation$species,]

#MPA_Protect$DistrArea <- FishDistribArea_all$DistrArea/1e+6
MPA_Protect <- merge(MPA_Protect,PctMPAI_IV,by="species")


MPA_Protect$TargetExp <- NA
for (i in 1 : nrow(MPA_Protect)) {   
  print(i)
  if(is.na(MPA_Protect$DistrArea[i]))  {  MPA_Protect$TargetExp[i]  <- NA
  }  else if(MPA_Protect$DistrArea[i]< 1e+05)  { MPA_Protect$TargetExp[i]  <- 100
  }  else if(MPA_Protect$DistrArea[i]> 39e+05) { MPA_Protect$TargetExp[i]  <- 10  
  }  else {  MPA_Protect$TargetExp[i]  <- NA}
}


qt=quantile(MPA_Protect[is.na(MPA_Protect$TargetExp),]$DistrArea, probs=c(0.1, 0.9),na.rm=T)


for(i in 1:nrow(MPA_Protect)) {
  if(is.na(MPA_Protect$TargetExp[i])) {  MPA_Protect$TargetExp[i] <- round(target_func(MPA_Protect[i,"DistrArea"], qt, log=T),3) }
}

MPA_Protect[,"Target_achievement_I_IV"] <- round(100*(MPA_Protect[,"Percentage_MPAI_IV"]/MPA_Protect[,"TargetExp"]),3)








setwd("~/Documents/Postdoc MARBEC/FISHUCN/last/FISHUCN_clean/data")
#mapMPA<-readOGR(dsn=".",layer="WDPA_Feb2018_marine-shapefile-polygons")
mapMPA<-st_read(dsn=".",layer="WDPA_Feb2018_marine-shapefile-polygons")

setwd("~/Documents/Postdoc MARBEC/ESTHETIC/TargetAchievement")
fish_esth<-read.csv2("acc_syn_worms.csv")

#Load data from Camille Albouy paper.
load("MatPa_Final_ok.rdata")







datanico_MPA$TargetExp <- NA
for (i in 1 : nrow(datanico_MPA)) {   
  print(i)
  if(is.na(datanico_MPA$DistrArea[i]))  {  datanico_MPA$TargetExp[i]  <- NA
  }  else if(datanico_MPA$DistrArea[i]< 1e+05)  { datanico_MPA$TargetExp[i]  <- 100
  }  else if(datanico_MPA$DistrArea[i]> 39e+05) { datanico_MPA$TargetExp[i]  <- 10  
  }  else {  datanico_MPA$TargetExp[i]  <- NA}
}


qt=quantile(datanico_MPA[is.na(datanico_MPA$TargetExp),]$DistrArea, probs=c(0.1, 0.9),na.rm=T)


for(i in 1:nrow(datanico_MPA)) {
  if(is.na(datanico_MPA$TargetExp[i])) {  datanico_MPA$TargetExp[i] <- round(target_func(datanico_MPA[i,"DistrArea"], qt, log=T),3) }
}

datanico_MPA[,"Target_achievement_I_IV"] <- round(100*(datanico_MPA[,"Percentage_MPAI_IV"]/datanico_MPA[,"TargetExp"]),3)
datanico_MPA[,"Target_achievement_I_II"] <- round(100*(datanico_MPA[,"Percentage_MPAI_II"]/datanico_MPA[,"TargetExp"]),3)



ggplot(datanico_MPA, aes(x= iucn_code, y = Percentage_MPAI_II))+ geom_boxplot()
ggplot(datanico_MPA, aes(x= iucn_code, y = Percentage_MPAI_IV))+ geom_boxplot()
ggplot(datanico_MPA, aes(x= iucn_code, y = Target_achievement_I_IV))+ geom_boxplot()
ggplot(datanico_MPA, aes(x= iucn_code, y = Target_achievement_I_II))+ geom_boxplot()

datanico_MPA <- merge(datanico_MPA,all_predict,by.x ="acc_sci_name",by.y = "species",all.x=T)
datanico_MPA <-datanico_MPA[,c(1:8,15,16)]

save(datanico_MPA,file="datanico_MPA.RData")






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


